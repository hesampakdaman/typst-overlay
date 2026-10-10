;;; typst-overlay.el --- Overlay Typst equations -*- lexical-binding: t; -*-

;; Author: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Maintainer: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/hesampakdaman/typst-overlay
;; Assisted-by: Claude:claude-sonnet-4-6

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; typst-overlay renders Typst math equations as inline overlays.
;; Equations are compiled asynchronously to SVG with the `typst'
;; compiler and displayed over the source text.  The overlay is hidden
;; while point is inside an equation, so you can edit the raw source,
;; and restored when point leaves.
;;
;; Supported buffers: `typst-ts-mode' and `org-mode' (where $...$ is
;; treated as Typst math).  Both use the Typst tree-sitter grammar to
;; find equations.
;;
;; Usage:
;;
;;   M-x typst-overlay-mode      enable in the current buffer
;;   M-x typst-overlay-refresh   re-render overlays manually
;;
;; To refresh on save, add `typst-overlay-save-refresh' to
;; `after-save-hook'.
;;
;; Requires the `typst' executable on PATH and the Typst tree-sitter
;; grammar.  See `customize-group' `typst-overlay' for options.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'treesit)

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-type "org-element-ast" (node &optional anonymous))

(defvar typst-overlay-mode)

;; customization
(defgroup typst-overlay nil
  "Render Typst math equations as overlays."
  :group 'tools
  :prefix "typst-overlay-")

(defcustom typst-overlay-scale 1.3
  "Scale factor for rendered equations."
  :type 'number
  :group 'typst-overlay)

(defcustom typst-overlay-cache-dir-name ".typst-overlay-cache"
  "Name of the directory used to store rendered SVG files."
  :type 'string
  :group 'typst-overlay)

(defcustom typst-overlay-max-active-compiles 8
  "Maximum number of concurrent Typst compilation processes."
  :type 'natnum
  :group 'typst-overlay)

(defcustom typst-overlay-extra-prelude ""
  "Extra Typst code prepended to every rendered overlay element."
  :type 'string
  :group 'typst-overlay)

(defface typst-overlay-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t :inherit error))
  "Face for equations that failed to compile."
  :group 'typst-overlay)

;; constants / buffer-local state
(defconst typst-overlay--grammar-url "https://github.com/uben0/tree-sitter-typst"
  "Source of the Typst tree-sitter grammar.")

(defvar-local typst-overlay--snapshot nil)
(defvar-local typst-overlay--active-overlay nil)
(defvar-local typst-overlay--shown-error-overlay nil
  "Error overlay whose message was last shown, to show it only once.")
(defvar-local typst-overlay--registry nil)
(defvar-local typst-overlay--artifact-cache nil)
(defvar-local typst-overlay--compile-queue nil)
(defvar-local typst-overlay--active-compiles 0)
(defvar-local typst-overlay--analyzer #'typst-overlay--analyze-typst
  "Function to analyze the current buffer.
Should return a `typst-overlay-analysis' struct.")

;; analyzer
(cl-defstruct typst-overlay-code-node
  beg
  end
  text
  hash)

(defun typst-overlay--make-code-node (node)
  "Return a `typst-overlay-code-node' for the tree-sitter NODE."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (text (buffer-substring-no-properties beg end)))
    (make-typst-overlay-code-node
     :beg beg
     :end end
     :text text
     :hash (md5 text))))

(defun typst-overlay--collect-code-nodes ()
  "Collect outermost code nodes in document order.

We query all `code` nodes via tree-sitter, but this includes nested
code blocks.  We only want top-level (outermost) ones, since nested
code should not be treated as independent top-level code nodes.

To enforce this, for each matched node we walk up its parent chain.
If we encounter another `code` node, this node is nested and skipped.

Remaining nodes are converted to `typst-overlay-code-node's and
returned in document order."
  (let* ((root (treesit-buffer-root-node))
         (query '((code) @code))
         (captures (treesit-query-capture root query))
         result)
    (dolist (cap captures)
      (let* ((node (cdr cap))
             (parent (treesit-node-parent node))
             (inside-code nil))
        ;; Walk up the parent chain to detect whether this `code` node
        ;; is nested inside another `code` node. If so, skip it.
        (while (and parent (not inside-code))
          (when (string= (treesit-node-type parent) "code")
            (setq inside-code t))
          (setq parent (treesit-node-parent parent)))
        (unless inside-code
          (push (typst-overlay--make-code-node node) result))))
    (nreverse result)))

(defun typst-overlay--sort-code-nodes (nodes)
  "Sort code NODES by buffer position."
  (sort nodes
        (lambda (a b)
          (< (typst-overlay-code-node-beg a)
             (typst-overlay-code-node-beg b)))))

(cl-defstruct typst-overlay-math-node
  beg
  end
  text
  text-hash)

(defun typst-overlay--make-math-node (node)
  "Return a `typst-overlay-math-node' for the tree-sitter NODE."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (text (buffer-substring-no-properties beg end)))
    (make-typst-overlay-math-node
     :beg beg
     :end end
     :text text
     :text-hash (md5 text))))

(defun typst-overlay--collect-math-nodes (root first-error)
  "Collect math nodes that should be rendered (i.e. in content, not code).

ROOT is the tree-sitter root node.  Math at or after FIRST-ERROR is skipped.

We query all `math` nodes via tree-sitter.  However, math can appear
both in normal document content and inside `code` blocks.  We only want
math that is part of rendered content.

For each math node, we walk up its parent chain:

- If we encounter a `content` node first, we include it.
- If we encounter a `code` node first, we exclude it.
- If neither is found (unexpected/edge case), we include it by default.

This effectively selects math expressions that belong to the document
body rather than programmatic code.
Math nodes that appear after a parse error in the document are excluded."
  (let* ((query '((math) @math))
         (captures (treesit-query-capture root query))
         result)
    (dolist (cap captures)
      (let* ((node (cdr cap))
             (node-beg (treesit-node-start node))
             (parent (treesit-node-parent node))
             (include nil)
             (done nil))
        (when (or (null first-error) (< node-beg first-error))
          ;; Walk up to decide whether this math node lives in `content`
          ;; (include) or inside `code` (exclude).
          (while (and parent (not done))
            (pcase (treesit-node-type parent)
              ("content"
               (setq include t)
               (setq done t))
              ("code"
               (setq include nil)
               (setq done t)))
            (setq parent (treesit-node-parent parent)))
          (when (or include (not done))
            (push (typst-overlay--make-math-node node) result)))))
    (nreverse result)))

(defun typst-overlay--sort-math-nodes (nodes)
  "Sort math NODES by buffer position."
  (sort nodes
        (lambda (a b)
          (< (typst-overlay-math-node-beg a)
             (typst-overlay-math-node-beg b)))))

(cl-defstruct typst-overlay-analysis
  code-nodes
  math-nodes
  first-error)

(defun typst-overlay--analyze-typst ()
  "Analyze the current Typst buffer using tree-sitter.
Return a `typst-overlay-analysis' with its code nodes, math nodes
and the position of the first parse error, if any."
  (let* ((root (treesit-buffer-root-node))
         (error-captures (treesit-query-capture root '((ERROR) @error)))
         (first-error (and error-captures
                           (apply #'min
                                  (mapcar (lambda (cap)
                                            (treesit-node-start (cdr cap)))
                                          error-captures)))))
    (make-typst-overlay-analysis
     :code-nodes (typst-overlay--sort-code-nodes
                  (typst-overlay--collect-code-nodes))
     :math-nodes (typst-overlay--sort-math-nodes
                  (typst-overlay--collect-math-nodes root first-error))
     :first-error first-error)))

(defconst typst-overlay--org-code-types
  '(src-block example-block export-block fixed-width comment comment-block
    code verbatim inline-src-block keyword)
  "Org element types whose contents are never treated as math.")

(defun typst-overlay--escaped-p (pos)
  "Return non-nil if the character at POS is escaped by a backslash.
A character is escaped when preceded by an odd number of backslashes."
  (save-excursion
    (goto-char pos)
    (cl-oddp (- pos (progn (skip-chars-backward "\\\\") (point))))))

(defun typst-overlay--org-paragraph-end ()
  "Return the end of the paragraph at point.
The paragraph ends at the next blank line or heading."
  (save-excursion
    (if (re-search-forward "\n[ \t]*\n\\|\n\\*+ " nil t)
        (match-beginning 0)
      (point-max))))

(defvar typst-overlay--org-parse-buffer nil
  "Hidden buffer reused to parse candidate equations in org buffers.
Reusing one buffer and parser is several times faster than
`treesit-parse-string', which creates both on every call.")

(defun typst-overlay--org-parse-buffer ()
  "Return the hidden Typst parse buffer, creating it if needed."
  (unless (buffer-live-p typst-overlay--org-parse-buffer)
    (setq typst-overlay--org-parse-buffer
          (generate-new-buffer " *typst-overlay-parse*" t))
    (with-current-buffer typst-overlay--org-parse-buffer
      (buffer-disable-undo)
      (treesit-parser-create 'typst)))
  typst-overlay--org-parse-buffer)

(defun typst-overlay--org-math-end (beg limit)
  "Return the end of the Typst math starting at BEG, or nil.
The text from BEG to LIMIT is parsed with the Typst tree-sitter
grammar, so nested math such as $#table($x$)$ is handled.  Return
nil if no math starts at BEG or it does not parse cleanly."
  (let ((text (buffer-substring-no-properties beg limit)))
    (with-current-buffer (typst-overlay--org-parse-buffer)
      (erase-buffer)
      (insert text)
      (let ((node (treesit-node-descendant-for-range
                   (treesit-buffer-root-node 'typst) 1 2)))
        (while (and node (not (equal (treesit-node-type node) "math")))
          (setq node (treesit-node-parent node)))
        (when (and node
                   (= (treesit-node-start node) 1)
                   (not (treesit-node-check node 'has-error)))
          (+ beg (1- (treesit-node-end node))))))))

(defun typst-overlay--org-in-code-p (pos)
  "Return non-nil if POS is inside an org code or verbatim element."
  (save-excursion
    (goto-char pos)
    (memq (org-element-type (org-element-context))
          typst-overlay--org-code-types)))

(defun typst-overlay--analyze-org ()
  "Collect $...$ spans in an org buffer as Typst math.

Follows Typst's delimiter rules rather than org's LaTeX ones: each
unescaped dollar sign may start Typst math, and where it ends is
decided by the Typst tree-sitter grammar.  So math may be written
with or without surrounding whitespace (Typst decides inline vs.
display from it) and may nest, as in $#table($x$)$.  Write \\$ for a
literal dollar sign.

Math may cross lines but not the end of a paragraph (a blank line
or heading), so a stray dollar sign cannot swallow the rest of the
buffer.  Spans in code, verbatim and src blocks are skipped, as are
empty spans and spans whose closing $ is followed by a digit, so
that prose like \"$5 and $10\" is left alone."
  (let (math-nodes)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "$" nil t)
        (let* ((beg (1- (point)))
               (end (and (not (typst-overlay--escaped-p beg))
                         (typst-overlay--org-math-end
                          beg (typst-overlay--org-paragraph-end)))))
          (if (and end
                   (> (- end beg) 2)
                   (not (memq (char-after end)
                              '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
                   (not (typst-overlay--org-in-code-p beg)))
              (let ((text (buffer-substring-no-properties beg end)))
                (push (make-typst-overlay-math-node
                       :beg beg
                       :end end
                       :text text
                       :text-hash (md5 text))
                      math-nodes)
                (goto-char end))
            ;; Not math: treat the opening $ as literal and move on.
            (goto-char (1+ beg))))))
    (make-typst-overlay-analysis
     :code-nodes nil
     :math-nodes (nreverse math-nodes)
     :first-error nil)))

;; snapshot
(cl-defstruct typst-overlay-element
  beg
  end
  text
  text-hash
  prelude-text
  prelude-hash
  cache-key)

(defun typst-overlay--make-element (math-node prelude-code-nodes)
  "Build an element from MATH-NODE and PRELUDE-CODE-NODES.
The cache key also covers `typst-overlay-extra-prelude'."
  (let* ((text-hash (typst-overlay-math-node-text-hash math-node))
         (prelude-text (typst-overlay--build-prelude-text prelude-code-nodes))
         (prelude-hash (md5 prelude-text))
         (cache-key (md5 (concat text-hash prelude-hash
                                 (md5 typst-overlay-extra-prelude)))))
    (make-typst-overlay-element
     :beg (typst-overlay-math-node-beg math-node)
     :end (typst-overlay-math-node-end math-node)
     :text (typst-overlay-math-node-text math-node)
     :text-hash text-hash
     :prelude-text prelude-text
     :prelude-hash prelude-hash
     :cache-key cache-key)))

(defun typst-overlay--build-prelude-text (code-nodes)
  "Return the full prelude source for CODE-NODES."
  (mapconcat #'typst-overlay-code-node-text code-nodes "\n\n"))

(cl-defstruct typst-overlay-snapshot
  version
  elements       ;; ordered list of typst-overlay-element
  code-nodes
  math-nodes)

(defun typst-overlay--collect-prelude-code-nodes (math-node code-nodes)
  "Return prelude CODE-NODES that occur before MATH-NODE.

A code node contributes to the prelude if:
- it appears before the math node in the buffer, and
- it satisfies `typst-overlay--prelude-code-node-p'

The result is returned in document order."
  (let ((math-beg (typst-overlay-math-node-beg math-node))
        result)
    (dolist (code-node code-nodes)
      (when (and (< (typst-overlay-code-node-beg code-node) math-beg)
                 (typst-overlay--prelude-code-node-p code-node))
        (push code-node result)))
    (nreverse result)))

(defun typst-overlay--prelude-code-node-p (code-node)
  "Return non-nil if CODE-NODE is a #let, #import or #include."
  (let ((text (string-trim-left (typst-overlay-code-node-text code-node))))
    (or (string-prefix-p "#let" text)
        (string-prefix-p "#import" text)
        (string-prefix-p "#include" text))))

(defun typst-overlay--make-snapshot (analysis)
  "Return a `typst-overlay-snapshot' built from ANALYSIS.
Each math node becomes an element paired with its prelude code."
  (let* ((code-nodes (typst-overlay-analysis-code-nodes analysis))
         (math-nodes (typst-overlay-analysis-math-nodes analysis))
         elements)
    (dolist (math-node math-nodes)
      (let* ((prelude-nodes
              (typst-overlay--collect-prelude-code-nodes math-node code-nodes))
             (element
              (typst-overlay--make-element
               math-node prelude-nodes)))
        (push element elements)))
    (make-typst-overlay-snapshot
     :version (float-time)
     :elements (nreverse elements)
     :code-nodes code-nodes
     :math-nodes math-nodes)))

;; differ
(cl-defstruct typst-overlay-diff-entry
  status   ;; 'unchanged 'moved 'added
  old      ;; old typst-overlay-element or nil
  new)     ;; new typst-overlay-element

(cl-defstruct typst-overlay-diff
  entries   ;; ordered by new snapshot
  deleted)  ;; list of old typst-overlay-element

(defun typst-overlay--same-element-position-p (old-element new-element)
  "Return non-nil if OLD-ELEMENT and NEW-ELEMENT span the same region."
  (and (= (typst-overlay-element-beg old-element)
          (typst-overlay-element-beg new-element))
       (= (typst-overlay-element-end old-element)
          (typst-overlay-element-end new-element))))

(defun typst-overlay--build-element-queues (elements)
  "Return hash table mapping cache-key to list of ELEMENTS in order."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (element elements)
      (let* ((cache-key (typst-overlay-element-cache-key element))
             (queue (gethash cache-key table)))
        (puthash cache-key
                 (cons element queue)
                 table)))
    (maphash (lambda (key queue)
               (puthash key (nreverse queue) table))
             table)
    table))

(defun typst-overlay--find-old-match (new-element old-queues)
  "Return first unmatched old element for NEW-ELEMENT in OLD-QUEUES, or nil."
  (let* ((cache-key (typst-overlay-element-cache-key new-element))
         (queue (gethash cache-key old-queues)))
    (car queue)))

(defun typst-overlay--consume-old-match (element old-queues)
  "Consume one unmatched old element with ELEMENT's cache-key from OLD-QUEUES."
  (let* ((cache-key (typst-overlay-element-cache-key element))
         (queue (gethash cache-key old-queues)))
    ;; Drop the first unmatched old element from the queue.
    (puthash cache-key (cdr queue) old-queues)))

(defun typst-overlay--make-added-entry (new-element)
  "Return a diff entry marking NEW-ELEMENT as added."
  (make-typst-overlay-diff-entry
   :status 'added
   :old nil
   :new new-element))

(defun typst-overlay--make-matched-entry (old-element new-element)
  "Return a diff entry matching OLD-ELEMENT to NEW-ELEMENT.
The status is `unchanged' if both span the same region, else `moved'."
  (let ((status (if (typst-overlay--same-element-position-p
                     old-element new-element)
                    'unchanged
                  'moved)))
    (make-typst-overlay-diff-entry
     :status status
     :old old-element
     :new new-element)))

(defun typst-overlay--diff-snapshots (old-snapshot new-snapshot)
  "Compute diff from OLD-SNAPSHOT to NEW-SNAPSHOT.

Elements are matched by `cache-key` (render identity).  Because
multiple elements may share the same cache-key, OLD elements are
grouped into queues (one queue per cache-key).

We then walk NEW elements in document order and try to consume
one matching OLD element from the corresponding queue:

- If a match is found:
  - same position → unchanged
  - different position → moved
  The matched OLD element is removed from the queue.

- If no match is found:
  → added

After processing all NEW elements, any OLD elements left in the
queues were not matched and are therefore deleted.

The resulting diff contains:
- `entries`: ordered like NEW-SNAPSHOT
- `deleted`: remaining OLD elements"
  (let* ((old-elements (typst-overlay-snapshot-elements old-snapshot))
         (new-elements (typst-overlay-snapshot-elements new-snapshot))
         (old-queues (typst-overlay--build-element-queues old-elements))
         entries
         deleted)
    (dolist (new-element new-elements)
      (let ((old-element
             (typst-overlay--find-old-match new-element old-queues)))
        (cond
         (old-element
          (typst-overlay--consume-old-match new-element old-queues)
          (push (typst-overlay--make-matched-entry old-element new-element)
                entries))
         (t
          (push (typst-overlay--make-added-entry new-element)
                entries)))))
    (maphash
     (lambda (_cache-key queue)
       (setq deleted (nconc (nreverse queue) deleted)))
     old-queues)
    (make-typst-overlay-diff
     :entries (nreverse entries)
     :deleted (nreverse deleted))))

;; planer
(cl-defstruct typst-overlay-artifact
  cache-key    ;; string: render identity
  svg-path)    ;; string: path to rendered svg

(cl-defstruct typst-overlay-record
  element      ;; typst-overlay-element (current occurrence)
  state        ;; 'rendering 'visible 'stale 'failed
  overlay      ;; Emacs overlay or nil
  artifact     ;; typst-overlay-artifact or nil
  generation)  ;; integer: guards async staleness

(cl-defstruct typst-overlay-registry
  records      ;; hash table: occurrence-key -> typst-overlay-record
  generation)  ;; latest generation applied

(defun typst-overlay--make-registry ()
  "Return an empty `typst-overlay-registry'."
  (make-typst-overlay-registry
   :records (make-hash-table :test #'equal)
   :generation 0))

(defun typst-overlay--make-artifact-cache ()
  "Return an empty artifact cache."
  (make-hash-table :test #'equal))

(defun typst-overlay--load-artifact-cache ()
  "Populate artifact cache from SVG files already on disk."
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file)))
         (cache-dir (and dir (expand-file-name typst-overlay-cache-dir-name dir))))
    (when (and cache-dir (file-directory-p cache-dir))
      (dolist (svg-path (directory-files cache-dir t "\\.svg\\'"))
        (let ((cache-key (file-name-base svg-path)))
          (puthash cache-key
                   (make-typst-overlay-artifact
                    :cache-key cache-key
                    :svg-path svg-path)
                   typst-overlay--artifact-cache))))))

(defun typst-overlay--ensure-runtime ()
  "Create the registry and artifact cache if they do not exist yet."
  (unless typst-overlay--registry
    (setq typst-overlay--registry
          (typst-overlay--make-registry)))
  (unless typst-overlay--artifact-cache
    (setq typst-overlay--artifact-cache
          (typst-overlay--make-artifact-cache))
    (typst-overlay--load-artifact-cache)))

(defun typst-overlay--occurrence-key (element)
  "Return the registry key for ELEMENT, its (BEG . END) region."
  (cons (typst-overlay-element-beg element)
        (typst-overlay-element-end element)))

(cl-defstruct typst-overlay-delete-op
  old)

(cl-defstruct typst-overlay-place-op
  old       ;; old element or nil
  new       ;; new element
  artifact) ;; artifact to place

(cl-defstruct typst-overlay-render-op
  old       ;; old element or nil
  new)      ;; new element

(cl-defstruct typst-overlay-render-plan
  generation
  delete
  place
  render)

(defun typst-overlay--plan-render (diff registry artifact-cache generation)
  "Build a renderer-facing plan from DIFF and current runtime state.
REGISTRY stores per-occurrence runtime records.
ARTIFACT-CACHE is a hash table mapping cache-key to typst-overlay-artifact.
GENERATION is the generation to stamp onto the returned plan."
  (make-typst-overlay-render-plan
   :generation generation
   :delete (typst-overlay--plan-delete-ops diff)
   :place (typst-overlay--plan-place-ops diff registry artifact-cache)
   :render (typst-overlay--plan-render-ops diff registry artifact-cache)))

(defun typst-overlay--plan-delete-ops (diff)
  "Build delete ops for all old elements deleted by DIFF."
  (mapcar #'typst-overlay--make-delete-op
          (typst-overlay-diff-deleted diff)))

(defun typst-overlay--make-delete-op (old-element)
  "Build a delete op for OLD-ELEMENT."
  (make-typst-overlay-delete-op
   :old old-element))

(defun typst-overlay--plan-place-ops (diff registry artifact-cache)
  "Build place ops from DIFF, REGISTRY and ARTIFACT-CACHE.
Covers entries that can reuse an existing artifact.

Cases:
- moved + old record has artifact            -> place-op
- added + artifact cache hit                 -> place-op
- unchanged + artifact exists but no overlay -> place-op"
  (let (ops)
    (dolist (entry (typst-overlay-diff-entries diff))
      (pcase (typst-overlay-diff-entry-status entry)
        ('unchanged
         (let* ((old-element (typst-overlay-diff-entry-old entry))
                (new-element (typst-overlay-diff-entry-new entry))
                (record (typst-overlay--get-record registry old-element))
                (artifact (and record (typst-overlay-record-artifact record)))
                (overlay (and record (typst-overlay-record-overlay record))))
           (when (and artifact (null overlay))
             (push (make-typst-overlay-place-op
                    :old old-element
                    :new new-element
                    :artifact artifact)
                   ops))))

        ('moved
         (let* ((old-element (typst-overlay-diff-entry-old entry))
                (new-element (typst-overlay-diff-entry-new entry))
                (record (typst-overlay--get-record registry old-element))
                (artifact (and record
                               (typst-overlay-record-artifact record))))
           (when artifact
             (push (make-typst-overlay-place-op
                    :old old-element
                    :new new-element
                    :artifact artifact)
                   ops))))

        ('added
         (let* ((new-element (typst-overlay-diff-entry-new entry))
                (cache-key (typst-overlay-element-cache-key new-element))
                (artifact (gethash cache-key artifact-cache)))
           (when artifact
             (push (make-typst-overlay-place-op
                    :old nil
                    :new new-element
                    :artifact artifact)
                   ops))))))
    (nreverse ops)))

(defun typst-overlay--plan-render-ops (diff registry artifact-cache)
  "Build render ops from DIFF, REGISTRY and ARTIFACT-CACHE.
Covers entries that do not have a reusable artifact.

Cases:
- moved + old record has no artifact      -> render-op
- added + artifact cache miss             -> render-op
- unchanged + stale record has no artifact -> render-op"
  (let (ops)
    (dolist (entry (typst-overlay-diff-entries diff))
      (pcase (typst-overlay-diff-entry-status entry)
        ('unchanged
         (let* ((old-element (typst-overlay-diff-entry-old entry))
                (new-element (typst-overlay-diff-entry-new entry))
                (record (typst-overlay--get-record registry old-element)))
           (when (and record
                      (eq (typst-overlay-record-state record) 'stale)
                      (null (typst-overlay-record-artifact record)))
             (push (make-typst-overlay-render-op
                    :old old-element
                    :new new-element)
                   ops))))

        ('moved
         (let* ((old-element (typst-overlay-diff-entry-old entry))
                (new-element (typst-overlay-diff-entry-new entry))
                (record (typst-overlay--get-record registry old-element))
                (artifact (and record
                               (typst-overlay-record-artifact record))))
           (unless artifact
             (push (make-typst-overlay-render-op
                    :old old-element
                    :new new-element)
                   ops))))

        ('added
         (let* ((new-element (typst-overlay-diff-entry-new entry))
                (cache-key (typst-overlay-element-cache-key new-element))
                (artifact (gethash cache-key artifact-cache)))
           (unless artifact
             (push (make-typst-overlay-render-op
                    :old nil
                    :new new-element)
                   ops))))))

    (nreverse ops)))

(defun typst-overlay--get-record (registry element)
  "Return the record for ELEMENT in REGISTRY, or nil if none exists."
  (gethash (typst-overlay--occurrence-key element)
           (typst-overlay-registry-records registry)))

;; render
(defun typst-overlay--apply-render-plan (plan registry artifact-cache)
  "Apply PLAN by mutating REGISTRY and runtime state.
ARTIFACT-CACHE is passed on when starting renders."
  (let ((generation (typst-overlay-render-plan-generation plan)))
    (dolist (op (typst-overlay-render-plan-delete plan))
      (typst-overlay--apply-delete-op op registry))
    (dolist (op (typst-overlay-render-plan-place plan))
      (typst-overlay--apply-place-op op registry generation))
    (dolist (op (typst-overlay-render-plan-render plan))
      (typst-overlay--apply-render-op op registry generation artifact-cache))
    (setf (typst-overlay-registry-generation registry) generation)))

(defun typst-overlay--apply-delete-op (op registry)
  "Apply delete OP by removing overlay and record from REGISTRY."
  (let* ((old-element (typst-overlay-delete-op-old op))
         (record (typst-overlay--get-record registry old-element)))
    (when record
      (typst-overlay--delete-record-overlay record)
      (typst-overlay--remove-record registry old-element))))

(defun typst-overlay--apply-place-op (op registry generation)
  "Apply place OP by making its new element visible in REGISTRY.
GENERATION stamps the record so stale async results can be ignored."
  (let* ((old-element (typst-overlay-place-op-old op))
         (new-element (typst-overlay-place-op-new op))
         (artifact (typst-overlay-place-op-artifact op))
         (record (and old-element
                      (typst-overlay--get-record registry old-element))))
    (when old-element
      (when record
        (typst-overlay--delete-record-overlay record))
      (typst-overlay--remove-record registry old-element))
    (unless record
      (setq record (make-typst-overlay-record)))
    (setf (typst-overlay-record-element record) new-element
          (typst-overlay-record-state record) 'visible
          (typst-overlay-record-artifact record) artifact
          (typst-overlay-record-generation record) generation
          (typst-overlay-record-overlay record)
          (typst-overlay--place-artifact-overlay new-element artifact))
    (typst-overlay--put-record registry new-element record)))

(defun typst-overlay--apply-render-op (op registry generation artifact-cache)
  "Apply render OP by registering its new element in REGISTRY.
Start an async render stamped with GENERATION.  ARTIFACT-CACHE receives
the result."
  (let* ((old-element (typst-overlay-render-op-old op))
         (new-element (typst-overlay-render-op-new op))
         (record (and old-element
                      (typst-overlay--get-record registry old-element))))
    (when old-element
      (when record
        (typst-overlay--delete-record-overlay record))
      (typst-overlay--remove-record registry old-element))
    (unless record
      (setq record (make-typst-overlay-record)))
    (setf (typst-overlay-record-element record) new-element
          (typst-overlay-record-state record) 'rendering
          (typst-overlay-record-overlay record) nil
          (typst-overlay-record-artifact record) nil
          (typst-overlay-record-generation record) generation)
    (typst-overlay--put-record registry new-element record)
    (typst-overlay--start-render-for-element new-element generation artifact-cache)))

(defun typst-overlay--put-record (registry element record)
  "Store RECORD for ELEMENT occurrence key in REGISTRY."
  (puthash (typst-overlay--occurrence-key element)
           record
           (typst-overlay-registry-records registry)))

(defun typst-overlay--remove-record (registry element)
  "Remove ELEMENT occurrence key entry from REGISTRY."
  (remhash (typst-overlay--occurrence-key element)
           (typst-overlay-registry-records registry)))

(defun typst-overlay--delete-record-overlay (record)
  "Delete RECORD live overlay, if any, and clear the slot."
  (let ((overlay (typst-overlay-record-overlay record)))
    (when overlay
      (delete-overlay overlay)
      (setf (typst-overlay-record-overlay record) nil))))

(defun typst-overlay--place-artifact-overlay (element artifact)
  "Place ARTIFACT for ELEMENT and return the created overlay."
  (typst-overlay--place-overlay-from-svg
   (typst-overlay-element-beg element)
   (typst-overlay-element-end element)
   (typst-overlay-artifact-svg-path artifact)))

(defun typst-overlay--overlay-at-point ()
  "Return the Typst overlay at point, or nil."
  (seq-find
   (lambda (ov)
     (overlay-get ov 'typst-overlay))
   (overlays-at (point))))

(defun typst-overlay--hide-overlay (overlay)
  "Hide OVERLAY by clearing its display property."
  (when (overlayp overlay)
    (overlay-put overlay 'display nil)))

(defun typst-overlay--show-overlay (overlay)
  "Show OVERLAY again using its cached image."
  (when (overlayp overlay)
    (let ((image (overlay-get overlay 'typst-overlay-image)))
      (when image
        (overlay-put overlay 'display image)))))

(defun typst-overlay--echo-error-at-point ()
  "Show the error of a failed equation when point enters it.
The message is shown once per entry, not after every command."
  (let ((overlay (seq-find (lambda (ov) (overlay-get ov 'typst-overlay-error))
                           (overlays-at (point)))))
    (unless (eq overlay typst-overlay--shown-error-overlay)
      (setq typst-overlay--shown-error-overlay overlay)
      (when overlay
        (message "Typst: %s" (overlay-get overlay 'typst-overlay-error))))))

(defun typst-overlay--post-command-update ()
  "Hide overlay under point and restore the previously active one."
  (typst-overlay--handle-upward-entry)
  (typst-overlay--echo-error-at-point)
  (let ((current (typst-overlay--overlay-at-point))
        (active typst-overlay--active-overlay))
    (unless (eq current active)
      (when (overlayp active)
        (typst-overlay--show-overlay active))
      (when (overlayp current)
        (typst-overlay--hide-overlay current))
      (setq typst-overlay--active-overlay current))))

(defun typst-overlay--invalidate-overlay (overlay &rest _args)
  "Delete OVERLAY if its underlying text is modified."
  (when (overlayp overlay)
    (when (eq overlay typst-overlay--active-overlay)
      (setq typst-overlay--active-overlay nil))
    (delete-overlay overlay)
    (when typst-overlay--registry
      (maphash
       (lambda (_key record)
         (when (eq (typst-overlay-record-overlay record) overlay)
           (setf (typst-overlay-record-overlay record) nil
                 (typst-overlay-record-state record) 'stale)))
       (typst-overlay-registry-records typst-overlay--registry)))))

(defun typst-overlay--recolor-svg (svg-path)
  "Read SVG-PATH and replace black with the current foreground color."
  (let ((fg (typst-overlay--foreground-color))
        (contents (with-temp-buffer
                    (insert-file-contents svg-path)
                    (buffer-string))))
    (replace-regexp-in-string
     (regexp-quote "#000000") fg contents t t)))

(defun typst-overlay--place-overlay-from-svg (beg end svg-path)
  "Create and return an overlay from BEG to END displaying SVG-PATH."
  (let* ((svg-data (typst-overlay--recolor-svg svg-path))
         (image (create-image svg-data 'svg t :ascent 'center :scale typst-overlay-scale))
         (overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'display image)
    (overlay-put overlay 'typst-overlay t)
    (overlay-put overlay 'typst-overlay-image image)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks '(typst-overlay--invalidate-overlay))
    (when (and (>= (point) beg) (<= (point) end))
      (typst-overlay--hide-overlay overlay)
      (setq typst-overlay--active-overlay overlay))
    overlay))

(defun typst-overlay--recolor-all-overlays ()
  "Recolor all visible overlays to match the current foreground."
  (when (and typst-overlay-mode typst-overlay--registry)
    (maphash
     (lambda (_key record)
       (when (eq (typst-overlay-record-state record) 'visible)
         (let* ((artifact (typst-overlay-record-artifact record))
                (overlay (typst-overlay-record-overlay record)))
           (when (and artifact (overlayp overlay))
             (let* ((svg-data (typst-overlay--recolor-svg
                               (typst-overlay-artifact-svg-path artifact)))
                    (image (create-image svg-data 'svg t :ascent 'center :scale typst-overlay-scale)))
               (overlay-put overlay 'typst-overlay-image image)
               (unless (eq overlay typst-overlay--active-overlay)
                 (overlay-put overlay 'display image)))))))
     (typst-overlay-registry-records typst-overlay--registry))))

(defun typst-overlay--on-theme-change (&rest _)
  "Handle theme changes by recoloring overlays in all typst-overlay buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when typst-overlay-mode
          (typst-overlay--recolor-all-overlays))))))

(defun typst-overlay--teardown ()
  "Remove overlays and clear buffer-local runtime state."
  (setq typst-overlay--compile-queue nil
        typst-overlay--active-compiles 0)
  (when (overlayp typst-overlay--active-overlay)
    (typst-overlay--show-overlay typst-overlay--active-overlay))
  (setq typst-overlay--active-overlay nil
        typst-overlay--shown-error-overlay nil)
  (when typst-overlay--registry
    (maphash
     (lambda (_key record)
       (typst-overlay--delete-record-overlay record))
     (typst-overlay-registry-records typst-overlay--registry)))
  (setq typst-overlay--snapshot nil
        typst-overlay--registry nil
        typst-overlay--artifact-cache nil))

(defun typst-overlay--build-element-source (element)
  "Build a complete Typst source string for ELEMENT."
  (let ((prelude (typst-overlay-element-prelude-text element))
        (math (typst-overlay-element-text element)))
    (concat
     "#set page(width: auto, height: auto, margin: 1pt, fill: none)\n"
     "#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n"
     "#set text(fill: rgb(\"#000000\"))\n"
     (if (string-empty-p typst-overlay-extra-prelude)
         ""
       (concat typst-overlay-extra-prelude "\n"))
     prelude
     (unless (string-empty-p prelude) "\n\n")
     math
     "\n")))

(defun typst-overlay--foreground-color ()
  "Return the default face's foreground color, or black."
  (let ((fg (face-foreground 'default nil t)))
    (if (stringp fg) fg "#000000")))

(defun typst-overlay--drain-compile-queue ()
  "Start queued compiles until the concurrency limit is reached."
  (while (and typst-overlay--compile-queue
              (< typst-overlay--active-compiles typst-overlay-max-active-compiles))
    (let ((thunk (pop typst-overlay--compile-queue)))
      (funcall thunk))))

(defun typst-overlay--start-render-for-element (element generation artifact-cache)
  "Start async render for ELEMENT at GENERATION.
ARTIFACT-CACHE receives the result.  Respects the concurrency limit."
  (let* ((buffer (current-buffer))
         (file (buffer-file-name buffer))
         (default-directory (if file
                                (file-name-directory file)
                              default-directory))
         (cache-key (typst-overlay-element-cache-key element))
         (svg-path (typst-overlay--artifact-svg-path cache-key))
         (source (typst-overlay--build-element-source element))
         ;; Markers follow edits made while the compile runs, so the
         ;; result can be placed where the equation is by then.
         (markers (typst-overlay--element-markers element))
         (callback (lambda (status output)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (let ((current (typst-overlay--relocate-element
                                         element markers)))
                           (set-marker (car markers) nil)
                           (set-marker (cdr markers) nil)
                           (cl-decf typst-overlay--active-compiles)
                           (typst-overlay--drain-compile-queue)
                           (if (eq status 'success)
                               (typst-overlay--handle-render-success
                                element current generation
                                cache-key svg-path artifact-cache)
                             (typst-overlay--handle-render-failure
                              element current generation output))))))))
    (if (< typst-overlay--active-compiles typst-overlay-max-active-compiles)
        (progn
          (cl-incf typst-overlay--active-compiles)
          (typst-overlay--start-async-compile source svg-path callback))
      (push (lambda ()
              (cl-incf typst-overlay--active-compiles)
              (typst-overlay--start-async-compile source svg-path callback))
            typst-overlay--compile-queue))))

(defun typst-overlay--start-async-compile (source svg-path callback)
  "Compile SOURCE to SVG-PATH asynchronously, then call CALLBACK.
CALLBACK receives the symbol `success' or `failure', and the
compiler's output.  `default-directory' must be bound by the caller
to resolve #import paths."
  (let ((buffer (generate-new-buffer " *typst-overlay-compile*")))
    (make-process
     :name "typst-overlay-compile"
     :buffer buffer
     :command (list "typst" "compile" "-" svg-path "--format" "svg")
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((ok (and (= (process-exit-status proc) 0)
                        (file-exists-p svg-path)
                        (not (file-directory-p svg-path)))))
           (unwind-protect
               (funcall callback
                        (if ok 'success 'failure)
                        (with-current-buffer (process-buffer proc)
                          (buffer-string)))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc))))))))
    (let ((proc (get-buffer-process buffer)))
      (process-send-string proc source)
      (process-send-eof proc)
      proc)))

(defun typst-overlay--element-markers (element)
  "Return markers (BEG . END) around ELEMENT that follow later edits.
Text typed right before or after ELEMENT stays outside the markers."
  (cons (copy-marker (typst-overlay-element-beg element) t)
        (copy-marker (typst-overlay-element-end element))))

(defun typst-overlay--relocate-element (element markers)
  "Return ELEMENT at its current position, or nil if its text changed.
MARKERS is a (BEG . END) pair from `typst-overlay--element-markers',
made when ELEMENT's render started."
  (let ((beg (marker-position (car markers)))
        (end (marker-position (cdr markers))))
    (when (and beg end
               (string= (buffer-substring-no-properties beg end)
                        (typst-overlay-element-text element)))
      (let ((current (copy-typst-overlay-element element)))
        (setf (typst-overlay-element-beg current) beg
              (typst-overlay-element-end current) end)
        current))))

(defun typst-overlay--handle-render-success
    (element current generation cache-key svg-path artifact-cache)
  "Commit successful render for ELEMENT if GENERATION is still current.
CURRENT is ELEMENT at its position now, or nil if its text changed
while compiling; then the artifact is kept for the next refresh.
CACHE-KEY, SVG-PATH and ARTIFACT-CACHE describe the new artifact."
  (when typst-overlay-mode
    (let* ((record (typst-overlay--get-record typst-overlay--registry element))
           (artifact (make-typst-overlay-artifact
                      :cache-key cache-key
                      :svg-path svg-path)))
      (puthash cache-key artifact artifact-cache)
      (when (and record
                 (= (typst-overlay-record-generation record) generation))
        (typst-overlay--delete-record-overlay record)
        (setf (typst-overlay-record-artifact record) artifact)
        (if current
            (setf (typst-overlay-record-state record) 'visible
                  (typst-overlay-record-overlay record)
                  (typst-overlay--place-artifact-overlay current artifact))
          (setf (typst-overlay-record-state record) 'stale
                (typst-overlay-record-overlay record) nil))))))

(defun typst-overlay--compile-error-summary (output)
  "Return the first error line of typst OUTPUT, without its prefix."
  (let ((lines (split-string output "\n" t "[ \t]+")))
    (or (seq-some (lambda (line)
                    (and (string-prefix-p "error: " line)
                         (substring line (length "error: "))))
                  lines)
        (car lines)
        "Compilation failed")))

(defun typst-overlay--handle-render-failure (element current generation output)
  "Mark ELEMENT failed if its record still matches GENERATION.
CURRENT is ELEMENT at its position now, or nil if its text changed
while compiling.  If CURRENT is non-nil, underline it with face
`typst-overlay-error' and attach the error from typst OUTPUT, which
is shown when point enters the equation."
  (when typst-overlay-mode
    (let ((record (typst-overlay--get-record typst-overlay--registry element)))
      (when (and record
                 (= (typst-overlay-record-generation record) generation))
        (typst-overlay--delete-record-overlay record)
        (setf (typst-overlay-record-state record) 'failed
              (typst-overlay-record-overlay record)
              (and current
                   (typst-overlay--place-error-overlay
                    current (typst-overlay--compile-error-summary output))))))))

(defun typst-overlay--place-error-overlay (element message)
  "Create and return an overlay marking ELEMENT as failed with MESSAGE."
  (let ((overlay (make-overlay (typst-overlay-element-beg element)
                               (typst-overlay-element-end element)
                               nil t nil)))
    (overlay-put overlay 'face 'typst-overlay-error)
    (overlay-put overlay 'typst-overlay-error message)
    (overlay-put overlay 'help-echo message)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks '(typst-overlay--invalidate-overlay))
    overlay))

(defun typst-overlay--artifact-svg-path (cache-key)
  "Return the cached SVG path for CACHE-KEY in the current file's directory."
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file))))
    (unless dir
      (error "A file-backed buffer is required"))
    (let ((cache-dir (expand-file-name typst-overlay-cache-dir-name dir)))
      (unless (file-directory-p cache-dir)
        (make-directory cache-dir t))
      (expand-file-name (concat cache-key ".svg") cache-dir))))

(defun typst-overlay--invalidate-overlays-past-error (first-error)
  "Delete all overlays starting at or after FIRST-ERROR position."
  (when (and first-error typst-overlay--registry)
    (maphash
     (lambda (_key record)
       (let ((element (typst-overlay-record-element record)))
         (when (and element
                    (>= (typst-overlay-element-beg element) first-error))
           (typst-overlay--delete-record-overlay record)
           (setf (typst-overlay-record-state record) 'stale))))
     (typst-overlay-registry-records typst-overlay--registry))))

;; loop
(defun typst-overlay--empty-snapshot ()
  "Return an empty snapshot."
  (make-typst-overlay-snapshot
   :version 0
   :elements nil
   :code-nodes nil
   :math-nodes nil))

(defun typst-overlay-refresh ()
  "Refresh Typst overlays for the current buffer."
  (interactive)
  (unless typst-overlay-mode
    (user-error "Mode typst-overlay-mode not active"))
  (typst-overlay--ensure-runtime)
  (let* ((old-snapshot (or typst-overlay--snapshot
                           (typst-overlay--empty-snapshot)))
         (analysis (funcall typst-overlay--analyzer))
         (new-snapshot (typst-overlay--make-snapshot analysis))
         (diff (typst-overlay--diff-snapshots old-snapshot new-snapshot))
         (generation (1+ (typst-overlay-registry-generation
                          typst-overlay--registry))))
    (typst-overlay--invalidate-overlays-past-error
     (typst-overlay-analysis-first-error analysis))
    (let ((plan (typst-overlay--plan-render
                 diff
                 typst-overlay--registry
                 typst-overlay--artifact-cache
                 generation)))
      (typst-overlay--apply-render-plan
       plan
       typst-overlay--registry
       typst-overlay--artifact-cache)
      (setq typst-overlay--snapshot new-snapshot)))
  t)

;; mode
(defun typst-overlay-save-refresh ()
  "Refresh Typst overlays if `typst-overlay-mode' is active.
Intended for use in `after-save-hook'."
  (when typst-overlay-mode
    (typst-overlay-refresh)))

(defvar-local typst-overlay--last-point nil
  "Tracks previous point location to detect upward cursor movement into overlays.")

(defun typst-overlay--handle-upward-entry ()
  "Jump to overlay end if entering a `typst-overlay' from below."
  (let ((curr-point (point)))
    (when (and typst-overlay--last-point
               (< curr-point typst-overlay--last-point)) ; Moving upwards
      (let* ((overlays (overlays-at curr-point))
             (typst-ov (cl-find-if (lambda (o) (overlay-get o 'typst-overlay)) overlays)))
        (when typst-ov
          (unless (and (>= typst-overlay--last-point (overlay-start typst-ov))
                       (<= typst-overlay--last-point (overlay-end typst-ov)))
            (goto-char (1- (overlay-end typst-ov)))))))
    (setq typst-overlay--last-point curr-point)))

(defun typst-overlay--missing-requirement ()
  "Return a message describing a missing requirement, or nil."
  (cond
   ((not (executable-find "typst"))
    "Binary typst not found in PATH")
   ((not (treesit-language-available-p 'typst))
    (format "Typst tree-sitter grammar not found; install it with `M-x treesit-install-language-grammar' from %s"
            typst-overlay--grammar-url))))

(defun typst-overlay--enable ()
  "Set up `typst-overlay-mode' in the current buffer."
  (when-let* ((problem (typst-overlay--missing-requirement)))
    ;; Leave the mode off rather than half set up.
    (setq typst-overlay-mode nil)
    (user-error "%s" problem))
  (setq-local typst-overlay--analyzer
              (if (derived-mode-p 'org-mode)
                  #'typst-overlay--analyze-org
                #'typst-overlay--analyze-typst))
  (typst-overlay--ensure-runtime)
  (setq typst-overlay--last-point (point))
  (add-hook 'post-command-hook #'typst-overlay--post-command-update nil t)
  (add-hook 'enable-theme-functions #'typst-overlay--on-theme-change)
  (add-hook 'disable-theme-functions #'typst-overlay--on-theme-change)
  (typst-overlay-refresh))

(defun typst-overlay--disable ()
  "Tear down `typst-overlay-mode' in the current buffer."
  (remove-hook 'post-command-hook #'typst-overlay--post-command-update t)
  (remove-hook 'enable-theme-functions #'typst-overlay--on-theme-change)
  (remove-hook 'disable-theme-functions #'typst-overlay--on-theme-change)
  (kill-local-variable 'typst-overlay--last-point)
  (typst-overlay--teardown))

;;;###autoload
(define-minor-mode typst-overlay-mode
  "Render Typst math overlays."
  :lighter " TypstOv"
  (if typst-overlay-mode
      (typst-overlay--enable)
    (typst-overlay--disable)))

(provide 'typst-overlay)

;;; typst-overlay.el ends here
