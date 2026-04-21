;;; typst-overlay.el --- Overlay Typst equations -*- lexical-binding: t; -*-

;; Author: Hesam Pakdaman
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/hesampakdaman/typst-overlay

;;; Commentary:
;; Brief description

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; constants / buffer-local state
(defconst typst-overlay--cache-dir-name ".typst-overlay-cache")
(defconst typst-overlay--max-active-compiles 8)
(defvar-local typst-overlay--compile-queue nil)
(defvar-local typst-overlay--active-compiles 0)
(defvar-local typst-overlay--snapshot nil)
(defvar-local typst-overlay--active-overlay nil)
(defvar-local typst-overlay--registry nil)
(defvar-local typst-overlay--artifact-cache nil)

;; analyzer
(cl-defstruct typst-overlay-code-node
  beg
  end
  text
  hash)

(defun typst-overlay--make-code-node (node)
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
code blocks. We only want top-level (outermost) ones, since nested
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
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (text (buffer-substring-no-properties beg end)))
    (make-typst-overlay-math-node
     :beg beg
     :end end
     :text text
     :text-hash (md5 text))))

(defun typst-overlay--collect-math-nodes ()
  "Collect math nodes that should be rendered (i.e. in content, not code).

We query all `math` nodes via tree-sitter. However, math can appear
both in normal document content and inside `code` blocks. We only want
math that is part of rendered content.

For each math node, we walk up its parent chain:

- If we encounter a `content` node first, we include it.
- If we encounter a `code` node first, we exclude it.
- If neither is found (unexpected/edge case), we include it by default.

This effectively selects math expressions that belong to the document
body rather than programmatic code."
  (let* ((root (treesit-buffer-root-node))
         (query '((math) @math))
         (captures (treesit-query-capture root query))
         result)
    (dolist (cap captures)
      (let* ((node (cdr cap))
             (parent (treesit-node-parent node))
             (include nil)
             (done nil))
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
        ;; Include if explicitly in content, or if no parent found.
        (when (or include (not done))
          (push (typst-overlay--make-math-node node) result))))
    (nreverse result)))

(defun typst-overlay--sort-math-nodes (nodes)
  (sort nodes
        (lambda (a b)
          (< (typst-overlay-math-node-beg a)
             (typst-overlay-math-node-beg b)))))

(cl-defstruct typst-overlay-analysis
  code-nodes
  math-nodes)

(defun typst-overlay--analyze ()
  (make-typst-overlay-analysis
   :code-nodes (typst-overlay--sort-code-nodes
                (typst-overlay--collect-code-nodes))
   :math-nodes (typst-overlay--sort-math-nodes
                (typst-overlay--collect-math-nodes))))


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
  (let* ((text-hash (typst-overlay-math-node-text-hash math-node))
         (prelude-text (typst-overlay--build-prelude-text prelude-code-nodes))
         (prelude-hash (md5 prelude-text))
         (cache-key (md5 (concat text-hash prelude-hash))))
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
  (let ((text (string-trim-left (typst-overlay-code-node-text code-node))))
    (or (string-prefix-p "#let" text)
        (string-prefix-p "#import" text)
        (string-prefix-p "#include" text))))

(defun typst-overlay--make-snapshot (analysis)
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
                 (append queue (list element))
                 table)))
    table))

(defun typst-overlay--find-old-match (new-element old-queues)
  "Return first unmatched old element for NEW-ELEMENT, or nil."
  (let* ((cache-key (typst-overlay-element-cache-key new-element))
         (queue (gethash cache-key old-queues)))
    (car queue)))

(defun typst-overlay--consume-old-match (element old-queues)
  "Consume one unmatched old element with the same cache-key as ELEMENT."
  (let* ((cache-key (typst-overlay-element-cache-key element))
         (queue (gethash cache-key old-queues)))
    ;; Drop the first unmatched old element from the queue.
    (puthash cache-key (cdr queue) old-queues)))

(defun typst-overlay--make-added-entry (new-element)
  (make-typst-overlay-diff-entry
   :status 'added
   :old nil
   :new new-element))

(defun typst-overlay--make-matched-entry (old-element new-element)
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

Elements are matched by `cache-key` (render identity). Because
multiple elements may share the same cache-key, OLD elements are
grouped into queues (one queue per cache-key).

We then walk NEW elements in document order and try to consume
one matching OLD element from the corresponding queue:

- If a match is found:
  - same position → 'unchanged
  - different position → 'moved
  The matched OLD element is removed from the queue.

- If no match is found:
  → 'added

After processing all NEW elements, any OLD elements left in the
queues were not matched and are therefore 'deleted.

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
  (make-typst-overlay-registry
   :records (make-hash-table :test #'equal)
   :generation 0))

(defun typst-overlay--make-artifact-cache ()
  (make-hash-table :test #'equal))

(defun typst-overlay--load-artifact-cache ()
  "Populate artifact cache from SVG files already on disk."
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file)))
         (cache-dir (and dir (expand-file-name typst-overlay--cache-dir-name dir))))
    (when (and cache-dir (file-directory-p cache-dir))
      (dolist (svg-path (directory-files cache-dir t "\\.svg\\'"))
        (let ((cache-key (file-name-base svg-path)))
          (puthash cache-key
                   (make-typst-overlay-artifact
                    :cache-key cache-key
                    :svg-path svg-path)
                   typst-overlay--artifact-cache))))))

(defun typst-overlay--ensure-runtime ()
  (unless typst-overlay--registry
    (setq typst-overlay--registry
          (typst-overlay--make-registry)))
  (unless typst-overlay--artifact-cache
    (setq typst-overlay--artifact-cache
          (typst-overlay--make-artifact-cache))
    (typst-overlay--load-artifact-cache)))

(defun typst-overlay--occurrence-key (element)
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
  "Build place ops for entries that can reuse an existing artifact.

Cases:
- moved + old record has artifact  -> place-op
- added + artifact cache hit       -> place-op"
  (let (ops)
    (dolist (entry (typst-overlay-diff-entries diff))
      (pcase (typst-overlay-diff-entry-status entry)
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
  "Build render ops for entries that do not have a reusable artifact.

Cases:
- moved + old record has no artifact -> render-op
- added + artifact cache miss        -> render-op

Unchanged entries are no-op."
  (let (ops)
    (dolist (entry (typst-overlay-diff-entries diff))
      (pcase (typst-overlay-diff-entry-status entry)
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
  "Return the registry record for ELEMENT, or nil if none exists."
  (gethash (typst-overlay--occurrence-key element)
           (typst-overlay-registry-records registry)))

;; render
(defun typst-overlay--apply-render-plan (plan registry artifact-cache)
  "Apply PLAN by mutating REGISTRY and runtime state."
  (let ((generation (typst-overlay-render-plan-generation plan)))
    (dolist (op (typst-overlay-render-plan-delete plan))
      (typst-overlay--apply-delete-op op registry))
    (dolist (op (typst-overlay-render-plan-place plan))
      (typst-overlay--apply-place-op op registry generation))
    (dolist (op (typst-overlay-render-plan-render plan))
      (typst-overlay--apply-render-op op registry generation artifact-cache))
    (setf (typst-overlay-registry-generation registry) generation)))

(defun typst-overlay--apply-delete-op (op registry)
  "Apply delete OP by removing overlay and record for the old occurrence."
  (let* ((old-element (typst-overlay-delete-op-old op))
         (record (typst-overlay--get-record registry old-element)))
    (when record
      (typst-overlay--delete-record-overlay record)
      (typst-overlay--remove-record registry old-element))))

(defun typst-overlay--apply-place-op (op registry generation)
  "Apply place OP by making NEW visible from an existing ARTIFACT."
  (let* ((old-element (typst-overlay-place-op-old op))
         (new-element (typst-overlay-place-op-new op))
         (artifact (typst-overlay-place-op-artifact op))
         (record (and old-element
                      (typst-overlay--get-record registry old-element))))
    (when old-element
      (when record
        (typst-overlay--delete-record-overlay record))
      (typst-overlay--remove-record registry old-element))
    (when (null old-element)
      (setq record (make-typst-overlay-record)))
    (setf (typst-overlay-record-element record) new-element
          (typst-overlay-record-state record) 'visible
          (typst-overlay-record-artifact record) artifact
          (typst-overlay-record-generation record) generation
          (typst-overlay-record-overlay record)
          (typst-overlay--place-artifact-overlay new-element artifact))
    (typst-overlay--put-record registry new-element record)))

(defun typst-overlay--apply-render-op (op registry generation artifact-cache)
  "Apply render OP by registering NEW as rendering and starting async render."
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
  "Store RECORD in REGISTRY for ELEMENT's occurrence key."
  (puthash (typst-overlay--occurrence-key element)
           record
           (typst-overlay-registry-records registry)))

(defun typst-overlay--remove-record (registry element)
  "Remove REGISTRY entry for ELEMENT's occurrence key."
  (remhash (typst-overlay--occurrence-key element)
           (typst-overlay-registry-records registry)))

(defun typst-overlay--delete-record-overlay (record)
  "Delete RECORD's live overlay, if any, and clear the slot."
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

(defun typst-overlay--post-command-update ()
  "Hide overlay under point and restore the previously active one."
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
    (delete-overlay overlay)))

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
         (image (create-image svg-data 'svg t :ascent 'center :scale 1.2))
         (overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'display image)
    (overlay-put overlay 'typst-overlay t)
    (overlay-put overlay 'typst-overlay-image image)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks '(typst-overlay--invalidate-overlay))
    (overlay-put overlay 'insert-in-front-hooks '(typst-overlay--invalidate-overlay))
    (overlay-put overlay 'insert-behind-hooks '(typst-overlay--invalidate-overlay))
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
                    (image (create-image svg-data 'svg t :ascent 'center :scale 1.2)))
               (overlay-put overlay 'typst-overlay-image image)
               (unless (eq overlay typst-overlay--active-overlay)
                 (overlay-put overlay 'display image)))))))
     (typst-overlay-registry-records typst-overlay--registry))))

(defun typst-overlay--on-theme-change (&rest _)
  "Handle theme changes by recoloring overlays."
  (when typst-overlay-mode
    (typst-overlay--recolor-all-overlays)))

(defun typst-overlay--teardown ()
  "Remove overlays and clear buffer-local runtime state."
  (setq typst-overlay--compile-queue nil
        typst-overlay--active-compiles 0)
  (when (overlayp typst-overlay--active-overlay)
    (typst-overlay--show-overlay typst-overlay--active-overlay))
  (setq typst-overlay--active-overlay nil)
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
     prelude
     (unless (string-empty-p prelude) "\n\n")
     math
     "\n")))

(defun typst-overlay--foreground-color ()
  (let ((fg (face-foreground 'default nil t)))
    (if (stringp fg) fg "#000000")))

(defun typst-overlay--drain-compile-queue ()
  "Start queued compiles until the concurrency limit is reached."
  (while (and typst-overlay--compile-queue
              (< typst-overlay--active-compiles typst-overlay--max-active-compiles))
    (let ((thunk (pop typst-overlay--compile-queue)))
      (funcall thunk))))

(defun typst-overlay--start-render-for-element (element generation artifact-cache)
  "Start async render for ELEMENT at GENERATION, respecting concurrency limit."
  (let* ((buffer (current-buffer))
         (file (buffer-file-name buffer))
         (default-directory (if file
                                (file-name-directory file)
                              default-directory))
         (cache-key (typst-overlay-element-cache-key element))
         (svg-path (typst-overlay--artifact-svg-path cache-key))
         (source (typst-overlay--build-element-source element))
         (callback (lambda (status)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (cl-decf typst-overlay--active-compiles)
                         (typst-overlay--drain-compile-queue)
                         (if (eq status 'success)
                             (typst-overlay--handle-render-success
                              element generation cache-key svg-path artifact-cache)
                           (typst-overlay--handle-render-failure
                            element generation)))))))
    (if (< typst-overlay--active-compiles typst-overlay--max-active-compiles)
        (progn
          (cl-incf typst-overlay--active-compiles)
          (typst-overlay--start-async-compile source svg-path element callback))
      (push (lambda ()
              (cl-incf typst-overlay--active-compiles)
              (typst-overlay--start-async-compile source svg-path element callback))
            typst-overlay--compile-queue))))

(defun typst-overlay--start-async-compile
    (source svg-path element callback)
  "Compile SOURCE to SVG-PATH asynchronously, then call CALLBACK.
CALLBACK receives either the symbol `success' or `failure'.
`default-directory' must be bound by the caller to resolve #import paths."
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
           (unless ok
             (with-current-buffer (process-buffer proc)
               (rename-buffer
                (format "*typst-overlay-error:%s*"
                        (truncate-string-to-width
                         (typst-overlay-element-text element) 20 nil nil t))
                t)))
           (unwind-protect
               (funcall callback (if ok 'success 'failure))
             (when ok
               (when (buffer-live-p (process-buffer proc))
                 (kill-buffer (process-buffer proc)))))))))
    (let ((proc (get-buffer-process buffer)))
      (process-send-string proc source)
      (process-send-eof proc)
      proc)))

(defun typst-overlay--handle-render-success
    (element generation cache-key svg-path artifact-cache)
  "Commit successful render for ELEMENT if its record is still current."
  (when typst-overlay-mode
    (let* ((record (typst-overlay--get-record typst-overlay--registry element))
           (artifact (make-typst-overlay-artifact
                      :cache-key cache-key
                      :svg-path svg-path)))
      (puthash cache-key artifact artifact-cache)
      (when (and record
                 (= (typst-overlay-record-generation record) generation))
        (typst-overlay--delete-record-overlay record)
        (setf (typst-overlay-record-state record) 'visible
              (typst-overlay-record-artifact record) artifact
              (typst-overlay-record-overlay record)
              (typst-overlay--place-artifact-overlay element artifact))))))

(defun typst-overlay--handle-render-failure (element generation)
  "Mark ELEMENT failed if its record is still current."
  (when typst-overlay-mode
    (let ((record (typst-overlay--get-record typst-overlay--registry element)))
      (when (and record
                 (= (typst-overlay-record-generation record) generation))
        (setf (typst-overlay-record-state record) 'failed)))))

(defun typst-overlay--artifact-svg-path (cache-key)
  "Return the cached SVG path for CACHE-KEY in the current file's directory."
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file))))
    (unless dir
      (error "typst-overlay requires a file-backed buffer"))
    (let ((cache-dir (expand-file-name typst-overlay--cache-dir-name dir)))
      (unless (file-directory-p cache-dir)
        (make-directory cache-dir t))
      (expand-file-name (concat cache-key ".svg") cache-dir))))

;; loop
(defun typst-overlay--empty-snapshot ()
  "Return an empty snapshot."
  (make-typst-overlay-snapshot
   :version 0
   :elements nil
   :code-nodes nil
   :math-nodes nil))

(defun typst-overlay--refresh ()
  "Refresh Typst overlays for the current buffer."
  (typst-overlay--ensure-runtime)
  (let* ((old-snapshot (or typst-overlay--snapshot
                           (typst-overlay--empty-snapshot)))
         (analysis (typst-overlay--analyze))
         (new-snapshot (typst-overlay--make-snapshot analysis))
         (diff (typst-overlay--diff-snapshots old-snapshot new-snapshot))
         (generation (1+ (typst-overlay-registry-generation
                          typst-overlay--registry)))
         (plan (typst-overlay--plan-render
                diff
                typst-overlay--registry
                typst-overlay--artifact-cache
                generation)))
    (typst-overlay--apply-render-plan
     plan
     typst-overlay--registry
     typst-overlay--artifact-cache)
    (setq typst-overlay--snapshot new-snapshot)))

;; mode
(defun typst-overlay--after-save ()
  "Refresh overlays after saving the buffer."
  (when typst-overlay-mode
    (typst-overlay--refresh)))

;;;###autoload
(define-minor-mode typst-overlay-mode
  "Render Typst math overlays."
  :lighter " TypstOv"
  (if typst-overlay-mode
      (progn
        (typst-overlay--ensure-runtime)
        (add-hook 'post-command-hook #'typst-overlay--post-command-update nil t)
        (add-hook 'after-save-hook #'typst-overlay--after-save nil t)
        (add-hook 'enable-theme-functions #'typst-overlay--on-theme-change)
        (add-hook 'disable-theme-functions #'typst-overlay--on-theme-change)
        (typst-overlay--refresh))
    (remove-hook 'post-command-hook #'typst-overlay--post-command-update t)
    (remove-hook 'after-save-hook #'typst-overlay--after-save t)
    (remove-hook 'enable-theme-functions #'typst-overlay--on-theme-change)
    (remove-hook 'disable-theme-functions #'typst-overlay--on-theme-change)
    (typst-overlay--teardown)))

(provide 'typst-overlay)

;;; typst-overlay.el ends here
