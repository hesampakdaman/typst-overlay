;;; typst-overlay.el --- Overlay Typst equations -*- lexical-binding: t; -*-

;; Author: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Maintainer: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Version: 0.7.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/hesampakdaman/typst-overlay
;; Assisted-by: Claude:claude-sonnet-4-6, Claude:claude-opus-5-5

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; typst-overlay renders Typst math equations as inline overlays.
;; Equations are compiled asynchronously to SVG with the `typst'
;; compiler, many per process, and displayed over the source text.  The overlay is hidden
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
;; Edited equations are rendered again once you stop typing; see
;; `typst-overlay-idle-refresh-delay'.
;;
;; Equations that fail to compile are reported to Flymake, in org
;; buffers by default; see `typst-overlay-use-flymake'.
;;
;; Requires the `typst' executable on PATH and the Typst tree-sitter
;; grammar.  See `customize-group' `typst-overlay' for options.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'project)
(require 'subr-x)
(require 'treesit)

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-type "org-element-ast" (node &optional anonymous))
(declare-function flymake-make-diagnostic "flymake"
                  (locus beg end type text &optional data overlay-properties))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-start "flymake" (&optional deferred force))

(defvar flymake-mode)

(defvar typst-overlay-mode)

(defconst typst-overlay-version (package-get-version)
  "Version of typst-overlay, read from its Version header when loaded.")

;;;###autoload
(defun typst-overlay-version ()
  "Show and return the version of typst-overlay."
  (interactive)
  (message "typst-overlay %s" typst-overlay-version)
  typst-overlay-version)

;; customization
(defgroup typst-overlay nil
  "Render Typst math equations as overlays."
  :group 'tools
  :prefix "typst-overlay-")

(defcustom typst-overlay-scale 1.0
  "Size of rendered equations relative to the surrounding text.
At 1.0 the math is drawn at the same size as the text around it.
Equations follow that text's size, so they grow and shrink with
`text-scale-adjust' and are larger in larger faces, such as org
headings.  Changing it resizes the equations already shown."
  :type 'number
  :group 'typst-overlay)

(defcustom typst-overlay-cache-directory
  (expand-file-name "typst-overlay/"
                    (cond ((getenv "XDG_CACHE_HOME"))
                          ((and (eq system-type 'windows-nt) (getenv "LOCALAPPDATA")))
                          ((eq system-type 'darwin) "~/Library/Caches")
                          ((file-directory-p "~/.cache") "~/.cache")
                          (t user-emacs-directory)))
  "Directory where rendered equations are kept, as SVG files.
By default it is in the user's cache directory: $XDG_CACHE_HOME if
set, %LOCALAPPDATA% on Windows, ~/Library/Caches on macOS, or else
~/.cache, or failing that `user-emacs-directory'.
They are kept in a folder per folder of the files they were rendered
for, named after its path, so reopening a file shows them without
compiling again.  The directory is safe to delete at any time."
  :type 'directory
  :group 'typst-overlay
  :package-version '(typst-overlay . "0.7.0"))

(defcustom typst-overlay-cache-max-age 30
  "Days after which an image not shown is deleted from the cache.
Images are checked at most once a day, while Emacs is idle.  If nil,
they are never deleted."
  :type '(choice (const :tag "Never" nil)
                 (natnum :tag "Days"))
  :group 'typst-overlay
  :package-version '(typst-overlay . "0.7.0"))

(defvar typst-overlay-cache-dir-name nil)
(make-obsolete-variable 'typst-overlay-cache-dir-name
                        'typst-overlay-cache-directory "0.7.0")

(defcustom typst-overlay-max-active-compiles 8
  "Maximum number of concurrent Typst compilation processes.
Equations to render are spread evenly over this many processes, each
compiling its share in one batch.  A value below 1 counts as 1."
  :type '(restricted-sexp :tag "Processes"
                          :match-alternatives ((lambda (n) (and (natnump n) (> n 0)))))
  :group 'typst-overlay)

(defcustom typst-overlay-extra-prelude ""
  "Extra Typst code prepended to every rendered overlay element."
  :type 'string
  :group 'typst-overlay)

(defcustom typst-overlay-idle-refresh-delay 0.25
  "Seconds Emacs must be idle after an edit before overlays refresh.
So equations are rendered again shortly after you stop typing.  If
nil, they are rendered again only by `typst-overlay-refresh', for
example on save with `typst-overlay-save-refresh'."
  :type '(choice (const :tag "Only when refreshed by hand" nil)
                 (number :tag "Seconds"))
  :group 'typst-overlay)

(defcustom typst-overlay-preview t
  "Whether to show the equation being edited rendered beside it.
While point is inside an equation, its source is shown instead of its
image.  If non-nil, the image is also shown after the equation, or
below it if the equation spans lines, and is updated as the equation
is rendered again; see `typst-overlay-idle-refresh-delay'."
  :type 'boolean
  :group 'typst-overlay)

(defface typst-overlay-preview
  '((((background dark))
     :foreground reset :box (:line-width (-1 . -1) :color "gray40"))
    (((background light))
     :foreground reset :box (:line-width (-1 . -1) :color "gray70"))
    (t :foreground reset :box (:line-width (-1 . -1))))
  "Face of the preview of the equation being edited.
See `typst-overlay-preview'.  Its box sets the preview apart from the
text, and its foreground is the color the equation is drawn in."
  :group 'typst-overlay)

(defcustom typst-overlay-system-fonts nil
  "Whether typst may use the fonts installed on the system.
If nil, it uses only the fonts it comes with, which include its
default math and text fonts, and those in `typst-overlay-font-paths'.
Looking through the system's fonts can take most of a second every
time typst starts, so turn this on only if a prelude sets a font
installed on the system, or add that font's folder to
`typst-overlay-font-paths' instead."
  :type 'boolean
  :group 'typst-overlay)

(defcustom typst-overlay-font-paths nil
  "Folders where typst looks for more fonts.
Unlike `typst-overlay-system-fonts', only these folders are searched,
which is fast when they hold few fonts."
  :type '(repeat directory)
  :group 'typst-overlay)

(defcustom typst-overlay-use-flymake 'org
  "Where to report equations that fail to compile to Flymake.
If `org', in org buffers only, where no language server checks the
math.  If t, also in Typst buffers.  If nil, nowhere.

Where it applies, `typst-overlay-mode' adds `typst-overlay-flymake' to
`flymake-diagnostic-functions', so the errors show up once
`flymake-mode' is on.  With Eglot, they are reported alongside its
language server's, which stops at the first error in the document;
an equation it also reports is then reported twice.  Takes effect
when `typst-overlay-mode' is turned on."
  :type '(choice (const :tag "In org buffers" org)
                 (const :tag "Everywhere" t)
                 (const :tag "Nowhere" nil))
  :group 'typst-overlay)

(defface typst-overlay-equation
  '((t :foreground reset))
  "Face rendered equations are shown in.
Their black is drawn in its foreground, by default that of the
`default' face, so they follow theme changes.  Without it, they would
take the face of the source text they cover, which a major mode may
dim, as `typst-ts-mode' does for the $ delimiters.  It sets only the
foreground, so equations keep the size of the text around them, as
in org headings."
  :group 'typst-overlay)

(defface typst-overlay-error
  ;; Dotted where supported, unlike the wavy underline of Flymake.
  `((t :underline (:style ,(if (>= emacs-major-version 30) 'dots 'line)
                   :color "Red1")))
  "Face for equations that failed to render.
Their error is shown by Flymake, if `flymake-mode' is on, or when
point enters them."
  :group 'typst-overlay)

;; constants / buffer-local state
(defconst typst-overlay--grammar-url "https://github.com/uben0/tree-sitter-typst"
  "Source of the Typst tree-sitter grammar.")

(defvar-local typst-overlay--snapshot nil)
(defvar-local typst-overlay--active-overlay nil)
(defvar-local typst-overlay--registry nil)
(defvar-local typst-overlay--artifact-cache nil)
(defvar-local typst-overlay--compile-queue nil)
(defvar-local typst-overlay--active-compiles 0)
(defvar-local typst-overlay--session 0
  "Incremented on teardown, so compiles started before it are ignored.")
(defvar-local typst-overlay--analyzer #'typst-overlay--analyze-typst
  "Function to analyze the current buffer.
Should return a `typst-overlay-analysis' struct.")

;; analyzer
(cl-defstruct typst-overlay-code-node
  beg
  end
  text
  hash
  scope)  ;; (BEG . END) of the content block it is in, or nil at top level

(defun typst-overlay--make-code-node (node &optional scope)
  "Return a `typst-overlay-code-node' for the tree-sitter NODE.
SCOPE is the tree-sitter `content' node NODE is in, if any."
  (let* ((beg (treesit-node-start node))
         (end (treesit-node-end node))
         (text (buffer-substring-no-properties beg end)))
    (make-typst-overlay-code-node
     :beg beg
     :end end
     :text text
     :hash (md5 text)
     :scope (and scope (cons (treesit-node-start scope)
                             (treesit-node-end scope))))))

(defun typst-overlay--collect-code-nodes (captures)
  "Collect code nodes that sit in markup, in document order.
CAPTURES are those of `typst-overlay--query-nodes'.

They include every `code` node, but this includes code
nested in other code, such as `#x' in `#let f(x) = $#x$'.  We only
want code written directly in markup: at top level, or in a content
block like the `[...]' of `#enum.item[...]'.

To enforce this, for each matched node we walk up its parent chain
to the first `code` node or content block.  If it is a `code` node,
this node is nested and skipped.  If it is a content block, that block
is the node's scope: a `#let' there is only visible inside it.

The body of a section is a `content` node too, but a heading opens no
scope in Typst, so only `content` written as `[...]' counts.

Remaining nodes are converted to `typst-overlay-code-node's and
returned in document order."
  (let (result)
    (dolist (cap (seq-filter (lambda (cap) (eq (car cap) 'code)) captures))
      (let* ((node (cdr cap))
             (parent (treesit-node-parent node)))
        ;; Walk up the parent chain to the enclosing `code` or `[...]'.
        (while (and parent
                    (not (pcase (treesit-node-type parent)
                           ("code" t)
                           ("content" (eq (char-after (treesit-node-start parent))
                                          ?\[)))))
          (setq parent (treesit-node-parent parent)))
        (unless (and parent (string= (treesit-node-type parent) "code"))
          (push (typst-overlay--make-code-node node parent) result))))
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

(defun typst-overlay--collect-math-nodes (captures first-error)
  "Collect math nodes that should be rendered (i.e. in content, not code).

CAPTURES are those of `typst-overlay--query-nodes'.  Math at or after
FIRST-ERROR is skipped.

They include every `math` node.  However, math can appear
both in normal document content and inside `code` blocks.  We only want
math that is part of rendered content.

For each math node, we walk up its parent chain:

- If we encounter a `content` node first, we include it.
- If we encounter a `code` node first, we exclude it.
- If neither is found (unexpected/edge case), we include it by default.

This effectively selects math expressions that belong to the document
body rather than programmatic code.

Math in the body of a function or loop is excluded too, wherever it
is, as in `n => [$d_#n$]' or `#for i in xs [$i$]': its variables only
have values when the function is called, so it cannot be rendered on
its own.  Those are the `binder' captures, see `typst-overlay--query-nodes'.

Math nodes that appear after a parse error in the document are excluded."
  (let ((binders (mapcar (lambda (cap)
                           (cons (treesit-node-start (cdr cap))
                                 (treesit-node-end (cdr cap))))
                         (seq-filter (lambda (cap) (eq (car cap) 'binder)) captures)))
        result)
    (dolist (cap (seq-filter (lambda (cap) (eq (car cap) 'math)) captures))
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
          (when (and (or include (not done))
                     ;; Comparing with the few binders is much faster than
                     ;; walking up to the root from every math node.
                     (not (seq-some (lambda (binder)
                                      (and (< (car binder) node-beg)
                                           (<= (treesit-node-end node) (cdr binder))))
                                    binders)))
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

(defun typst-overlay--query-nodes (root)
  "Return the `code', `math' and `binder' nodes under ROOT, as query captures.
A binder binds variables for the code in its body: a function, written
as `x => ...' or `#let f(x) = ...', or a `for' or `while' loop.  All
are found in one pass, as each query walks the whole tree."
  (treesit-query-capture root '((code) @code (math) @math
                                (lambda) @binder (for) @binder (while) @binder
                                (let pattern: (call)) @binder)))

(defun typst-overlay--first-error (root)
  "Return the start of the first parse error under ROOT, or nil."
  (let ((captures (and (treesit-node-check root 'has-error)
                       (treesit-query-capture root '((ERROR) @error)))))
    (and captures
         (apply #'min (mapcar (lambda (cap) (treesit-node-start (cdr cap)))
                              captures)))))

(defun typst-overlay--analyze-typst ()
  "Analyze the current Typst buffer using tree-sitter.
Return a `typst-overlay-analysis' with its code nodes, math nodes
and the position of the first parse error, if any.

A syntax error in one equation usually makes tree-sitter give up on
the rest of the document, or read the rest of the document as part of
it, see `typst-overlay--swallowed-dollar'.  So when the buffer has
either, it is parsed again with its broken equations blanked out, see
`typst-overlay--repair-parse'.  Those equations are still returned,
so they are compiled and underlined with their error."
  (let* ((root (treesit-buffer-root-node))
         (captures (typst-overlay--query-nodes root))
         (first-error (typst-overlay--first-error root))
         (repair (and (or first-error (typst-overlay--first-swallowed captures))
                      (typst-overlay--repair-parse)))
         (root (if repair (car repair) root))
         (captures (if repair (typst-overlay--query-nodes root) captures))
         (first-error (if repair (typst-overlay--first-error root) first-error)))
    (make-typst-overlay-analysis
     :code-nodes (typst-overlay--sort-code-nodes
                  (typst-overlay--collect-code-nodes captures))
     :math-nodes (typst-overlay--sort-math-nodes
                  (nconc (typst-overlay--collect-math-nodes captures first-error)
                         (mapcar (pcase-lambda (`(,beg . ,end))
                                   (typst-overlay--span-math-node beg end))
                                 (cdr repair))))
     :first-error first-error)))

(defun typst-overlay--span-math-node (beg end)
  "Return a math node for the text from BEG to END."
  (let ((text (buffer-substring-no-properties beg end)))
    (make-typst-overlay-math-node :beg beg :end end
                                  :text text :text-hash (md5 text))))

(defvar typst-overlay--repair-buffer nil
  "Hidden buffer holding a copy of a Typst buffer being repaired.")

(defun typst-overlay--repair-parse ()
  "Parse a copy of the current buffer with broken equations blanked out.
Return (ROOT . BROKEN): ROOT is the root node of the repaired copy,
whose positions match the buffer, and BROKEN the (BEG . END) spans
of the equations that were blanked out.

Starting at the first parse error, or the first equation that
swallowed a $, the first unescaped $ that does not start clean math is
paired with the next one, as in org buffers, and blanked out with
spaces, keeping positions.  Dollar signs in
comments, strings and raw text are skipped; the grammar still finds
those inside parse errors.  This repeats until the copy parses, or
no equation is to blame, such as for an error in code."
  (let ((text (save-restriction
                (widen)
                (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (buffer-live-p typst-overlay--repair-buffer)
      (setq typst-overlay--repair-buffer
            (generate-new-buffer " *typst-overlay-repair*" t))
      (with-current-buffer typst-overlay--repair-buffer
        (buffer-disable-undo)
        (treesit-parser-create 'typst)))
    (with-current-buffer typst-overlay--repair-buffer
      (erase-buffer)
      (insert text)
      (let (broken span)
        (while (setq span (typst-overlay--blank-broken-math
                           (typst-overlay--first-problem
                            (treesit-buffer-root-node 'typst))))
          (push span broken))
        (cons (treesit-buffer-root-node 'typst) (nreverse broken))))))

(defconst typst-overlay--literal-node-types
  '("comment" "string" "raw_span" "raw_blck")
  "Typst node types whose dollar signs are not math.")

(defun typst-overlay--literal-p (pos)
  "Return non-nil if POS is in a comment, string or raw text."
  (let ((node (treesit-node-at pos 'typst)))
    (while (and node (not (member (treesit-node-type node)
                                  typst-overlay--literal-node-types)))
      (setq node (treesit-node-parent node)))
    node))

(defun typst-overlay--swallowed-dollar (node)
  "Return the position of a $ math NODE swallowed, or nil.
Typst ends math at the next unescaped $, but the tree-sitter grammar
lets a trailing operator take it as an operand: in $E_$, the
subscript is the closing $, and the math goes on to the next $ in the
document.  Such math has a $ inside that is not escaped, not in a
string, raw text or comment, and not where math nested in it starts
or ends, as in $#table($x$)$.  NODE is in the current buffer."
  (let ((beg (treesit-node-start node))
        (end (treesit-node-end node)))
    ;; Most math has no $ inside: check that first, without copying it.
    (when (save-excursion
            (goto-char (1+ beg))
            (search-forward "$" (1- end) t))
      (let ((nested (mapcan (lambda (cap)
                              (unless (treesit-node-eq (cdr cap) node)
                                (list (treesit-node-start (cdr cap))
                                      (1- (treesit-node-end (cdr cap))))))
                            (treesit-query-capture node '((math) @math)))))
        (save-excursion
          (goto-char (1+ beg))
          (catch 'found
            (while (search-forward "$" (1- end) t)
              (let ((pos (1- (point))))
                (unless (or (memq pos nested)
                            (typst-overlay--escaped-p pos)
                            (typst-overlay--literal-p pos))
                  (throw 'found pos))))))))))

(defun typst-overlay--first-swallowed (captures)
  "Return the start of the first math in CAPTURES that swallowed a $.
See `typst-overlay--swallowed-dollar'.  CAPTURES are those of
`typst-overlay--query-nodes', in document order."
  (seq-some (lambda (cap)
              (and (eq (car cap) 'math)
                   (typst-overlay--swallowed-dollar (cdr cap))
                   (treesit-node-start (cdr cap))))
            captures))

(defun typst-overlay--first-problem (root)
  "Return where the first broken equation under ROOT may start, or nil.
That is the first parse error or the first math that swallowed a $,
whichever comes first."
  (let ((positions (delq nil (list (typst-overlay--first-error root)
                                   (typst-overlay--first-swallowed
                                    (typst-overlay--query-nodes root))))))
    (and positions (apply #'min positions))))

(defun typst-overlay--blank-broken-math (error)
  "Blank out the first broken equation from the parse ERROR, a position.
Return its (BEG . END) span, or nil if ERROR is nil or no broken
equation follows it."
  (when error
    (save-excursion
      (goto-char error)
      (catch 'found
        (while (search-forward "$" nil t)
          (let* ((beg (1- (point)))
                 (limit (typst-overlay--paragraph-end))
                 (candidate (and (not (typst-overlay--escaped-p beg))
                                 (not (typst-overlay--literal-p beg))))
                 (clean (and candidate (typst-overlay--math-end beg limit)))
                 (end (and candidate (not clean)
                           (typst-overlay--find-closing limit))))
            (cond
             ;; Skip clean math whole, so its closing $ is not taken
             ;; for an opening one.
             (clean (goto-char clean))
             (end
              (let ((original (buffer-substring beg end)))
                (delete-region beg end)
                (insert (replace-regexp-in-string "[^\n]" " " original))
                (throw 'found (cons beg end))))
             (t (goto-char (1+ beg))))))))))

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

(defun typst-overlay--paragraph-end ()
  "Return the end of the paragraph at point.
The paragraph ends at the next blank line or org heading."
  (save-excursion
    (if (re-search-forward "\n[ \t]*\n\\|\n\\*+ " nil t)
        (match-beginning 0)
      (point-max))))

(defvar typst-overlay--math-parse-buffer nil
  "Hidden buffer reused to parse candidate equations on their own.
Reusing one buffer and parser is several times faster than
`treesit-parse-string', which creates both on every call.")

(defun typst-overlay--math-parse-buffer ()
  "Return the hidden Typst parse buffer, creating it if needed."
  (unless (buffer-live-p typst-overlay--math-parse-buffer)
    (setq typst-overlay--math-parse-buffer
          (generate-new-buffer " *typst-overlay-parse*" t))
    (with-current-buffer typst-overlay--math-parse-buffer
      (buffer-disable-undo)
      (treesit-parser-create 'typst)))
  typst-overlay--math-parse-buffer)

(defun typst-overlay--math-end (beg limit)
  "Return the end of the Typst math starting at BEG, or nil.
The text from BEG to LIMIT is parsed with the Typst tree-sitter
grammar, so nested math such as $#table($x$)$ is handled.  Return
nil if no math starts at BEG, or it does not parse cleanly or swallowed
a $, see `typst-overlay--swallowed-dollar'."
  (let ((text (buffer-substring-no-properties beg limit)))
    (with-current-buffer (typst-overlay--math-parse-buffer)
      (erase-buffer)
      (insert text)
      (let ((node (treesit-node-descendant-for-range
                   (treesit-buffer-root-node 'typst) 1 2)))
        (while (and node (not (equal (treesit-node-type node) "math")))
          (setq node (treesit-node-parent node)))
        (when (and node
                   (= (treesit-node-start node) 1)
                   (not (treesit-node-check node 'has-error))
                   (not (typst-overlay--swallowed-dollar node)))
          (+ beg (1- (treesit-node-end node))))))))

(defun typst-overlay--org-in-code-p (pos)
  "Return non-nil if POS is inside an org code or verbatim element."
  (save-excursion
    (goto-char pos)
    (memq (org-element-type (org-element-context))
          typst-overlay--org-code-types)))

(defun typst-overlay--find-closing (limit)
  "Return the position after the next unescaped $ before LIMIT, or nil.
Search from point."
  (catch 'found
    (while (search-forward "$" limit t)
      (unless (typst-overlay--escaped-p (1- (point)))
        (throw 'found (point))))))

(defun typst-overlay--analyze-org ()
  "Collect $...$ spans in an org buffer as Typst math.

Follows Typst's delimiter rules rather than org's LaTeX ones: each
unescaped dollar sign may start Typst math, and where it ends is
decided by the Typst tree-sitter grammar.  So math may be written
with or without surrounding whitespace (Typst decides inline vs.
display from it) and may nest, as in $#table($x$)$.  Write \\$ for a
literal dollar sign.

If the grammar finds no math at a dollar sign, for example because
the math has a syntax error, it is paired with the next unescaped
dollar sign instead.  The span is then compiled like any other, so
its syntax error is reported and underlined.

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
               (limit (typst-overlay--paragraph-end))
               (end (and (not (typst-overlay--escaped-p beg))
                         (or (typst-overlay--math-end beg limit)
                             (save-excursion
                               (typst-overlay--find-closing limit))))))
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

(defconst typst-overlay--render-version "2"
  "Version of how equations are compiled, part of every cache key.
Changing it renders cached equations again, for changes that make
their images differ, such as writing their depth to the SVG.")

(defun typst-overlay--make-element (math-node prelude-code-nodes &optional preludes)
  "Build an element from MATH-NODE and PRELUDE-CODE-NODES.
The cache key also covers `typst-overlay-extra-prelude' and the files
it imports, the fonts typst may use and `typst-overlay--render-version'.

PRELUDES, if non-nil, is a hash table from `typst-overlay--prelude-table'
that keeps each prelude's text and hash, so a prelude shared by many
math nodes is built and hashed once."
  (pcase-let* ((preludes (or preludes (typst-overlay--prelude-table)))
               (text-hash (typst-overlay-math-node-text-hash math-node))
               (`(,prelude-text . ,prelude-hash)
                (typst-overlay--prelude prelude-code-nodes preludes))
               (cache-key (md5 (concat text-hash prelude-hash
                                       (md5 (concat typst-overlay-extra-prelude
                                                    (typst-overlay--imports-hash
                                                     typst-overlay-extra-prelude
                                                     preludes)))
                                       typst-overlay--render-version
                                       ;; Only when changed, so images cached
                                       ;; before these options stay valid.
                                       (and (or typst-overlay-system-fonts
                                                typst-overlay-font-paths)
                                            (md5 (string-join (typst-overlay--font-args)
                                                              "\0")))))))
    (make-typst-overlay-element
     :beg (typst-overlay-math-node-beg math-node)
     :end (typst-overlay-math-node-end math-node)
     :text (typst-overlay-math-node-text math-node)
     :text-hash text-hash
     :prelude-text prelude-text
     :prelude-hash prelude-hash
     :cache-key cache-key)))

(defun typst-overlay--prelude-table ()
  "Return an empty table for `typst-overlay--prelude'.
It also keeps the hashes of the files the preludes import, by their
path, so each is read once."
  (make-hash-table :test #'equal))

(defun typst-overlay--prelude (code-nodes preludes)
  "Return (TEXT . HASH) of the prelude made of CODE-NODES.
PRELUDES is a table from `typst-overlay--prelude-table' to look it
up in, or store it in.  HASH also covers the files TEXT imports, so
equations are rendered again when one of them changes."
  (let ((key (mapcar #'typst-overlay-code-node-beg code-nodes)))
    (or (gethash key preludes)
        (let ((text (typst-overlay--build-prelude-text code-nodes)))
          (puthash key
                   (cons text (md5 (concat text (typst-overlay--imports-hash
                                                 text preludes))))
                   preludes)))))

(defconst typst-overlay--import-regexp
  "^[ \t]*#\\(?:import\\|include\\)[ \t]+\"\\([^\"]*\\)\""
  "Regexp matching a top-level #import or #include of a file.
Its first group is the path, as written.")

(defun typst-overlay--imports-hash (text files)
  "Return a hash of the files TEXT imports or includes, or \"\" if none.
Those are the files its #import and #include lines name, not the files
they import in turn, nor packages.  Relative paths are from the
buffer's folder, absolute ones from its project's root.  FILES is a
table from `typst-overlay--prelude-table' keeping each file's hash."
  (let ((start 0) hashes)
    (while (string-match typst-overlay--import-regexp text start)
      (setq start (match-end 0))
      (let ((path (match-string 1 text)))
        (unless (string-prefix-p "@" path)
          (let ((file (if (string-prefix-p "/" path)
                          (expand-file-name
                           (substring path 1)
                           (or (gethash :root files)
                               (puthash :root (typst-overlay--project-root
                                               (typst-overlay--buffer-dir))
                                        files)))
                        (expand-file-name path (typst-overlay--buffer-dir)))))
            (push (or (gethash file files)
                      (puthash file (typst-overlay--file-hash file) files))
                  hashes)))))
    (if hashes
        (md5 (apply #'concat (nreverse hashes)))
      "")))

(defun typst-overlay--file-hash (file)
  "Return a hash of FILE's contents, or \"none\" if it cannot be read."
  (if (and (file-regular-p file) (file-readable-p file))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (md5 (current-buffer)))
    "none"))

(defun typst-overlay--buffer-dir ()
  "Return the folder of the buffer's file, or for buffers without one, its own.
Relative #import paths are from there."
  (expand-file-name (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory)))

(defun typst-overlay--build-prelude-text (code-nodes)
  "Return the full prelude source for CODE-NODES."
  (mapconcat #'typst-overlay-code-node-text code-nodes "\n\n"))

(cl-defstruct typst-overlay-snapshot
  elements       ;; ordered list of typst-overlay-element
  code-nodes
  math-nodes)

(defun typst-overlay--collect-prelude-code-nodes (math-node code-nodes)
  "Return the CODE-NODES that are in MATH-NODE's prelude.
CODE-NODES are those that satisfy `typst-overlay--prelude-code-node-p'.
One contributes to the prelude if:
- it appears before the math node in the buffer, and
- the math node is within its scope, if it has one.

The result is returned in document order."
  (let ((math-beg (typst-overlay-math-node-beg math-node))
        result)
    (dolist (code-node code-nodes)
      (when (and (< (typst-overlay-code-node-beg code-node) math-beg)
                 (let ((scope (typst-overlay-code-node-scope code-node)))
                   (or (null scope) (< math-beg (cdr scope)))))
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
         ;; Once, rather than for every math node.
         (prelude-code-nodes (seq-filter #'typst-overlay--prelude-code-node-p
                                         code-nodes))
         (preludes (typst-overlay--prelude-table))
         elements)
    (dolist (math-node math-nodes)
      (let* ((prelude-nodes
              (typst-overlay--collect-prelude-code-nodes
               math-node prelude-code-nodes))
             (element
              (typst-overlay--make-element
               math-node prelude-nodes preludes)))
        (push element elements)))
    (make-typst-overlay-snapshot
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

(defun typst-overlay--artifact-live-p (artifact)
  "Return non-nil if ARTIFACT's SVG is still on disk.
The cache folder may be deleted while overlays are shown."
  (and artifact
       (file-exists-p (typst-overlay-artifact-svg-path artifact))))

(defun typst-overlay--artifact-reusable-p (record element artifact)
  "Return non-nil if ARTIFACT, RECORD's, can be shown for ELEMENT.
It can if RECORD's overlay already shows it where ELEMENT is, as when
text typed before ELEMENT moved it, or else if its SVG is on disk.
The overlay is checked first: after an edit, every equation after it
has moved, and checking the disk for each of them is slow."
  (or (and record (typst-overlay--overlay-shows-p record element artifact))
      (typst-overlay--artifact-live-p artifact)))

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
  (let ((cache-dir (typst-overlay--cache-dir)))
    (when (file-directory-p cache-dir)
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
           ;; Overlay first: checking the disk for every equation is slow.
           (when (and (null overlay) (typst-overlay--artifact-live-p artifact))
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
           (when (typst-overlay--artifact-reusable-p record new-element artifact)
             (push (make-typst-overlay-place-op
                    :old old-element
                    :new new-element
                    :artifact artifact)
                   ops))))

        ('added
         (let* ((new-element (typst-overlay-diff-entry-new entry))
                (cache-key (typst-overlay-element-cache-key new-element))
                (artifact (gethash cache-key artifact-cache)))
           (when (typst-overlay--artifact-live-p artifact)
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
                      (not (typst-overlay--artifact-live-p
                            (typst-overlay-record-artifact record))))
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
           (unless (typst-overlay--artifact-reusable-p record new-element artifact)
             (push (make-typst-overlay-render-op
                    :old old-element
                    :new new-element)
                   ops))))

        ('added
         (let* ((new-element (typst-overlay-diff-entry-new entry))
                (cache-key (typst-overlay-element-cache-key new-element))
                (artifact (gethash cache-key artifact-cache)))
           (unless (typst-overlay--artifact-live-p artifact)
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
  (let* ((generation (typst-overlay-render-plan-generation plan))
         (place-ops (typst-overlay-render-plan-place plan))
         (render-ops (typst-overlay-render-plan-render plan))
         ;; Take out every old record before storing any new one: an
         ;; element may move to where another one was, as when two are
         ;; swapped, and would overwrite its record.
         (place-records (mapcar (lambda (op)
                                  (typst-overlay--take-record
                                   registry (typst-overlay-place-op-old op)))
                                place-ops))
         (render-records (mapcar (lambda (op)
                                   (typst-overlay--take-record
                                    registry (typst-overlay-render-op-old op)))
                                 render-ops)))
    (dolist (op (typst-overlay-render-plan-delete plan))
      (typst-overlay--apply-delete-op op registry))
    (cl-mapc (lambda (op record)
               (typst-overlay--apply-place-op op record registry generation))
             place-ops place-records)
    (typst-overlay--start-renders
     (cl-mapcar (lambda (op record)
                  (typst-overlay--apply-render-op op record registry generation))
                render-ops render-records)
     generation artifact-cache)
    (setf (typst-overlay-registry-generation registry) generation)))

(defun typst-overlay--take-record (registry element)
  "Remove the record for ELEMENT from REGISTRY and return it.
Its overlay is kept, for the op applied to it to reuse or delete.
Return nil if ELEMENT is nil or has no record."
  (let ((record (and element (typst-overlay--get-record registry element))))
    (when record
      (typst-overlay--remove-record registry element))
    record))

(defun typst-overlay--apply-delete-op (op registry)
  "Apply delete OP by removing overlay and record from REGISTRY."
  (let* ((old-element (typst-overlay-delete-op-old op))
         (record (typst-overlay--get-record registry old-element)))
    (when record
      (typst-overlay--delete-record-overlay record)
      (typst-overlay--remove-record registry old-element))))

(defun typst-overlay--apply-place-op (op record registry generation)
  "Apply place OP by making its new element visible in REGISTRY.
RECORD is the old element's record, already taken out, or nil.
GENERATION stamps the record so stale async results can be ignored.

An equation moves when text is typed before it, and its overlay moves
with it.  So an overlay already showing ARTIFACT where the equation
is now is kept, rather than replaced by reading the SVG again."
  (let ((new-element (typst-overlay-place-op-new op))
        (artifact (typst-overlay-place-op-artifact op)))
    (unless record
      (setq record (make-typst-overlay-record)))
    (unless (typst-overlay--overlay-shows-p record new-element artifact)
      (typst-overlay--delete-record-overlay record)
      (setf (typst-overlay-record-overlay record)
            (typst-overlay--place-artifact-overlay new-element artifact)))
    (setf (typst-overlay-record-element record) new-element
          (typst-overlay-record-state record) 'visible
          (typst-overlay-record-artifact record) artifact
          (typst-overlay-record-generation record) generation)
    (typst-overlay--put-record registry new-element record)))

(defun typst-overlay--overlay-shows-p (record element artifact)
  "Return non-nil if RECORD's overlay shows ARTIFACT over ELEMENT."
  (let ((overlay (typst-overlay-record-overlay record)))
    (and overlay
         (overlay-buffer overlay)
         (overlay-get overlay 'typst-overlay)
         (eq (typst-overlay-record-artifact record) artifact)
         (= (overlay-start overlay) (typst-overlay-element-beg element))
         (= (overlay-end overlay) (typst-overlay-element-end element)))))

(defun typst-overlay--apply-render-op (op record registry generation)
  "Apply render OP by registering its new element in REGISTRY.
RECORD is the old element's record, already taken out, or nil.  It is
stamped with GENERATION.  Return the new element, to be rendered by
`typst-overlay--start-renders'."
  (let ((new-element (typst-overlay-render-op-new op)))
    (if record
        (typst-overlay--delete-record-overlay record)
      (setq record (make-typst-overlay-record)))
    (setf (typst-overlay-record-element record) new-element
          (typst-overlay-record-state record) 'rendering
          (typst-overlay-record-overlay record) nil
          (typst-overlay-record-artifact record) nil
          (typst-overlay-record-generation record) generation)
    (typst-overlay--put-record registry new-element record)
    new-element))

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
  "Hide OVERLAY by clearing its display property.
Its face goes too, so the source keeps its own."
  (when (overlayp overlay)
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'face nil)))

(defun typst-overlay--show-overlay (overlay)
  "Show OVERLAY again using its cached image."
  (when (overlayp overlay)
    (let ((image (overlay-get overlay 'typst-overlay-image)))
      (when image
        (overlay-put overlay 'display image)
        (overlay-put overlay 'face 'typst-overlay-equation)))))

(defun typst-overlay--eldoc (callback &rest _)
  "Document the error of the failed equation at point with CALLBACK.
For `eldoc-documentation-functions'.  Nothing is documented where
Flymake already shows a diagnostic, which Flymake documents itself."
  (unless (and (bound-and-true-p flymake-mode)
               (flymake-diagnostics (point)))
    (when-let* ((overlay (seq-find (lambda (ov) (overlay-get ov 'typst-overlay-error))
                                   (overlays-at (point)))))
      (funcall callback (overlay-get overlay 'typst-overlay-error)
               :thing "Typst" :face 'error))))

(defun typst-overlay--post-command-update ()
  "Hide overlay under point and restore the previously active one."
  (typst-overlay--handle-upward-entry)
  (let ((current (typst-overlay--overlay-at-point))
        (active typst-overlay--active-overlay))
    (unless (eq current active)
      (when (overlayp active)
        (typst-overlay--show-overlay active))
      (when (overlayp current)
        (typst-overlay--hide-overlay current))
      (setq typst-overlay--active-overlay current)))
  (typst-overlay--update-preview))

(defvar-local typst-overlay--preview nil
  "Overlay showing the image of the equation point is in, or nil.
See `typst-overlay-preview'.")

(defun typst-overlay--update-preview ()
  "Show the image of the equation being edited after it.
That is the equation of the hidden overlay at point.  While it is
being edited it has no overlay, until it is rendered again, so the
preview keeps its last image as long as point stays inside it.
Otherwise the preview is removed."
  (let ((active typst-overlay--active-overlay)
        (preview typst-overlay--preview))
    (cond
     ((and typst-overlay-preview (overlayp active) (overlay-buffer active))
      (unless (overlayp preview)
        ;; Like the equation overlays: text typed right before or
        ;; after the equation stays outside it.
        (setq preview (make-overlay (overlay-start active) (overlay-end active)
                                    nil t nil)
              typst-overlay--preview preview)
        (overlay-put preview 'evaporate t))
      ;; An overlay's image and text never change, so there is nothing
      ;; to update while it stays the active one.
      (unless (eq (overlay-get preview 'typst-overlay-source) active)
        (overlay-put preview 'typst-overlay-source active)
        (move-overlay preview (overlay-start active) (overlay-end active))
        (overlay-put preview 'after-string
                     (concat
                      ;; Below an equation that spans lines, else beside it.
                      (if (string-search "\n" (buffer-substring-no-properties
                                               (overlay-start active)
                                               (overlay-end active)))
                          "\n"
                        " ")
                      (propertize " "
                                  'display (append (overlay-get active 'typst-overlay-image)
                                                   ;; Room between it and the box.
                                                   '(:margin (4 . 2)))
                                  'face 'typst-overlay-preview)))))
     ((and (overlayp preview)
           (overlay-buffer preview)
           (<= (overlay-start preview) (point))
           (< (point) (overlay-end preview))))
     (t (typst-overlay--delete-preview)))))

(defun typst-overlay--delete-preview ()
  "Remove the preview, if any."
  (when (overlayp typst-overlay--preview)
    (delete-overlay typst-overlay--preview))
  (setq typst-overlay--preview nil))

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

(defun typst-overlay--read-svg (svg-path)
  "Read SVG-PATH, drawing its black in the color of the text around it.
Emacs paints `currentColor' in the foreground of the text an image is
shown in, so equations follow theme changes without being reread."
  ;; As raw bytes, which is all an image needs, and much faster than
  ;; decoding the file as text.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally svg-path)
    (while (search-forward "#000000" nil t)
      (replace-match "currentColor" t t))
    (buffer-string)))

(defconst typst-overlay--typst-font-size 11.0
  "Typst's default text size in points, which equations are compiled at.")

(defconst typst-overlay--page-margin 1.0
  "Margin around each equation's image, in points.
As set in `typst-overlay--source-header'.")

(defun typst-overlay--create-image (svg-data)
  "Return an image of SVG-DATA sized relative to the surrounding text.
Its height is given in ems, so it follows the font where it is shown,
including `text-scale-adjust'.  SVG-DATA without a height in points
falls back to a fixed `typst-overlay-scale'.  It is aligned as
`typst-overlay--image-ascent' says."
  (if (string-match "<svg[^>]* height=\"\\([0-9.]+\\)pt\"" svg-data)
      (let ((height (string-to-number (match-string 1 svg-data))))
        (create-image svg-data 'svg t
                      :ascent (typst-overlay--image-ascent svg-data height)
                      :height (cons (* typst-overlay-scale
                                       (/ height typst-overlay--typst-font-size))
                                    'em)))
    (create-image svg-data 'svg t :ascent 'center :scale typst-overlay-scale)))

(defun typst-overlay--image-ascent (svg-data height)
  "Return the `:ascent' of an image of SVG-DATA, HEIGHT points high.
An inline equation sits on the baseline of the text around it: its
SVG says how far it reaches below the baseline, so the percentage of
the image above it is known.  Other images, such as display equations
or those cached before this was written to the SVG, are centered."
  (if (and (> height 0)
           (string-match "href=\"typst-overlay-depth:\\([-0-9.e]+\\)\"" svg-data))
      (let ((depth (+ (string-to-number (match-string 1 svg-data))
                      typst-overlay--page-margin)))
        (max 0 (min 100 (round (* 100 (/ (- height depth) height))))))
    'center))

(defun typst-overlay--place-overlay-from-svg (beg end svg-path)
  "Create and return an overlay from BEG to END displaying SVG-PATH."
  (typst-overlay--mark-used svg-path)
  (let* ((svg-data (typst-overlay--read-svg svg-path))
         (image (typst-overlay--create-image svg-data))
         (overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'display image)
    (overlay-put overlay 'face 'typst-overlay-equation)
    (overlay-put overlay 'typst-overlay t)
    (overlay-put overlay 'typst-overlay-image image)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks '(typst-overlay--invalidate-overlay))
    (when (and (>= (point) beg) (<= (point) end))
      (typst-overlay--hide-overlay overlay)
      (setq typst-overlay--active-overlay overlay)
      (typst-overlay--update-preview))
    overlay))

;; Watched rather than given a Customize setter, so `setq' resizes
;; the equations too.
(defvar typst-overlay--resize-timer nil
  "Timer of the pending `typst-overlay--resize-images', or nil.")

(defun typst-overlay--scale-changed (&rest _)
  "Resize the equations shown, after `typst-overlay-scale' changes.
Watchers run before the new value is set, so this waits for it.  One
resize covers any changes made until then."
  (unless typst-overlay--resize-timer
    (setq typst-overlay--resize-timer
          (run-at-time 0 nil #'typst-overlay--resize-images))))

(add-variable-watcher 'typst-overlay-scale #'typst-overlay--scale-changed)

(defun typst-overlay--resize-images ()
  "Create the images of every buffer's equations again at their scale.
Each image keeps its SVG data, so nothing is read or compiled."
  (setq typst-overlay--resize-timer nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when typst-overlay-mode
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when-let* ((image (overlay-get overlay 'typst-overlay-image)))
            (let ((resized (typst-overlay--create-image (plist-get (cdr image) :data))))
              (overlay-put overlay 'typst-overlay-image resized)
              ;; Hidden while point is in it.
              (when (overlay-get overlay 'display)
                (overlay-put overlay 'display resized)))))
        (when typst-overlay--preview
          (typst-overlay--delete-preview)
          (typst-overlay--update-preview))))))

(defun typst-overlay--teardown ()
  "Remove overlays and clear buffer-local runtime state."
  (setq typst-overlay--compile-queue nil
        typst-overlay--active-compiles 0)
  (cl-incf typst-overlay--session)
  (when (overlayp typst-overlay--active-overlay)
    (typst-overlay--show-overlay typst-overlay--active-overlay))
  (setq typst-overlay--active-overlay nil)
  (typst-overlay--delete-preview)
  (when typst-overlay--registry
    (maphash
     (lambda (_key record)
       (typst-overlay--delete-record-overlay record))
     (typst-overlay-registry-records typst-overlay--registry)))
  (setq typst-overlay--snapshot nil
        typst-overlay--registry nil
        typst-overlay--artifact-cache nil))

;; batch compile
;;
;; Starting `typst' takes a few milliseconds, or most of a second if
;; it looks through the system's fonts (see
;; `typst-overlay-system-fonts'), and compiling an equation takes less
;; than a millisecond.  So the equations to render are compiled
;; in batches: one Typst document per batch, one page per equation,
;; written to one SVG per page.  Each equation sits in its own #[...]
;; block, so its prelude does not leak into the others.
;;
;; When a batch fails, `typst' reports only its first error and
;; writes no pages.  The error's line tells which equation failed, or
;; which prelude, shared by every equation that uses it.  Those are
;; marked failed and the rest is compiled again.  An error that cannot
;; be traced falls back to compiling each equation on its own.

(defconst typst-overlay--source-header
  (concat "#set page(width: auto, height: auto, margin: 1pt, fill: none)\n"
          "#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n"
          "#set text(fill: rgb(\"#000000\"))\n"
          ;; How far an inline equation reaches below the baseline,
          ;; written to the SVG as a link on an empty box, which is
          ;; not drawn.  See `typst-overlay--image-ascent'.
          "#let typst-overlay-depth(body) = context {\n"
          "  if body.func() == math.equation and not body.block {\n"
          "    let depth = (measure(body).height\n"
          "      - measure(text(bottom-edge: \"baseline\", body)).height)\n"
          "    place(link(\"typst-overlay-depth:\" + str(depth.pt()), box()))\n"
          "  }\n"
          "}\n")
  "Typst source that starts every batch.")

(cl-defstruct typst-overlay-job
  element      ;; typst-overlay-element to render
  generation   ;; generation of the refresh that started it
  svg-path     ;; where its SVG goes in the cache
  occurrences) ;; (ELEMENT . MARKERS) for each equation it renders

(cl-defstruct typst-overlay-job-lines
  beg          ;; first line of the job's block
  prelude-end  ;; last line of its prelude, or BEG if it has none
  end)         ;; last line of its block

(defun typst-overlay--job-block (element &optional dir root)
  "Return (BLOCK . PRELUDE-LINES) for ELEMENT.
BLOCK is a Typst block rendering ELEMENT with its prelude, ending in
a newline.  PRELUDE-LINES is how many lines its prelude takes.

If DIR and ROOT are given, the relative paths the prelude imports
from DIR are made absolute in ROOT, see `typst-overlay--root-paths'."
  (let* ((prelude (concat
                   (unless (string-empty-p typst-overlay-extra-prelude)
                     (concat typst-overlay-extra-prelude "\n"))
                   (let ((text (typst-overlay-element-prelude-text element)))
                     (unless (string-empty-p text)
                       (concat text "\n")))))
         (prelude (if (and dir root)
                      (typst-overlay--root-paths prelude dir root)
                    prelude)))
    (cons (concat "#[\n"
                  prelude
                  ;; Number each equation as if compiled on its own.
                  "#counter(math.equation).update(0)\n"
                  ;; On the same lines as the equation, so line
                  ;; numbers in its errors still point at it.
                  "#let typst-overlay-math = [" (typst-overlay-element-text element) "]\n"
                  "#typst-overlay-math\n"
                  "#typst-overlay-depth(typst-overlay-math)\n"
                  "]\n")
          (cl-count ?\n prelude))))

(defun typst-overlay--batch-source (jobs &optional dir root)
  "Return (SOURCE . LINES) for compiling JOBS as one document.
LINES has a `typst-overlay-job-lines' per job, in order, telling
which lines of SOURCE belong to it.  DIR and ROOT are passed on to
`typst-overlay--job-block'."
  (let ((parts (list typst-overlay--source-header))
        (line (1+ (cl-count ?\n typst-overlay--source-header)))
        lines)
    (dolist (job jobs)
      (unless (eq job (car jobs))
        (push "#pagebreak()\n" parts)
        (cl-incf line))
      (pcase-let ((`(,block . ,prelude-lines)
                   (typst-overlay--job-block (typst-overlay-job-element job)
                                             dir root)))
        (push block parts)
        (push (make-typst-overlay-job-lines
               :beg line
               :prelude-end (+ line prelude-lines)
               :end (+ line (cl-count ?\n block) -1))
              lines)
        (cl-incf line (cl-count ?\n block))))
    (cons (apply #'concat (nreverse parts)) (nreverse lines))))

(defun typst-overlay--project-root (dir)
  "Return the root of the project DIR is in, or DIR outside a project.
Typst may only read files in the root, so equations can import files
above DIR in their project, as in #import \"../defs.typ\"."
  (let ((root (when-let* ((project (project-current nil dir)))
                (file-name-as-directory (expand-file-name (project-root project))))))
    (if (and root (string-prefix-p root dir))
        root
      dir)))

(defun typst-overlay--root-paths (prelude dir root)
  "Return PRELUDE with its relative paths from DIR made absolute in ROOT.
Typst reads the source from stdin as if it were a file in ROOT, so a
relative path in #import \"defs.typ\" would be looked up in ROOT, not
in DIR next to the buffer's file.  Written from ROOT, as in
#import \"/sub/defs.typ\", it is found wherever DIR is.  Paths of
#import and #include are rewritten; package and absolute paths are
left alone, as are those outside ROOT, which typst then reports."
  (if (string= dir root)
      prelude
    (with-temp-buffer
      (insert prelude)
      (goto-char (point-min))
      (while (re-search-forward typst-overlay--import-regexp nil t)
        (let* ((written (match-string 1))
               (path (save-match-data (expand-file-name written dir))))
          (when (and (not (string-match-p "\\`[/@]" written))
                     (string-prefix-p root path))
            (replace-match (concat "/" (file-relative-name path root)) t t nil 1))))
      (buffer-string))))

(defun typst-overlay--compile-errors (output)
  "Return (LINE . MESSAGE) for each error in typst OUTPUT, in order.
OUTPUT is in typst's short diagnostic format.  Only errors in the
compiled source itself, not in imported files, have a location."
  (let ((start 0) errors)
    (while (string-match "^<stdin>:\\([0-9]+\\):[0-9]+: error: \\(.*\\)$"
                         output start)
      (push (cons (string-to-number (match-string 1 output))
                  (match-string 2 output))
            errors)
      (setq start (match-end 0)))
    (nreverse errors)))

(defun typst-overlay--job-error (errors job-lines)
  "Return the message of the last of ERRORS within JOB-LINES, or nil.
A syntax error is reported from the outside in, starting with the
unclosed delimiters around it, so the last one is the actual cause."
  (cdr (car (last (seq-filter
                   (lambda (error)
                     (<= (typst-overlay-job-lines-beg job-lines)
                         (car error)
                         (typst-overlay-job-lines-end job-lines)))
                   errors)))))

(defun typst-overlay--failed-jobs (jobs lines line)
  "Return the JOBS that fail because of an error at LINE, or nil.
LINES are the jobs' `typst-overlay-job-lines'.  An error in a job's
prelude fails every job with the same prelude."
  (let ((index (cl-position-if
                (lambda (job-lines)
                  (<= (typst-overlay-job-lines-beg job-lines)
                      line
                      (typst-overlay-job-lines-end job-lines)))
                lines)))
    (when index
      (let* ((job (nth index jobs))
             (prelude-hash (typst-overlay-element-prelude-hash
                            (typst-overlay-job-element job))))
        ;; The prelude starts after the block's opening "#[" line; an
        ;; error on that line itself comes from the job's own content.
        (if (< (typst-overlay-job-lines-beg (nth index lines))
               line
               (1+ (typst-overlay-job-lines-prelude-end (nth index lines))))
            (seq-filter (lambda (other)
                          (equal (typst-overlay-element-prelude-hash
                                  (typst-overlay-job-element other))
                                 prelude-hash))
                        jobs)
          (list job))))))

(defun typst-overlay--batch-pages (dir)
  "Return an alist of page number to SVG file in DIR."
  (mapcar (lambda (file)
            (cons (string-to-number (file-name-base file)) file))
          (directory-files dir t "\\`[0-9]+\\.svg\\'")))

(defun typst-overlay--max-active-compiles ()
  "Return `typst-overlay-max-active-compiles', but at least 1.
With 0, nothing would ever be compiled."
  (max 1 typst-overlay-max-active-compiles))

(defun typst-overlay--drain-compile-queue ()
  "Start queued batches until the concurrency limit is reached."
  (while (and typst-overlay--compile-queue
              (< typst-overlay--active-compiles (typst-overlay--max-active-compiles)))
    (let ((thunk (pop typst-overlay--compile-queue)))
      (funcall thunk))))

(defun typst-overlay--start-renders (elements generation artifact-cache)
  "Start async renders for ELEMENTS at GENERATION.
Elements with the same cache key, such as an $x$ written many times,
share one job, whose image is shown for all of them.  Jobs are spread
evenly over at most `typst-overlay-max-active-compiles' batches,
which run in parallel.  ARTIFACT-CACHE receives the results."
  (when elements
    (let* ((jobs (mapcar
                  (lambda (group)
                    (make-typst-overlay-job
                     :element (car group)
                     :generation generation
                     :svg-path (typst-overlay--artifact-svg-path
                                (typst-overlay-element-cache-key (car group)))
                     ;; Markers follow edits made while compiling, so the
                     ;; result can be placed where the equation is by then.
                     :occurrences (mapcar (lambda (element)
                                            (cons element
                                                  (typst-overlay--element-markers element)))
                                          group)))
                  (typst-overlay--group-by-cache-key elements)))
           (size (ceiling (length jobs)
                          (float (typst-overlay--max-active-compiles)))))
      (dolist (batch (seq-split jobs size))
        (typst-overlay--queue-batch batch artifact-cache)))))

(defun typst-overlay--group-by-cache-key (elements)
  "Return ELEMENTS grouped by cache key, as a list of lists.
Groups are ordered by their first element, and keep ELEMENTS' order."
  (let ((groups (make-hash-table :test #'equal))
        keys)
    (dolist (element elements)
      (let ((key (typst-overlay-element-cache-key element)))
        (unless (gethash key groups)
          (push key keys))
        (push element (gethash key groups))))
    (mapcar (lambda (key) (nreverse (gethash key groups)))
            (nreverse keys))))

(defun typst-overlay--queue-batch (jobs artifact-cache)
  "Compile JOBS as one batch, now or once a compile slot is free.
ARTIFACT-CACHE receives the results."
  (let* ((buffer (current-buffer))
         (session typst-overlay--session)
         (start (lambda ()
                  (cl-incf typst-overlay--active-compiles)
                  (typst-overlay--run-batch jobs artifact-cache buffer session))))
    (if (< typst-overlay--active-compiles (typst-overlay--max-active-compiles))
        (funcall start)
      (setq typst-overlay--compile-queue
            (nconc typst-overlay--compile-queue (list start))))))

(defun typst-overlay--run-batch (jobs artifact-cache buffer session)
  "Compile JOBS of BUFFER into a temporary folder in the cache.
ARTIFACT-CACHE receives the results.  SESSION is the
`typst-overlay--session' the batch belongs to."
  (let* (;; #import paths are relative to the buffer's file, or for
         ;; buffers without one, its directory.
         (file-dir (with-current-buffer buffer
                     (typst-overlay--buffer-dir)))
         (root (typst-overlay--project-root file-dir))
         (source (typst-overlay--batch-source jobs file-dir root))
         (cache-dir (file-name-directory
                     (typst-overlay-job-svg-path (car jobs))))
         (dir (make-temp-file (expand-file-name "batch-" cache-dir) t))
         ;; Typst takes the folder it runs in as the root of a source
         ;; read from stdin.
         (default-directory root))
    (typst-overlay--compile-async
     (car source) dir
     (lambda (ok output)
       (unwind-protect
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (typst-overlay--finish-batch
                jobs (cdr source) dir ok output artifact-cache session)))
         (delete-directory dir t))))))

(defun typst-overlay--font-args ()
  "Return the arguments telling typst which fonts it may use.
See `typst-overlay-system-fonts' and `typst-overlay-font-paths'."
  (append (unless typst-overlay-system-fonts
            '("--ignore-system-fonts"))
          (mapcan (lambda (dir) (list "--font-path" (expand-file-name dir)))
                  typst-overlay-font-paths)))

(defun typst-overlay--compile-async (source dir callback)
  "Compile SOURCE asynchronously to one SVG per page in DIR.
Pages are written as 1.svg, 2.svg, and so on.  CALLBACK receives
whether the compile succeeded and typst's output."
  (let* ((buffer (generate-new-buffer " *typst-overlay-compile*" t))
         (proc (make-process
                :name "typst-overlay-compile"
                :buffer buffer
                :command `("typst" "compile" "--diagnostic-format" "short"
                           "--format" "svg" ,@(typst-overlay--font-args)
                           "-" ,(expand-file-name "{p}.svg" dir))
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (unwind-protect
                        (funcall callback
                                 (= (process-exit-status proc) 0)
                                 (with-current-buffer (process-buffer proc)
                                   (buffer-string)))
                      (when (buffer-live-p (process-buffer proc))
                        (kill-buffer (process-buffer proc)))))))))
    (process-send-string proc source)
    (process-send-eof proc)
    proc))

(defun typst-overlay--finish-batch (jobs lines dir ok output artifact-cache session)
  "Handle a finished batch of JOBS, with LINES from its source.
DIR holds its pages, OK and OUTPUT are the compile result.
ARTIFACT-CACHE receives the results.  Batches from before a teardown,
whose SESSION is outdated, are ignored."
  (if (/= session typst-overlay--session)
      (dolist (job jobs)
        (typst-overlay--release-job job))
    (cl-decf typst-overlay--active-compiles)
    (typst-overlay--warn-unknown-fonts output)
    (let* ((pages (typst-overlay--batch-pages dir))
           (errors (and (not ok) (typst-overlay--compile-errors output)))
           (location (car errors))
           failed)
      (cond
       ;; Success: each page goes to its job's cache file.
       ((and ok (= (length pages) (length jobs)))
        (cl-loop for job in jobs
                 for page from 1
                 do (rename-file (alist-get page pages)
                                 (typst-overlay-job-svg-path job) t)
                    (typst-overlay--finish-job job artifact-cache nil)))
       ;; A traceable error: fail those jobs, compile the rest again.
       ((and location
             (setq failed (typst-overlay--failed-jobs
                           jobs lines (car location))))
        (dolist (job failed)
          (typst-overlay--finish-job
           job artifact-cache
           (or (typst-overlay--job-error
                errors (nth (cl-position job jobs) lines))
               (cdr location))))
        (let ((rest (seq-difference jobs failed #'eq)))
          (when rest
            (typst-overlay--queue-batch rest artifact-cache))))
       ;; Anything else: compile each equation on its own.
       ((cdr jobs)
        (dolist (job jobs)
          (typst-overlay--queue-batch (list job) artifact-cache)))
       (t
        (typst-overlay--finish-job
         (car jobs) artifact-cache
         (or (typst-overlay--job-error errors (car lines))
             (typst-overlay--compile-error-summary output))))))
    (typst-overlay--drain-compile-queue)
    (typst-overlay--flymake-report)))

(defvar typst-overlay--warned-fonts nil
  "Fonts already warned about by `typst-overlay--warn-unknown-fonts'.")

(defun typst-overlay--warn-unknown-fonts (output)
  "Warn about each font typst OUTPUT says it could not find.
Typst then draws the equation in its default font, which is easy to
miss, so the warning says how to let typst find the font.  Each font
is warned about once."
  (unless typst-overlay-system-fonts
    (let ((start 0))
      (while (string-match "warning: unknown font family: \\(.*\\)$" output start)
        (let ((font (match-string 1 output)))
          (setq start (match-end 0))
          (unless (member font typst-overlay--warned-fonts)
            (push font typst-overlay--warned-fonts)
            (display-warning
             'typst-overlay
             (format-message
              "Typst did not find font \"%s\".  Add its folder to `typst-overlay-font-paths', or set `typst-overlay-system-fonts' to t"
              font))))))))

(defun typst-overlay--release-job (job)
  "Release JOB's markers."
  (pcase-dolist (`(,_ . ,markers) (typst-overlay-job-occurrences job))
    (set-marker (car markers) nil)
    (set-marker (cdr markers) nil)))

(defun typst-overlay--finish-job (job artifact-cache error)
  "Show the result of JOB for each of its equations: its SVG, or ERROR.
ERROR is nil for a successful result, which ARTIFACT-CACHE receives."
  (pcase-dolist (`(,element . ,markers) (typst-overlay-job-occurrences job))
    (let ((current (typst-overlay--relocate-element element markers)))
      (if error
          (typst-overlay--handle-render-failure
           element current (typst-overlay-job-generation job) error)
        (typst-overlay--handle-render-success
         element current (typst-overlay-job-generation job)
         (typst-overlay-element-cache-key element)
         (typst-overlay-job-svg-path job) artifact-cache))))
  (typst-overlay--release-job job))

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
  "Return the message of the first error in typst OUTPUT."
  (let ((lines (split-string output "\n" t "[ \t]+")))
    (or (seq-some (lambda (line)
                    (and (string-match "\\(?:\\`\\|: \\)error: \\(.*\\)" line)
                         (match-string 1 line)))
                  lines)
        (car lines)
        "Compilation failed")))

(defun typst-overlay--handle-render-failure (element current generation message)
  "Mark ELEMENT failed if its record still matches GENERATION.
CURRENT is ELEMENT at its position now, or nil if its text changed
while compiling.  If CURRENT is non-nil, underline it with face
`typst-overlay-error' and attach the error MESSAGE, which is shown
when point enters the equation."
  (when typst-overlay-mode
    (let ((record (typst-overlay--get-record typst-overlay--registry element)))
      (when (and record
                 (= (typst-overlay-record-generation record) generation))
        (typst-overlay--delete-record-overlay record)
        (setf (typst-overlay-record-state record) 'failed
              (typst-overlay-record-overlay record)
              (and current
                   (typst-overlay--place-error-overlay current message)))))))

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

;; flymake
(defvar-local typst-overlay--flymake-report-fn nil
  "Report function from the latest Flymake check, or nil.")

(defun typst-overlay--error-overlays ()
  "Return the overlays of failed equations, in buffer order."
  (sort (seq-filter (lambda (ov) (overlay-get ov 'typst-overlay-error))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun typst-overlay--flymake-report ()
  "Report the failed equations to Flymake, if it has asked for them.
Compiles finish after Flymake's check, so this is called again as
their results come in.  Each report replaces the previous one.

Nothing is reported while the backend is not one of the buffer's
`flymake-diagnostic-functions', as Flymake would still take it."
  (when (and typst-overlay--flymake-report-fn
             (bound-and-true-p flymake-mode)
             (memq #'typst-overlay-flymake flymake-diagnostic-functions))
    (save-restriction
      (widen)
      (funcall typst-overlay--flymake-report-fn
               (mapcar (lambda (overlay)
                         (flymake-make-diagnostic
                          (current-buffer)
                          (overlay-start overlay) (overlay-end overlay)
                          :error
                          (let ((message (overlay-get overlay 'typst-overlay-error)))
                            ;; Flymake 1.4 and later take an origin.
                            (if (fboundp 'flymake-diagnostic-origin)
                                (list "typst-overlay" nil message)
                              message))))
                       (typst-overlay--error-overlays))
               ;; Without a region, Flymake keeps the diagnostics of
               ;; earlier reports in the same check.
               :region (cons (point-min) (point-max))))))

(defun typst-overlay-flymake (report-fn &rest _args)
  "Flymake backend for equations that failed to compile.
`typst-overlay-mode' adds it to `flymake-diagnostic-functions' where
`typst-overlay-use-flymake' applies.  It
reports the errors of the last `typst-overlay-refresh' to REPORT-FN,
and keeps reporting as compiles finish."
  (setq typst-overlay--flymake-report-fn report-fn)
  (typst-overlay--flymake-report))

(defun typst-overlay--add-flymake-backend ()
  "Add `typst-overlay-flymake' to the buffer's Flymake backends.
Only where `typst-overlay-use-flymake' applies.  Eglot replaces the
backends with its own when it starts, and restores those it saved when
it stops, so this also runs from `eglot-managed-mode-hook'."
  (when (and (if (eq typst-overlay-use-flymake 'org)
                 (derived-mode-p 'org-mode)
               typst-overlay-use-flymake)
             (not (memq #'typst-overlay-flymake flymake-diagnostic-functions)))
    (add-hook 'flymake-diagnostic-functions #'typst-overlay-flymake nil t)
    (when (bound-and-true-p flymake-mode)
      (flymake-start))))

;; The backend runs no code from the buffer, it only reports errors
;; found by `typst-overlay-mode', so Flymake may run it in untrusted
;; buffers too.  See `trusted-content'.
(put 'typst-overlay-flymake 'flymake-always-safe t)

(defun typst-overlay--cache-dir ()
  "Return the directory for the current buffer's cached SVG files.
It is in `typst-overlay-cache-directory', named after the folder of
the buffer's file, or for buffers without one, such as org-capture's,
its own folder; see `typst-overlay--cache-dir-name'.

Each folder has its own, so equations whose preludes name files by
relative paths, which may differ between folders, cannot share
images."
  (expand-file-name (typst-overlay--cache-dir-name (typst-overlay--buffer-dir))
                    typst-overlay-cache-directory))

(defun typst-overlay--cache-dir-name (dir)
  "Return the name of the cache folder for the files in DIR.
Like backup files in `backup-directory-alist', it is DIR's path with
each / written as ! and each ! as !!, and a drive such as c: as
!drive_c, as : is not allowed in file names on Windows.  A name longer
than 100 characters is replaced by its hash, so the path stays well
within the length Windows allows."
  (let ((name (replace-regexp-in-string
               "/" "!"
               (replace-regexp-in-string
                "\\`\\([a-zA-Z]\\):" "!drive_\\1"
                (replace-regexp-in-string "!" "!!" dir)))))
    (if (> (length name) 100)
        (md5 name)
      name)))

(defun typst-overlay--artifact-svg-path (cache-key)
  "Return the cached SVG path for CACHE-KEY."
  (let ((cache-dir (typst-overlay--cache-dir)))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    (expand-file-name (concat cache-key ".svg") cache-dir)))

(defvar typst-overlay--used-images (make-hash-table :test #'equal)
  "Cached images marked as used this session, by path.")

(defun typst-overlay--mark-used (svg-path)
  "Mark the cached image SVG-PATH as used now, once a session.
Its modification time is set to now, so it is kept by
`typst-overlay--prune-cache'."
  (unless (gethash svg-path typst-overlay--used-images)
    (puthash svg-path t typst-overlay--used-images)
    (ignore-errors (set-file-times svg-path))))

(defconst typst-overlay--cache-image-regexp "\\`[0-9a-f]\\{32\\}\\.svg\\'"
  "Regexp matching the name of a cached image, its cache key.")

(defvar typst-overlay--last-prune nil
  "When the cache was last pruned, or nil if not yet this session.")

(defun typst-overlay--schedule-prune ()
  "Prune the cache once Emacs is idle, if not done in the last day."
  (when (and typst-overlay-cache-max-age
             (or (null typst-overlay--last-prune)
                 (> (float-time (time-since typst-overlay--last-prune)) 86400)))
    (setq typst-overlay--last-prune (current-time))
    (run-with-idle-timer 5 nil #'typst-overlay--prune-cache)))

(defun typst-overlay--prune-cache ()
  "Delete the cached images not used in `typst-overlay-cache-max-age' days.
Cache folders left empty are deleted too.  Only files and folders
named as the cache names them are touched, so nothing else in
`typst-overlay-cache-directory' is deleted, whatever it is set to."
  (when (and typst-overlay-cache-max-age
             (file-directory-p typst-overlay-cache-directory))
    (let ((limit (time-subtract nil (days-to-time typst-overlay-cache-max-age))))
      (dolist (folder (directory-files typst-overlay-cache-directory t
                                       "\\`\\(?:!\\|[0-9a-f]\\{32\\}\\'\\)"))
        (when (file-directory-p folder)
          (dolist (file (directory-files folder t typst-overlay--cache-image-regexp))
            (when (time-less-p (file-attribute-modification-time (file-attributes file))
                               limit)
              (ignore-errors (delete-file file))))
          (when (directory-empty-p folder)
            (ignore-errors (delete-directory folder))))))))

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
         ;; The whole buffer, even when narrowed, or every equation
         ;; outside the narrowing would lose its overlay.
         (analysis (save-restriction
                     (widen)
                     (funcall typst-overlay--analyzer)))
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
  (typst-overlay--flymake-report)
  t)

;; mode
(defun typst-overlay-save-refresh ()
  "Refresh Typst overlays if `typst-overlay-mode' is active.
Intended for use in `after-save-hook'."
  (when typst-overlay-mode
    (typst-overlay-refresh)))

(defvar-local typst-overlay--idle-timer nil
  "Timer of the pending idle refresh, or nil.")

(defun typst-overlay--schedule-refresh (&rest _)
  "Refresh overlays once Emacs is idle, after an edit.
Waits `typst-overlay-idle-refresh-delay' seconds; for
`after-change-functions'.  An idle timer counts from the last input,
so one scheduled refresh covers the edits typed until then."
  (when (and typst-overlay-idle-refresh-delay
             (not typst-overlay--idle-timer))
    (setq typst-overlay--idle-timer
          (run-with-idle-timer typst-overlay-idle-refresh-delay nil
                               #'typst-overlay--idle-refresh
                               (current-buffer)))))

(defun typst-overlay--idle-refresh (buffer)
  "Refresh the overlays of BUFFER, from its idle timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq typst-overlay--idle-timer nil)
      (when typst-overlay-mode
        (typst-overlay-refresh)))))

(defun typst-overlay--cancel-idle-refresh ()
  "Cancel the pending idle refresh, if any."
  (when typst-overlay--idle-timer
    (cancel-timer typst-overlay--idle-timer)
    (setq typst-overlay--idle-timer nil)))

(defvar-local typst-overlay--last-point nil
  "Tracks previous point location to detect upward cursor movement into overlays.")

(defconst typst-overlay--line-up-commands
  '(previous-line previous-logical-line evil-previous-line evil-previous-visual-line)
  "Commands that move point up by lines.
See `typst-overlay--handle-upward-entry'.")

(defun typst-overlay--handle-upward-entry ()
  "Jump to overlay end if moving up a line into a `typst-overlay'.
A shown image is a single glyph at the start of its equation, so
moving up onto it would put point on the first line of an equation
that spans lines.  Other commands, such as searching or clicking,
put point where they meant to."
  (let ((curr-point (point)))
    (when (and typst-overlay--last-point
               (memq this-command typst-overlay--line-up-commands)
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
   ((not (image-type-available-p 'svg))
    "This Emacs cannot display SVG images; it must be built with SVG support (librsvg)")
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
  (typst-overlay--schedule-prune)
  (setq typst-overlay--last-point (point))
  (add-hook 'post-command-hook #'typst-overlay--post-command-update nil t)
  (add-hook 'after-change-functions #'typst-overlay--schedule-refresh nil t)
  (add-hook 'eglot-managed-mode-hook #'typst-overlay--add-flymake-backend nil t)
  (add-hook 'eldoc-documentation-functions #'typst-overlay--eldoc nil t)
  ;; Org buffers, for one, have no ElDoc function until now.
  (when global-eldoc-mode
    (turn-on-eldoc-mode))
  (typst-overlay-refresh)
  (typst-overlay--add-flymake-backend))

(defun typst-overlay--disable ()
  "Tear down `typst-overlay-mode' in the current buffer."
  (remove-hook 'post-command-hook #'typst-overlay--post-command-update t)
  (remove-hook 'after-change-functions #'typst-overlay--schedule-refresh t)
  (typst-overlay--cancel-idle-refresh)
  (kill-local-variable 'typst-overlay--last-point)
  (typst-overlay--teardown)
  ;; Clear the diagnostics of the overlays just deleted.
  (typst-overlay--flymake-report)
  (remove-hook 'flymake-diagnostic-functions #'typst-overlay-flymake t)
  (remove-hook 'eglot-managed-mode-hook #'typst-overlay--add-flymake-backend t)
  (remove-hook 'eldoc-documentation-functions #'typst-overlay--eldoc t)
  (kill-local-variable 'typst-overlay--flymake-report-fn))

;;;###autoload
(define-minor-mode typst-overlay-mode
  "Render Typst math overlays."
  :lighter " TypstOv"
  (if typst-overlay-mode
      (typst-overlay--enable)
    (typst-overlay--disable)))

(provide 'typst-overlay)

;;; typst-overlay.el ends here
