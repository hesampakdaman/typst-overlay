;;; typst-overlay-test.el --- Tests for typst-overlay -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Table-driven tests.  Detection cases are (NAME INPUT EXPECTED),
;; where EXPECTED is the list of equation texts, in buffer order, that
;; should be detected in INPUT.  Pipeline cases cover how old and new
;; equations are matched (the diff) and whether each one is placed,
;; compiled or left alone (the plan).
;;
;; Run with:
;;
;;   emacs --batch -Q -L . -l test/typst-overlay-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org)
(require 'typst-overlay)

(defun typst-overlay-test--detect (setup input)
  "Insert INPUT, call SETUP, and return the detected equation texts.
Also check that each node's region contains exactly its text."
  (with-temp-buffer
    (insert input)
    (funcall setup)
    (let ((analysis (funcall typst-overlay--analyzer)))
      (mapcar (lambda (node)
                (let ((text (typst-overlay-math-node-text node)))
                  (should (equal (buffer-substring-no-properties
                                  (typst-overlay-math-node-beg node)
                                  (typst-overlay-math-node-end node))
                                 text))
                  text))
              (typst-overlay-analysis-math-nodes analysis)))))

(defun typst-overlay-test--run-cases (setup cases)
  "Check every case in CASES, preparing each buffer with SETUP.
The case name is part of the compared value so failures show it."
  (dolist (case cases)
    (pcase-let ((`(,name ,input ,expected) case))
      (should (equal (list name expected)
                     (list name (typst-overlay-test--detect setup input)))))))

(defun typst-overlay-test--setup-org ()
  "Prepare the current buffer for the org analyzer."
  (org-mode)
  (setq-local typst-overlay--analyzer #'typst-overlay--analyze-org))

(defun typst-overlay-test--setup-typst ()
  "Prepare the current buffer for the Typst analyzer."
  (treesit-parser-create 'typst)
  (setq-local typst-overlay--analyzer #'typst-overlay--analyze-typst))

;;; Org

(defconst typst-overlay-test--org-cases
  '(;; Detected
    ("inline" "The area is $pi r^2$ here." ("$pi r^2$"))
    ("two inline" "$a$ and $b$" ("$a$" "$b$"))
    ("adjacent" "$alpha$$beta$" ("$alpha$" "$beta$"))
    ("display on one line" "Sum: $ x + y $ done." ("$ x + y $"))
    ("display block" "$\n  x = 1\n$" ("$\n  x = 1\n$"))
    ("indented display block" "  $\n  x\n  $" ("$\n  x\n  $"))
    ("display closing on content line" "$\n  x = 1 $" ("$\n  x = 1 $"))
    ("inline wrapped across lines" "so $a +\nb$ holds" ("$a +\nb$"))
    ("escaped backslash before $" "a \\\\$x$ b" ("$x$"))
    ("after stray $ and blank line" "a $ b\n\nlater $z$" ("$z$"))
    ("after heading" "a $ b\n* Heading\nthen $y$" ("$y$"))
    ("nested math (#6)" "a $#table($x$)$ b" ("$#table($x$)$"))
    ("nested two levels" "$#table($#box[$y$]$)$ b" ("$#table($#box[$y$]$)$"))
    ("dollar in a string in code" "$#table(\"a $ b\")$ c" ("$#table(\"a $ b\")$"))
    ("escaped dollar inside math" "$a \\$ b$ c" ("$a \\$ b$"))
    ("half-open interval" "an $[0, 1)$ interval" ("$[0, 1)$"))
    ("variable in math" "$#x$ var" ("$#x$"))
    ("unclosed outer, inner math only" "$#table($x$ unclosed" ("$x$"))
    ;; Not detected
    ("escaped dollars" "costs \\$5 and \\$6" ())
    ("escaped dollars before words" "use \\$a or \\$b" ())
    ("prices" "costs $5 and $10 total" ())
    ("prices with slash" "$5/$10 per month" ())
    ("empty span" "$$" ())
    ("lone dollar" "a $ b" ())
    ("stray $ before heading" "a $ b\n* Heading $\nbody" ())
    ("inline code" "~$a$~ and =$b$=" ())
    ("src block" "#+begin_src sh\necho \"$a\" \"$b\"\n#+end_src" ())
    ("example block" "#+begin_example\n$ x $\n#+end_example" ())
    ("keyword line" "#+title: $x$" ()))
  "Org cases: (NAME INPUT EXPECTED).")

(ert-deftest typst-overlay-test-org-detection ()
  "The org analyzer detects exactly the expected equations."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--run-cases #'typst-overlay-test--setup-org
                                 typst-overlay-test--org-cases))

;;; Typst

(defconst typst-overlay-test--typst-cases
  '(;; Detected
    ("inline and display" "Text $x$ and $ y $." ("$x$" "$ y $"))
    ("inside content block" "#box[inside $c$]\nend $d$" ("$c$" "$d$"))
    ("after code" "#let v = 2\n#box[text]\n$u$" ("$u$"))
    ("before parse error" "Before $p$.\n#let broken = (\nAfter $q$." ("$p$"))
    ("nested math (#6)" "a $#table($x$)$ b" ("$#table($x$)$"))
    ;; Not detected
    ("bound in #let" "#let f = $a$" ())
    ("inside code expression" "#box[#let k = 1 and $z$]" ())
    ("after parse error" "#let h = (\n$e$" ()))
  "Typst cases: (NAME INPUT EXPECTED).")

(ert-deftest typst-overlay-test-typst-detection ()
  "The Typst analyzer detects exactly the expected equations."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--run-cases #'typst-overlay-test--setup-typst
                                 typst-overlay-test--typst-cases))

;;; Requirements

(cl-defun typst-overlay-test--enable-error (has-typst has-grammar
                                                     &optional (has-svg t))
  "Enable the mode with HAS-TYPST, HAS-GRAMMAR and HAS-SVG faked.
Return the `user-error' message and whether the mode stayed on."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) (and has-typst "/usr/bin/typst")))
              ((symbol-function 'treesit-language-available-p)
               (lambda (&rest _) has-grammar))
              ((symbol-function 'image-type-available-p)
               (lambda (&rest _) has-svg)))
      (list (condition-case err
                (progn (typst-overlay-mode 1) nil)
              (user-error (cadr err)))
            typst-overlay-mode))))

(ert-deftest typst-overlay-test-missing-svg-support ()
  "Without SVG support in Emacs the mode refuses to turn on."
  (pcase-let ((`(,message ,mode) (typst-overlay-test--enable-error t t nil)))
    (should (string-match-p "SVG" message))
    (should-not mode)))

(ert-deftest typst-overlay-test-missing-typst ()
  "Without the typst binary the mode refuses to turn on."
  (pcase-let ((`(,message ,mode) (typst-overlay-test--enable-error nil t)))
    (should (string-match-p "typst not found" message))
    (should-not mode)))

(ert-deftest typst-overlay-test-missing-grammar ()
  "Without the Typst grammar the mode refuses to turn on."
  (pcase-let ((`(,message ,mode) (typst-overlay-test--enable-error t nil)))
    (should (string-match-p "grammar not found" message))
    (should (string-match-p (regexp-quote typst-overlay--grammar-url) message))
    (should-not mode)))

;;; Pipeline

;; Elements are written as (BEG TEXT); old elements in plan cases as
;; (BEG TEXT STATE), where STATE is the record state before refresh.
;; Results are summarized by element start positions.

(defun typst-overlay-test--element (beg text)
  "Return an element for TEXT starting at BEG, with no prelude."
  (typst-overlay--make-element
   (make-typst-overlay-math-node :beg beg
                                 :end (+ beg (length text))
                                 :text text
                                 :text-hash (md5 text))
   nil))

(defun typst-overlay-test--snapshot (specs)
  "Return a snapshot of elements built from SPECS, a list of (BEG TEXT)."
  (make-typst-overlay-snapshot
   :elements (mapcar (lambda (spec)
                       (typst-overlay-test--element (nth 0 spec) (nth 1 spec)))
                     specs)))

(defun typst-overlay-test--beg (element)
  "Return the start of ELEMENT, or nil if ELEMENT is nil."
  (and element (typst-overlay-element-beg element)))

(defun typst-overlay-test--summarize-diff (diff)
  "Summarize DIFF as (:entries ((STATUS OLD-BEG NEW-BEG) ...) :deleted BEGS)."
  (list :entries (mapcar (lambda (entry)
                           (list (typst-overlay-diff-entry-status entry)
                                 (typst-overlay-test--beg
                                  (typst-overlay-diff-entry-old entry))
                                 (typst-overlay-test--beg
                                  (typst-overlay-diff-entry-new entry))))
                         (typst-overlay-diff-entries diff))
        :deleted (mapcar #'typst-overlay-test--beg
                         (typst-overlay-diff-deleted diff))))

(defconst typst-overlay-test--diff-cases
  '(("nothing changed"
     ((1 "$a$") (10 "$b$")) ((1 "$a$") (10 "$b$"))
     (:entries ((unchanged 1 1) (unchanged 10 10)) :deleted ()))
    ("first equation"
     () ((1 "$a$"))
     (:entries ((added nil 1)) :deleted ()))
    ("one edited"
     ((1 "$a$") (10 "$b$")) ((1 "$a$") (10 "$c$"))
     (:entries ((unchanged 1 1) (added nil 10)) :deleted (10)))
    ("shifted by text above"
     ((1 "$a$") (10 "$b$")) ((1 "$a$") (15 "$b$"))
     (:entries ((unchanged 1 1) (moved 10 15)) :deleted ()))
    ("one deleted"
     ((1 "$a$") (10 "$b$")) ((1 "$a$"))
     (:entries ((unchanged 1 1)) :deleted (10)))
    ("swapped"
     ((1 "$a$") (10 "$b$")) ((1 "$b$") (10 "$a$"))
     (:entries ((moved 10 1) (moved 1 10)) :deleted ()))
    ("duplicate, second removed"
     ((1 "$x$") (10 "$x$")) ((1 "$x$"))
     (:entries ((unchanged 1 1)) :deleted (10)))
    ("duplicate, matched in order"
     ((1 "$x$") (10 "$x$")) ((5 "$x$"))
     (:entries ((moved 1 5)) :deleted (10))))
  "Diff cases: (NAME OLD NEW EXPECTED).")

(ert-deftest typst-overlay-test-diff ()
  "Old and new elements are matched as expected."
  (dolist (case typst-overlay-test--diff-cases)
    (pcase-let ((`(,name ,old ,new ,expected) case))
      (should (equal (list name expected)
                     (list name
                           (typst-overlay-test--summarize-diff
                            (typst-overlay--diff-snapshots
                             (typst-overlay-test--snapshot old)
                             (typst-overlay-test--snapshot new)))))))))

(defun typst-overlay-test--artifact (element)
  "Return a fake artifact for ELEMENT."
  (make-typst-overlay-artifact
   :cache-key (typst-overlay-element-cache-key element)
   :svg-path "unused.svg"))

(defun typst-overlay-test--registry (old)
  "Return a registry with a record for each (BEG TEXT STATE) in OLD.
Records that are `visible' or `stale' have an artifact; only
`visible' ones have an overlay, represented by a placeholder.  The
pseudo-state `stale-no-artifact' is a `stale' record without one."
  (let ((registry (typst-overlay--make-registry)))
    (dolist (spec old)
      (pcase-let* ((`(,beg ,text ,state) spec)
                   (element (typst-overlay-test--element beg text)))
        (typst-overlay--put-record
         registry element
         (make-typst-overlay-record
          :element element
          :state (if (eq state 'stale-no-artifact) 'stale state)
          :overlay (and (eq state 'visible) 'overlay)
          :artifact (and (memq state '(visible stale))
                         (typst-overlay-test--artifact element))
          :generation 1))))
    registry))

(defun typst-overlay-test--artifact-cache (texts)
  "Return an artifact cache holding an artifact for each of TEXTS."
  (let ((cache (typst-overlay--make-artifact-cache)))
    (dolist (text texts)
      (let ((artifact (typst-overlay-test--artifact
                       (typst-overlay-test--element 1 text))))
        (puthash (typst-overlay-artifact-cache-key artifact) artifact cache)))
    cache))

(defun typst-overlay-test--summarize-plan (plan)
  "Summarize PLAN as (:delete OLD-BEGS :place NEW-BEGS :render NEW-BEGS)."
  (list :delete (mapcar (lambda (op)
                          (typst-overlay-test--beg (typst-overlay-delete-op-old op)))
                        (typst-overlay-render-plan-delete plan))
        :place (mapcar (lambda (op)
                         (typst-overlay-test--beg (typst-overlay-place-op-new op)))
                       (typst-overlay-render-plan-place plan))
        :render (mapcar (lambda (op)
                          (typst-overlay-test--beg (typst-overlay-render-op-new op)))
                        (typst-overlay-render-plan-render plan))))

(defconst typst-overlay-test--plan-cases
  '(("visible and unchanged: nothing to do"
     ((1 "$a$" visible)) () ((1 "$a$"))
     (:delete () :place () :render ()))
    ("new, in cache: placed without compiling"
     () ("$a$") ((1 "$a$"))
     (:delete () :place (1) :render ()))
    ("new, not in cache: compiled"
     () () ((1 "$a$"))
     (:delete () :place () :render (1)))
    ("moved: image reused"
     ((1 "$a$" visible)) () ((5 "$a$"))
     (:delete () :place (5) :render ()))
    ("moved while compiling: compiled again"
     ((1 "$a$" rendering)) () ((5 "$a$"))
     (:delete () :place () :render (5)))
    ("edited: old removed, new compiled"
     ((1 "$a$" visible)) () ((1 "$b$"))
     (:delete (1) :place () :render (1)))
    ("stale with image: image reused"
     ((1 "$a$" stale)) () ((1 "$a$"))
     (:delete () :place (1) :render ()))
    ("stale without image: compiled"
     ((1 "$a$" stale-no-artifact)) () ((1 "$a$"))
     (:delete () :place () :render (1)))
    ("failed and unchanged: not retried"
     ((1 "$a$" failed)) () ((1 "$a$"))
     (:delete () :place () :render ())))
  "Plan cases: (NAME OLD CACHED NEW EXPECTED).
OLD is the previous elements with their record states, CACHED the
texts with an artifact in the cache, and NEW the current elements.")

(ert-deftest typst-overlay-test-plan ()
  "Each element is placed, compiled or left alone as expected."
  (dolist (case typst-overlay-test--plan-cases)
    (pcase-let ((`(,name ,old ,cached ,new ,expected) case))
      (let ((diff (typst-overlay--diff-snapshots
                   (typst-overlay-test--snapshot
                    (mapcar (lambda (spec) (list (nth 0 spec) (nth 1 spec))) old))
                   (typst-overlay-test--snapshot new))))
        (should (equal (list name expected)
                       (list name
                             (typst-overlay-test--summarize-plan
                              (typst-overlay--plan-render
                               diff
                               (typst-overlay-test--registry old)
                               (typst-overlay-test--artifact-cache cached)
                               2)))))))))

;;; Async placement

;; While an equation compiles, the buffer may be edited.  Each case is
;; (NAME EDIT EXPECTED): EDIT is (insert POS STRING), (delete BEG END)
;; or nil, applied to "Text $a + b$ end." after the render started.
;; EXPECTED is where the result is placed, (BEG . END), or nil if the
;; equation's text changed.

(defconst typst-overlay-test--relocate-cases
  '(("no edit" nil (6 . 13))
    ("typed before the line" (insert 1 "INSERTED ") (15 . 22))
    ("typed right before $" (insert 6 "XX") (8 . 15))
    ("typed right after $" (insert 13 "YY") (6 . 13))
    ("typed after the equation" (insert 17 "!") (6 . 13))
    ("deleted text before" (delete 1 6) (1 . 8))
    ("edited inside" (insert 12 " + c") nil)
    ("deleted the equation" (delete 6 13) nil))
  "Relocation cases: (NAME EDIT EXPECTED).")

(ert-deftest typst-overlay-test-relocate ()
  "A finished render is placed where its equation is now."
  (dolist (case typst-overlay-test--relocate-cases)
    (pcase-let ((`(,name ,edit ,expected) case))
      (with-temp-buffer
        (insert "Text $a + b$ end.")
        (let* ((element (typst-overlay-test--element 6 "$a + b$"))
               (markers (typst-overlay--element-markers element)))
          (pcase edit
            (`(insert ,pos ,string) (goto-char pos) (insert string))
            (`(delete ,beg ,end) (delete-region beg end)))
          (let ((current (typst-overlay--relocate-element element markers)))
            (should (equal (list name expected)
                           (list name
                                 (and current
                                      (cons (typst-overlay-element-beg current)
                                            (typst-overlay-element-end current))))))))))))

;;; Compile errors

(defconst typst-overlay-test--error-summary-cases
  '(("real typst output"
     "error: unknown variable: ac\n  ┌─ <stdin>:4:1\n  │\n4 │ $ac$\n  │  ^^\n  │\n  = hint: try adding spaces between each letter: `a c`\n"
     "unknown variable: ac")
    ("warning before the error"
     "warning: unused import\nerror: expected expression\n"
     "expected expression")
    ("no error prefix" "\n  something odd happened\n" "something odd happened")
    ("no output" "" "Compilation failed"))
  "Error summary cases: (NAME OUTPUT EXPECTED).")

(ert-deftest typst-overlay-test-error-summary ()
  "The first error line is extracted from typst's output."
  (dolist (case typst-overlay-test--error-summary-cases)
    (pcase-let ((`(,name ,output ,expected) case))
      (should (equal (list name expected)
                     (list name (typst-overlay--compile-error-summary output)))))))

(defun typst-overlay-test--failed-record (element generation)
  "Return a registry holding a rendering record for ELEMENT at GENERATION."
  (let ((registry (typst-overlay--make-registry)))
    (typst-overlay--put-record
     registry element
     (make-typst-overlay-record :element element :state 'rendering
                                :generation generation))
    registry))

(ert-deftest typst-overlay-test-render-failure ()
  "A failed render underlines the equation, unless it is outdated."
  (with-temp-buffer
    (insert "Bad $ac$ end.")
    (setq-local typst-overlay-mode t)
    (let ((element (typst-overlay-test--element 5 "$ac$")))
      ;; Current generation: marked failed and underlined.
      (setq typst-overlay--registry (typst-overlay-test--failed-record element 1))
      (typst-overlay--handle-render-failure
       element element 1 "error: unknown variable: ac\n")
      (let* ((record (typst-overlay--get-record typst-overlay--registry element))
             (overlay (typst-overlay-record-overlay record)))
        (should (eq (typst-overlay-record-state record) 'failed))
        (should (equal (buffer-substring (overlay-start overlay) (overlay-end overlay))
                       "$ac$"))
        (should (eq (overlay-get overlay 'face) 'typst-overlay-error))
        (should (equal (overlay-get overlay 'typst-overlay-error)
                       "unknown variable: ac")))
      ;; Text changed while compiling: failed, but nothing to underline.
      (remove-overlays)
      (setq typst-overlay--registry (typst-overlay-test--failed-record element 1))
      (typst-overlay--handle-render-failure element nil 1 "error: x\n")
      (should-not (typst-overlay-record-overlay
                   (typst-overlay--get-record typst-overlay--registry element)))
      ;; Outdated generation: left alone.
      (setq typst-overlay--registry (typst-overlay-test--failed-record element 2))
      (typst-overlay--handle-render-failure element element 1 "error: x\n")
      (should (eq (typst-overlay-record-state
                   (typst-overlay--get-record typst-overlay--registry element))
                  'rendering)))))

(ert-deftest typst-overlay-test-error-shown-once ()
  "The error is shown when point enters the equation, not on every command."
  (with-temp-buffer
    (insert "Bad $ac$ end.")
    (typst-overlay--place-error-overlay
     (typst-overlay-test--element 5 "$ac$") "unknown variable: ac")
    (let (shown)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) shown))))
        (dolist (pos '(1 6 7 6 1 7))    ; outside, inside x3, outside, inside
          (goto-char pos)
          (typst-overlay--echo-error-at-point)))
      (should (equal (reverse shown)
                     '("Typst: unknown variable: ac"
                       "Typst: unknown variable: ac"))))))

;;; Compile bookkeeping

(defmacro typst-overlay-test--with-fake-compiles (callbacks &rest body)
  "Run BODY with compiles faked; started callbacks are pushed to CALLBACKS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'typst-overlay--start-async-compile)
              (lambda (_source _svg-path callback) (push callback ,callbacks)))
             ((symbol-function 'typst-overlay--artifact-svg-path)
              (lambda (_key) "unused.svg")))
     ,@body))

(ert-deftest typst-overlay-test-compile-count ()
  "Compiles finishing after a teardown do not corrupt the count."
  (with-temp-buffer
    (insert "Text $a$ end.")
    (setq-local typst-overlay-mode t)
    (let ((element (typst-overlay-test--element 6 "$a$"))
          callbacks)
      (typst-overlay-test--with-fake-compiles callbacks
        ;; A compile finishing normally brings the count back to 0.
        (typst-overlay--ensure-runtime)
        (typst-overlay--start-render-for-element
         element 1 typst-overlay--artifact-cache)
        (should (= typst-overlay--active-compiles 1))
        (funcall (pop callbacks) 'failure "")
        (should (= typst-overlay--active-compiles 0))
        ;; One finishing after a teardown is ignored.
        (typst-overlay--start-render-for-element
         element 1 typst-overlay--artifact-cache)
        (typst-overlay--teardown)
        (typst-overlay--ensure-runtime)
        (funcall (pop callbacks) 'failure "")
        (should (= typst-overlay--active-compiles 0))))))

(provide 'typst-overlay-test)

;;; typst-overlay-test.el ends here
