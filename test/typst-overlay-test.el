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
(require 'image)  ; before `create-image' is stubbed, so loading it later cannot undo that
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
    ("unclosed outer: found up to the next $" "$#table($x$ unclosed" ("$#table($"))
    ("syntax error: unclosed parenthesis" "a $frac(1, 2$ b" ("$frac(1, 2$"))
    ("syntax error: unclosed string" "a $ \"oops $ b" ("$ \"oops $"))
    ("syntax error: keyword without body" "a $#let$ b" ("$#let$"))
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
    ;; Broken equations are kept and the rest continues
    ("broken equation among sections"
     "= Intro\n\nFirst $a$ and $b^2$.\n\n= Methods\n\nBroken $frac(1, 2$ here.\n\nThen $c$.\n\n= Results\n\n$ sum_(i=1)^n i $\n"
     ("$a$" "$b^2$" "$frac(1, 2$" "$c$" "$ sum_(i=1)^n i $"))
    ("unclosed string in math"
     "Good $a$.\n\nBroken $ \"oops $ here.\n\nAfter $b$ and $c$.\n"
     ("$a$" "$ \"oops $" "$b$" "$c$"))
    ("broken equation inside a content block"
     "#block[\n  Inside $a$ and $frac(1, 2$.\n]\n\nOutside $b$.\n"
     ("$a$" "$frac(1, 2$" "$b$"))
    ("two broken equations"
     "$a$ then $frac(1$ and $ \"x $ then $b$.\n"
     ("$a$" "$frac(1$" "$ \"x $" "$b$"))
    ("two broken equations under a heading"
     "= A\n\nOne $frac(1, 2$.\n\nGood $a$.\n\nTwo $ \"oops $.\n\nGood $b$.\n"
     ("$frac(1, 2$" "$a$" "$ \"oops $" "$b$"))
    ("dollars in raw text are not blamed"
     "= A\n\nRaw `$ \"x $` here.\n\nBroken $frac(1, 2$.\n\nAfter $b$.\n"
     ("$frac(1, 2$" "$b$"))
    ("clean math before a broken equation is skipped whole"
     "= A\n\n#let r = $b$\n#let n(x) = $x$\n\nBroken $frac(1, 2$.\n\nAfter $c$.\n"
     ("$frac(1, 2$" "$c$"))
    ("code still recognised after a broken equation"
     "Broken $frac(1, 2$.\n\n#let f = $a$\n\nAfter $b$.\n"
     ("$frac(1, 2$" "$b$"))
    ("dollars in a comment are not blamed"
     "= A\n\n// see $ \"x $ here\n\nBroken $frac(1, 2$.\n\nAfter $b$.\n"
     ("$frac(1, 2$" "$b$"))
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
    ("short diagnostic format"
     "<stdin>:5:3: error: unknown variable: ac\n"
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
       element element 1 "unknown variable: ac")
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
      (typst-overlay--handle-render-failure element nil 1 "x")
      (should-not (typst-overlay-record-overlay
                   (typst-overlay--get-record typst-overlay--registry element)))
      ;; Outdated generation: left alone.
      (setq typst-overlay--registry (typst-overlay-test--failed-record element 2))
      (typst-overlay--handle-render-failure element element 1 "x")
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

;;; Batch compile

(ert-deftest typst-overlay-test-batch-source ()
  "Each job's recorded lines match its block in the batch source."
  (let ((typst-overlay-extra-prelude "#let k = 1")
        (jobs (list (make-typst-overlay-job
                     :element (typst-overlay--make-element
                               (make-typst-overlay-math-node
                                :beg 1 :end 4 :text "$a$" :text-hash (md5 "$a$"))
                               (list (make-typst-overlay-code-node
                                      :beg 1 :end 2 :text "#let f = 2" :hash ""))))
                    (make-typst-overlay-job
                     :element (typst-overlay-test--element 10 "$\nb\n$")))))
    (pcase-let* ((`(,source . ,lines) (typst-overlay--batch-source jobs))
                 (source-lines (vconcat (split-string source "\n"))))
      (cl-flet ((line (n) (aref source-lines (1- n))))
        (should (= (length lines) 2))
        (pcase-let ((`(,first ,second) lines))
          (should (equal (line (typst-overlay-job-lines-beg first)) "#["))
          (should (equal (line (typst-overlay-job-lines-prelude-end first))
                         "#let f = 2"))
          (should (equal (line (typst-overlay-job-lines-end first)) "]"))
          (should (equal (line (1+ (typst-overlay-job-lines-end first)))
                         "#pagebreak()"))
          (should (equal (line (typst-overlay-job-lines-prelude-end second))
                         "#let k = 1"))
          (should (equal (line (1- (typst-overlay-job-lines-end second))) "$"))
          (should (equal (line (typst-overlay-job-lines-end second)) "]")))))))

(ert-deftest typst-overlay-test-failed-jobs ()
  "An error is traced to its job, or to every job sharing a prelude."
  (let* ((typst-overlay-extra-prelude "")
         (shared (list (make-typst-overlay-code-node
                        :beg 1 :end 2 :text "#let f = 2" :hash "")))
         (make (lambda (beg text prelude)
                 (make-typst-overlay-job
                  :element (typst-overlay--make-element
                            (make-typst-overlay-math-node
                             :beg beg :end (+ beg (length text))
                             :text text :text-hash (md5 text))
                            prelude))))
         (jobs (list (funcall make 1 "$a$" nil)
                     (funcall make 10 "$b$" nil)
                     (funcall make 20 "$c$" shared)
                     (funcall make 30 "$d$" shared)))
         (lines (cdr (typst-overlay--batch-source jobs))))
    (cl-flet ((failed (job-index field)
                (mapcar (lambda (job)
                          (typst-overlay-element-text (typst-overlay-job-element job)))
                        (typst-overlay--failed-jobs
                         jobs lines (funcall field (nth job-index lines))))))
      ;; An error on a job's "#[" line belongs to that job only, even
      ;; when it has no prelude.
      (should (equal (failed 1 #'typst-overlay-job-lines-beg) '("$b$")))
      ;; An error in its math belongs to that job only.
      (should (equal (failed 2 #'typst-overlay-job-lines-end) '("$c$")))
      ;; An error in a prelude fails every job sharing it.
      (should (equal (failed 2 #'typst-overlay-job-lines-prelude-end)
                     '("$c$" "$d$"))))))

(ert-deftest typst-overlay-test-job-error ()
  "A job's error is the innermost one reported within its lines."
  (let ((errors (typst-overlay--compile-errors
                 (concat "<stdin>:10:1: error: unclosed delimiter\n"
                         "<stdin>:11:0: error: unclosed delimiter\n"
                         "<stdin>:11:2: error: unclosed string\n"
                         "<stdin>:20:3: error: elsewhere\n"
                         "other.typ:1:1: error: in an imported file\n"))))
    (should (equal errors '((10 . "unclosed delimiter")
                            (11 . "unclosed delimiter")
                            (11 . "unclosed string")
                            (20 . "elsewhere"))))
    (should (equal (typst-overlay--job-error
                    errors (make-typst-overlay-job-lines :beg 10 :prelude-end 10 :end 12))
                   "unclosed string"))
    (should-not (typst-overlay--job-error
                 errors (make-typst-overlay-job-lines :beg 13 :prelude-end 13 :end 15)))))

(defun typst-overlay-test--page-count (source)
  "Return how many pages SOURCE produces."
  (let ((count 1) (start 0))
    (while (string-match "^#pagebreak()$" source start)
      (cl-incf count)
      (setq start (match-end 0)))
    count))

(defun typst-overlay-test--error-at (marker message)
  "Return a fake typst that fails at the first line containing MARKER.
MESSAGE is the error it reports there."
  (lambda (source)
    (let ((index (cl-position-if (lambda (line) (string-search marker line))
                                 (split-string source "\n"))))
      (and index (format "<stdin>:%d:1: error: %s\n" (1+ index) message)))))

(defmacro typst-overlay-test--with-batches (text behavior &rest body)
  "Enable the mode on an org file with TEXT, compiling with a fake typst.
BEHAVIOR is called with each batch source and returns nil to succeed
or typst's error output to fail.  Within BODY, `run-compiles' runs
pending compiles until none are left, and `batches' lists the page
counts of every compile started, in order."
  (declare (indent 2))
  `(let ((dir (make-temp-file "typst-overlay-test-" t))
         (pending nil)
         (batches nil)
         (behavior ,behavior))
     (unwind-protect
         (cl-letf (((symbol-function 'executable-find)
                    (lambda (&rest _) "/usr/bin/typst"))
                   ;; CI's Emacs may lack SVG support; no real image is needed.
                   ((symbol-function 'image-type-available-p)
                    (lambda (&rest _) t))
                   ((symbol-function 'create-image)
                    (lambda (&rest _) '(image :type svg)))
                   ((symbol-function 'typst-overlay--compile-async)
                    (lambda (source out callback)
                      (setq batches (append batches
                                            (list (typst-overlay-test--page-count
                                                   source))))
                      (setq pending (append pending
                                            (list (list source out callback)))))))
           (with-temp-buffer
             (setq buffer-file-name (expand-file-name "test.org" dir))
             (insert ,text)
             (org-mode)
             (cl-flet ((run-compiles ()
                         (while pending
                           (pcase-let* ((`(,source ,out ,callback) (pop pending))
                                        (output (funcall behavior source)))
                             (unless output
                               (dotimes (i (typst-overlay-test--page-count source))
                                 (with-temp-file (expand-file-name
                                                  (format "%d.svg" (1+ i)) out)
                                   (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"/>"))))
                             (funcall callback (null output) (or output ""))))))
               (typst-overlay-mode 1)
               ,@body
               (set-buffer-modified-p nil))))
       (delete-directory dir t))))

(defun typst-overlay-test--equations (n)
  "Return org text with N different equations, one per line."
  (mapconcat (lambda (i) (format "Line $x_%d$ here." i))
             (number-sequence 1 n) "\n"))

(defun typst-overlay-test--overlays (property)
  "Return the texts covered by overlays with PROPERTY, in order."
  (mapcar (lambda (overlay)
            (buffer-substring-no-properties (overlay-start overlay)
                                            (overlay-end overlay)))
          (sort (seq-filter (lambda (overlay) (overlay-get overlay property))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest typst-overlay-test-batch-split ()
  "Equations are spread evenly over at most the compile limit."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-max-active-compiles 8))
    (typst-overlay-test--with-batches (typst-overlay-test--equations 20) #'ignore
      (should (equal batches '(3 3 3 3 3 3 2)))
      (run-compiles)
      (should (= (length (typst-overlay-test--overlays 'typst-overlay)) 20))
      (should (= typst-overlay--active-compiles 0)))
    (typst-overlay-test--with-batches (typst-overlay-test--equations 1) #'ignore
      (should (equal batches '(1))))))

(ert-deftest typst-overlay-test-batch-broken-equation ()
  "A broken equation fails on its own; the rest of its batch is retried."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-max-active-compiles 8))
    (typst-overlay-test--with-batches
        (replace-regexp-in-string "x_5" "BAD" (typst-overlay-test--equations 20))
        (typst-overlay-test--error-at "BAD" "unknown variable: BAD")
      (run-compiles)
      ;; Equation 5 is in the second batch of 3, retried as a batch of 2.
      (should (equal batches '(3 3 3 3 3 3 2 2)))
      (should (= (length (typst-overlay-test--overlays 'typst-overlay)) 19))
      (should (equal (typst-overlay-test--overlays 'typst-overlay-error) '("$BAD$")))
      (should (equal (overlay-get (car (overlays-at (1+ (string-search "$BAD$" (buffer-string)))))
                                  'typst-overlay-error)
                     "unknown variable: BAD")))))

(ert-deftest typst-overlay-test-batch-broken-prelude ()
  "A broken prelude fails every equation sharing it, without retries."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-max-active-compiles 8)
        (typst-overlay-extra-prelude "#import \"BROKEN.typ\": *"))
    (typst-overlay-test--with-batches (typst-overlay-test--equations 20)
        (typst-overlay-test--error-at "BROKEN" "file not found")
      (run-compiles)
      (should (equal batches '(3 3 3 3 3 3 2)))
      (should (= (length (typst-overlay-test--overlays 'typst-overlay-error)) 20)))))

(ert-deftest typst-overlay-test-batch-untraceable-error ()
  "An error without a location falls back to one compile per equation."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-max-active-compiles 2))
    (typst-overlay-test--with-batches (typst-overlay-test--equations 4)
        (lambda (source)
          (and (> (typst-overlay-test--page-count source) 1) "error: boom\n"))
      (run-compiles)
      (should (equal batches '(2 2 1 1 1 1)))
      (should (= (length (typst-overlay-test--overlays 'typst-overlay)) 4)))))

(ert-deftest typst-overlay-test-batch-after-teardown ()
  "Batches finishing after the mode is turned off change nothing."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches (typst-overlay-test--equations 3) #'ignore
    (typst-overlay-mode -1)
    (typst-overlay-mode 1)
    (run-compiles)
    (should (= typst-overlay--active-compiles 0))
    ;; Only the second enable's compiles placed overlays, once each.
    (should (= (length (typst-overlay-test--overlays 'typst-overlay)) 3))))

;;; Image size

(defconst typst-overlay-test--image-size-cases
  '(("real typst header, default scale"
     "<svg viewBox=\"0 0 13.2893 11.2268\" width=\"13.2893pt\" height=\"11.2268pt\" xmlns=\"http://www.w3.org/2000/svg\"><g/></svg>"
     1.3 (:height (1.3268 . em)))
    ("one em at scale 1"
     "<svg width=\"30pt\" height=\"11pt\"><g/></svg>" 1.0 (:height (1.0 . em)))
    ("height on an inner element is ignored"
     "<svg width=\"30pt\"><rect height=\"22pt\"/></svg>" 1.3 (:scale 1.3))
    ("height without units falls back"
     "<svg width=\"30\" height=\"11\"><g/></svg>" 1.3 (:scale 1.3)))
  "Image size cases: (NAME SVG SCALE EXPECTED).
EXPECTED is the size property the image is created with.")

(ert-deftest typst-overlay-test-image-size ()
  "Images are sized in ems from the SVG height, or fall back to a scale."
  (dolist (case typst-overlay-test--image-size-cases)
    (pcase-let ((`(,name ,svg ,scale ,expected) case))
      (cl-letf (((symbol-function 'create-image)
                 (lambda (_data _type _data-p &rest props) props)))
        (let* ((typst-overlay-scale scale)
               (props (typst-overlay--create-image svg))
               (actual (if (plist-member props :height)
                           (let ((height (plist-get props :height)))
                             (list :height (cons (/ (round (* 10000 (car height))) 10000.0)
                                                 (cdr height))))
                         (list :scale (plist-get props :scale)))))
          (should (equal (list name expected) (list name actual))))))))

(provide 'typst-overlay-test)

;;; typst-overlay-test.el ends here
