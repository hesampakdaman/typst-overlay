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

(defun typst-overlay-test--enable-error (has-typst has-grammar)
  "Enable the mode with HAS-TYPST and HAS-GRAMMAR faked.
Return the `user-error' message and whether the mode stayed on."
  (with-temp-buffer
    (org-mode)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) (and has-typst "/usr/bin/typst")))
              ((symbol-function 'treesit-language-available-p)
               (lambda (&rest _) has-grammar)))
      (list (condition-case err
                (progn (typst-overlay-mode 1) nil)
              (user-error (cadr err)))
            typst-overlay-mode))))

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
   :version 0
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

(provide 'typst-overlay-test)

;;; typst-overlay-test.el ends here
