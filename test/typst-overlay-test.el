;;; typst-overlay-test.el --- Tests for typst-overlay -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Table-driven tests for equation detection.  Each case is
;; (NAME INPUT EXPECTED), where EXPECTED is the list of equation
;; texts, in buffer order, that should be detected in INPUT.
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
  (typst-overlay-test--run-cases #'typst-overlay-test--setup-org
                                 typst-overlay-test--org-cases))

;;; Typst

(defconst typst-overlay-test--typst-cases
  '(;; Detected
    ("inline and display" "Text $x$ and $ y $." ("$x$" "$ y $"))
    ("inside content block" "#box[inside $c$]\nend $d$" ("$c$" "$d$"))
    ("after code" "#let v = 2\n#box[text]\n$u$" ("$u$"))
    ("before parse error" "Before $p$.\n#let broken = (\nAfter $q$." ("$p$"))
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

(provide 'typst-overlay-test)

;;; typst-overlay-test.el ends here
