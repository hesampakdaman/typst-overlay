;;; typst-overlay-test.el --- Tests for typst-overlay -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Table-driven tests.  Detection cases are (NAME INPUT EXPECTED),
;; where EXPECTED is the list of equation texts, in buffer order, that
;; should be detected in INPUT.  Edit cases cover what is compiled
;; again after the buffer changes, and what each equation then shows.
;;
;; Run with:
;;
;;   emacs --batch -Q -L . -l test/typst-overlay-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'flymake)
(require 'image)  ; before `create-image' is stubbed, so loading it later cannot undo that
(require 'org)
(require 'typst-overlay)

(defvar eglot-managed-mode-hook)

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
    ("dollar in a string in code" "$#table(\"a $ b\")$ c" ("$#table(\"a $ b\")$"))
    ("escaped dollar inside math" "$a \\$ b$ c" ("$a \\$ b$"))
    ("half-open interval" "an $[0, 1)$ interval" ("$[0, 1)$"))
    ("variable in math" "$#x$ var" ("$#x$"))
    ("unclosed outer: found up to the next $" "$#table($x$ unclosed" ("$#table($"))
    ("syntax error: unclosed parenthesis" "a $frac(1, 2$ b" ("$frac(1, 2$"))
    ;; Not detected
    ("escaped dollars before words" "use \\$a or \\$b" ())
    ("prices" "costs $5 and $10 total" ())
    ("empty span" "$$" ())
    ("lone dollar" "a $ b" ())
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
    ("content passed to a function" "#enum(enum.item[$x$])" ("$x$"))
    ("content bound by #let" "#let c = [$c$]" ("$c$"))
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
    ("after parse error" "#let h = (\n$e$" ())
    ("in a function" "#enum(numbering: n => [$d_#n$])" ())
    ("in a function defined with #let" "#let f(x) = [$x^2$]" ())
    ("in a function's code block" "#let f(y) = { [$y$] }" ())
    ("in a show rule's function" "#show heading: it => [$h$ #it]" ())
    ("in a for loop" "#for i in range(3) [$i$]" ())
    ("in a while loop" "#while false [$w$]" ()))
  "Typst cases: (NAME INPUT EXPECTED).")

(ert-deftest typst-overlay-test-typst-detection ()
  "The Typst analyzer detects exactly the expected equations."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--run-cases #'typst-overlay-test--setup-typst
                                 typst-overlay-test--typst-cases))

;;; Preludes

(defconst typst-overlay-test--prelude-cases
  '(("top-level #let" "#let k = 1\n$k$" (("$k$" "#let k = 1")))
    ("#let in a content block"
     "#enum(\n  enum.item[\n    #let f(x) = $#x$\n    $f(1)$\n  ],\n)"
     (("$f(1)$" "#let f(x) = $#x$")))
    ("#let does not leave its content block"
     "#box[#let k = 1\n$k$]\n$k$"
     (("$k$" "#let k = 1") ("$k$" "")))
    ("#let under a heading reaches later sections"
     "=== A\n#let k = 1\n$k$\n=== B\n$k$"
     (("$k$" "#let k = 1") ("$k$" "#let k = 1")))
    ("#let in a block under a heading"
     "=== A\n#box[#let k = 1\n$k$]\n=== B\n$k$"
     (("$k$" "#let k = 1") ("$k$" "")))
    ("top-level and block #let"
     "#let a = 1\n#box[#let b = 2\n$a + b$]"
     (("$a + b$" "#let a = 1\n\n#let b = 2"))))
  "Prelude cases: (NAME INPUT ((EQUATION PRELUDE) ...)).")

(ert-deftest typst-overlay-test-typst-prelude ()
  "Each equation gets the #let code in scope before it as its prelude."
  (skip-unless (treesit-language-available-p 'typst))
  (dolist (case typst-overlay-test--prelude-cases)
    (pcase-let ((`(,name ,input ,expected) case))
      (with-temp-buffer
        (insert input)
        (typst-overlay-test--setup-typst)
        (let ((snapshot (typst-overlay--make-snapshot
                         (funcall typst-overlay--analyzer))))
          (should (equal (list name expected)
                         (list name
                               (mapcar (lambda (element)
                                         (list (typst-overlay-element-text element)
                                               (typst-overlay-element-prelude-text element)))
                                       (typst-overlay-snapshot-elements snapshot))))))))))

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

;;; Elements

(defun typst-overlay-test--element (beg text)
  "Return an element for TEXT starting at BEG, with no prelude."
  (typst-overlay--make-element
   (make-typst-overlay-math-node :beg beg
                                 :end (+ beg (length text))
                                 :text text
                                 :text-hash (md5 text))
   nil))

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

(ert-deftest typst-overlay-test-eldoc ()
  "ElDoc shows the error of the failed equation at point."
  (with-temp-buffer
    (insert "Bad $ac$ end.")
    (typst-overlay--place-error-overlay
     (typst-overlay-test--element 5 "$ac$") "unknown variable: ac")
    (cl-flet ((doc ()
                (let (result)
                  (typst-overlay--eldoc
                   (lambda (&rest args) (setq result args)))
                  result)))
      (goto-char 6)
      (should (equal (doc) '("unknown variable: ac" :thing "Typst" :face error)))
      (goto-char 1)
      (should-not (doc))
      ;; Not where Flymake already shows a diagnostic.
      (goto-char 6)
      (setq-local flymake-mode t)
      (cl-letf (((symbol-function 'flymake-diagnostics)
                 (lambda (&rest _) '(diagnostic))))
        (should-not (doc))))))

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
          (should (equal (line (- (typst-overlay-job-lines-end second) 3)) "$]"))
          (should (equal (line (typst-overlay-job-lines-end second)) "]")))))))

(ert-deftest typst-overlay-test-root-paths ()
  "Relative import paths are rewritten from the project root."
  (cl-flet ((rebase (prelude)
              (typst-overlay--root-paths prelude "/p/sub/" "/p/")))
    (should (equal (rebase "#import \"defs.typ\": f") "#import \"/sub/defs.typ\": f"))
    (should (equal (rebase "#import \"../common.typ\"") "#import \"/common.typ\""))
    (should (equal (rebase "#include \"part.typ\"") "#include \"/sub/part.typ\""))
    ;; Packages, absolute paths and paths outside the root are kept.
    (dolist (prelude '("#import \"@preview/physica:0.9.5\": *"
                       "#import \"/common.typ\""
                       "#import \"../../outside.typ\""
                       "#let path = \"defs.typ\""))
      (should (equal (rebase prelude) prelude)))
    ;; Nothing to rewrite when the file is in the root.
    (should (equal (typst-overlay--root-paths "#import \"defs.typ\"" "/p/" "/p/")
                   "#import \"defs.typ\""))))

(ert-deftest typst-overlay-test-project-root ()
  "Batches run from the project root, with imports rewritten to it."
  (let* ((root (file-name-as-directory (make-temp-file "typst-overlay-test-" t)))
         (sub (expand-file-name "sub/" root))
         (project-vc-extra-root-markers '(".typst-overlay-test-root"))
         compiled)
    (unwind-protect
        (progn
          (make-directory sub)
          (write-region "" nil (expand-file-name ".typst-overlay-test-root" root))
          (should (equal (typst-overlay--project-root sub) root))
          ;; Outside a project, the folder itself.
          (should (equal (typst-overlay--project-root temporary-file-directory)
                         temporary-file-directory))
          (cl-letf (((symbol-function 'typst-overlay--compile-async)
                     (lambda (source _dir _callback)
                       (setq compiled (cons source default-directory)))))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "main.typ" sub))
              (typst-overlay--run-batch
               (list (make-typst-overlay-job
                      :element (typst-overlay--make-element
                                (make-typst-overlay-math-node
                                 :beg 30 :end 33 :text "$a$" :text-hash (md5 "$a$"))
                                (list (make-typst-overlay-code-node
                                       :beg 1 :end 28 :text "#import \"../common.typ\": a"
                                       :hash "")))
                      :svg-path (expand-file-name "a.svg" root)))
               nil (current-buffer) typst-overlay--session)
              (set-buffer-modified-p nil)))
          (should (equal (cdr compiled) root))
          (should (string-search "#import \"/common.typ\": a" (car compiled))))
      (delete-directory root t))))

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

(defun typst-overlay-test--source-equations (source)
  "Return the equation texts in batch SOURCE, one per page, in order."
  (let ((start 0) equations)
    (while (string-match "^#let typst-overlay-math = \\[\\(\\(?:.\\|\n\\)*?\\)]\n#typst-overlay-math$"
                         source start)
      (push (match-string 1 source) equations)
      (setq start (match-end 0)))
    (nreverse equations)))

(defmacro typst-overlay-test--with-batches (text behavior &rest body)
  "Enable the mode on an org file with TEXT, compiling with a fake typst.
BEHAVIOR is called with each batch source and returns nil to succeed
or typst's error output to fail.  Each SVG written names the equation
it was rendered from.  Within BODY, `run-compiles' runs pending
compiles until none are left, the newest first if given a non-nil
argument.  `batches' lists the page counts of every compile started,
and `compiled' the equations compiled, in order."
  (declare (indent 2))
  `(let ((dir (make-temp-file "typst-overlay-test-" t))
         (pending nil)
         (batches nil)
         (compiled nil)
         (behavior ,behavior))
     (unwind-protect
         (cl-letf (((symbol-value 'typst-overlay-cache-directory)
                    (expand-file-name "cache/" dir))
                   ((symbol-function 'executable-find)
                    (lambda (&rest _) "/usr/bin/typst"))
                   ;; CI's Emacs may lack SVG support; no real image is needed.
                   ((symbol-function 'image-type-available-p)
                    (lambda (&rest _) t))
                   ((symbol-function 'create-image)
                    (lambda (data &rest _) (list 'image :type 'svg :data data)))
                   ((symbol-function 'typst-overlay--compile-async)
                    (lambda (source out callback)
                      (setq batches (append batches
                                            (list (typst-overlay-test--page-count
                                                   source))))
                      (setq compiled (append compiled
                                             (typst-overlay-test--source-equations
                                              source)))
                      (setq pending (append pending
                                            (list (list source out callback)))))))
           (with-temp-buffer
             (setq buffer-file-name (expand-file-name "test.org" dir))
             (insert ,text)
             (org-mode)
             (cl-flet ((run-compiles (&optional newest-first)
                         (when newest-first
                           (setq pending (reverse pending)))
                         (while pending
                           (pcase-let* ((`(,source ,out ,callback) (pop pending))
                                        (output (funcall behavior source)))
                             (unless output
                               (seq-do-indexed
                                (lambda (equation i)
                                  (with-temp-file (expand-file-name
                                                   (format "%d.svg" (1+ i)) out)
                                    (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><!-- "
                                            equation " --></svg>")))
                                (typst-overlay-test--source-equations source)))
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

(ert-deftest typst-overlay-test-imported-file-changed ()
  "Equations are compiled again when a file they import changes."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-extra-prelude "#import \"defs.typ\": *"))
    (typst-overlay-test--with-batches "Text $a$ end." #'ignore
      (write-region "#let a = 1" nil (expand-file-name "defs.typ" dir))
      (typst-overlay-refresh)
      (run-compiles)
      (setq compiled nil)
      ;; Unchanged, nothing is compiled.
      (typst-overlay-refresh)
      (run-compiles)
      (should-not compiled)
      (write-region "#let a = 2" nil (expand-file-name "defs.typ" dir))
      (typst-overlay-refresh)
      (run-compiles)
      (should (equal compiled '("$a$"))))))

(ert-deftest typst-overlay-test-imports-hash ()
  "The prelude hash covers the files it imports, from the file or the root."
  (let* ((root (file-name-as-directory (make-temp-file "typst-overlay-test-" t)))
         (sub (expand-file-name "sub/" root))
         (project-vc-extra-root-markers '(".typst-overlay-test-root")))
    (unwind-protect
        (with-temp-buffer
          (make-directory sub)
          (write-region "" nil (expand-file-name ".typst-overlay-test-root" root))
          (setq buffer-file-name (expand-file-name "main.typ" sub))
          (cl-flet ((hash (text)
                      (cdr (typst-overlay--prelude
                            (list (make-typst-overlay-code-node :beg 1 :text text))
                            (typst-overlay--prelude-table))))
                    (write (file text)
                      (write-region text nil (expand-file-name file root))))
            (dolist (text '("#import \"defs.typ\": f" "#include \"/top.typ\""))
              (write "sub/defs.typ" "1")
              (write "top.typ" "1")
              (let ((before (hash text)))
                (should (equal (hash text) before))
                (write (if (string-search "defs" text) "sub/defs.typ" "top.typ") "2")
                (should-not (equal (hash text) before))))
            ;; Packages are not read.
            (should (equal (typst-overlay--imports-hash
                            "#import \"@preview/physica:0.9.5\": *"
                            (typst-overlay--prelude-table))
                           "")))
          (set-buffer-modified-p nil))
      (delete-directory root t))))

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

;;; Edits

;; Each case is (NAME TEXT STEPS COMPILED SHOWN).  TEXT is rendered,
;; then STEPS are applied, followed by a refresh and running every
;; compile.  A step is (insert POS STRING), (replace FROM TO) for the
;; first FROM, `refresh', `run', `run-newest-first' (compiles finish
;; out of order), `delete-images' (the cache folder's SVGs are deleted)
;; or `reopen' (the mode is turned off and on).  COMPILED is the
;; equations compiled during STEPS, in order.  SHOWN describes each
;; equation's overlay in buffer order: its text when it shows its own
;; image, "TEXT showing OTHER" for another equation's image, and
;; "TEXT failed" when underlined as failed.

(defconst typst-overlay-test--edit-cases
  '(("nothing changed" "$a$ and $b$" () () ("$a$" "$b$"))
    ("text typed above: images reused"
     "$a$ and $b$" ((insert 1 "New. ")) () ("$a$" "$b$"))
    ("swapped: images reused"
     "$a$ and $b$" ((replace "$a$ and $b$" "$b$ and $a$")) () ("$b$" "$a$"))
    ("cut and pasted after the other: image reused"
     "$a$ and $bb$" ((replace "$a$ and $bb$" "$bb$ and $a$")) () ("$bb$" "$a$"))
    ("one edited: only it compiled"
     "$a$ and $b$" ((replace "$b$" "$c$")) ("$c$") ("$a$" "$c$"))
    ("one deleted"
     "$a$ and $b$" ((replace " and $b$" "")) () ("$a$"))
    ("duplicates, one deleted"
     "$x$ and $x$ and $y$" ((replace "$x$ and " "")) () ("$x$" "$y$"))
    ("duplicates, nothing changed"
     "$x$ and $x$" () () ("$x$" "$x$"))
    ("edited back: image reused from the cache"
     "$a$ and $b$" ((replace "$b$" "$c$") refresh run (replace "$c$" "$b$"))
     ("$c$") ("$a$" "$b$"))
    ("edited and restored before a refresh: image reused"
     "$a$ and $b$" ((replace "$b$" "$c$") (replace "$c$" "$b$")) () ("$a$" "$b$"))
    ("reopened: images loaded from the cache folder"
     "$a$ and $b$" (reopen) () ("$a$" "$b$"))
    ("images deleted from disk, then moved: overlays kept, nothing read"
     "$a$ and $b$" (delete-images (insert 1 "New. ")) () ("$a$" "$b$"))
    ("images deleted from disk, edited and restored: compiled again"
     "$a$ and $b$" (delete-images (replace "$b$" "$c$") (replace "$c$" "$b$"))
     ("$b$") ("$a$" "$b$"))
    ("failed and unchanged: not retried"
     "$a$ and $BAD$" () () ("$a$" "$BAD$ failed"))
    ("failed, edited and restored: retried"
     "$a$ and $BAD$" ((replace "$BAD$" "$BAX$") (replace "$BAX$" "$BAD$"))
     ("$BAD$") ("$a$" "$BAD$ failed"))
    ("moved while compiling: compiled again"
     "$a$ and $b$" ((replace "$b$" "$c$") refresh (insert 1 "New. "))
     ("$c$" "$c$") ("$a$" "$c$"))
    ("edited while compiling, results out of order: outdated one dropped"
     "$a$ and $b$" ((replace "$b$" "$c$") refresh (replace "$c$" "$d$") refresh
                    run-newest-first)
     ("$c$" "$d$") ("$a$" "$d$"))
    ("edited while failing, results out of order: outdated failure dropped"
     "$a$ and $b$" ((replace "$b$" "$BAD$") refresh (replace "$BAD$" "$BAE$") refresh
                    run-newest-first)
     ("$BAD$" "$BAE$") ("$a$" "$BAE$")))
  "Edit cases: (NAME TEXT STEPS COMPILED SHOWN).")

(defun typst-overlay-test--shown ()
  "Describe each equation overlay in the buffer, in order.
See `typst-overlay-test--edit-cases'."
  (mapcar (lambda (overlay)
            (let ((text (buffer-substring-no-properties (overlay-start overlay)
                                                        (overlay-end overlay)))
                  (data (plist-get (cdr (overlay-get overlay 'typst-overlay-image))
                                   :data)))
              (cond
               ((overlay-get overlay 'typst-overlay-error) (concat text " failed"))
               ((string-match "<!-- \\(.*\\) -->" data)
                (if (equal (match-string 1 data) text)
                    text
                  (format "%s showing %s" text (match-string 1 data)))))))
          (sort (seq-filter (lambda (overlay)
                              (or (overlay-get overlay 'typst-overlay)
                                  (overlay-get overlay 'typst-overlay-error)))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest typst-overlay-test-edits ()
  "After edits, only what changed is compiled and each equation shows its image."
  (skip-unless (treesit-language-available-p 'typst))
  (dolist (case typst-overlay-test--edit-cases)
    (pcase-let ((`(,name ,text ,steps ,expected-compiled ,expected-shown) case))
      (typst-overlay-test--with-batches text
          (typst-overlay-test--error-at "BAD" "unknown variable: BAD")
        (run-compiles)
        (setq compiled nil)
        (dolist (step (append steps '(refresh run)))
          (pcase step
            (`(insert ,pos ,string) (goto-char pos) (insert string))
            (`(replace ,from ,to)
             (goto-char (point-min))
             (search-forward from)
             (replace-match to t t))
            ('refresh (typst-overlay-refresh))
            ('run (run-compiles))
            ('run-newest-first (run-compiles t))
            ('delete-images
             (dolist (file (directory-files-recursively dir "\\.svg\\'"))
               (delete-file file)))
            ('reopen (typst-overlay-mode -1) (typst-overlay-mode 1))))
        (should (equal (list name expected-compiled expected-shown)
                       (list name compiled (typst-overlay-test--shown))))))))

(ert-deftest typst-overlay-test-moved-overlays-kept ()
  "Equations moved by typing before them keep their overlays."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "$a$ and $b$, $c$" #'ignore
    (run-compiles)
    (cl-flet ((overlays ()
                (seq-filter (lambda (overlay) (overlay-get overlay 'typst-overlay))
                            (overlays-in (point-min) (point-max)))))
      (let ((before (overlays))
            (reads 0))
        (cl-letf* ((read-svg (symbol-function 'typst-overlay--read-svg))
                   ((symbol-function 'typst-overlay--read-svg)
                    (lambda (path) (cl-incf reads) (funcall read-svg path))))
          (goto-char (point-min))
          (insert "New. ")
          (search-forward "$b$")
          (replace-match "$bb$" t t)
          (typst-overlay-refresh)
          (run-compiles))
        ;; $a$ and $c$ kept theirs; only $bb$ was read, once compiled.
        (should (= reads 1))
        (should (equal (typst-overlay-test--shown) '("$a$" "$bb$" "$c$")))
        (should (= (length (seq-intersection before (overlays))) 2))))))

(ert-deftest typst-overlay-test-equation-face ()
  "Images are shown in their own face; hidden, the source keeps its own."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "Text $a$ end." #'ignore
    (run-compiles)
    (let ((overlay (car (seq-filter (lambda (overlay) (overlay-get overlay 'typst-overlay))
                                    (overlays-in (point-min) (point-max))))))
      (goto-char (point-max))
      (typst-overlay--post-command-update)
      (should (eq (overlay-get overlay 'face) 'typst-overlay-equation))
      (goto-char (point-min))
      (search-forward "$a")
      (typst-overlay--post-command-update)
      (should-not (overlay-get overlay 'display))
      (should-not (overlay-get overlay 'face))
      (goto-char (point-max))
      (typst-overlay--post-command-update)
      (should (eq (overlay-get overlay 'face) 'typst-overlay-equation)))))

(ert-deftest typst-overlay-test-upward-entry ()
  "Only moving up by lines into an equation puts point at its end."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "Text $abcdef$ end." #'ignore
    (run-compiles)
    (cl-flet ((enter (command)
                ;; From after the equation, onto its start.
                (goto-char (point-max))
                (typst-overlay--post-command-update)
                (search-backward "$abc")
                (let ((this-command command))
                  (typst-overlay--post-command-update))
                (current-column)))
      ;; On its closing $.
      (should (= (enter 'previous-line) 12))
      ;; Searching or clicking puts point where it meant to.
      (should (= (enter 'isearch-repeat-backward) 5))
      (should (= (enter 'mouse-set-point) 5)))))

(ert-deftest typst-overlay-test-narrowed ()
  "Refreshing a narrowed buffer keeps the equations outside it."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "* A\n$a$\n* B\n$b$\n" #'ignore
    (run-compiles)
    (goto-char (point-min))
    (search-forward "* B")
    (narrow-to-region (point-min) (match-beginning 0))
    (typst-overlay-refresh)
    (run-compiles)
    (widen)
    (should (equal (typst-overlay-test--shown) '("$a$" "$b$")))))

(defun typst-overlay-test--preview ()
  "Describe the preview: its placement and the equation in its image.
Placement is \"beside\" or \"below\", or the result is nil if there is
no preview."
  (let ((preview typst-overlay--preview))
    (when (and preview (overlay-buffer preview))
      (let* ((string (overlay-get preview 'after-string))
             (data (plist-get (cdr (get-text-property 1 'display string)) :data)))
        (string-match "<!-- \\(\\(?:.\\|\n\\)*\\) -->" data)
        (list (if (eq (aref string 0) ?\n) "below" "beside")
              (match-string 1 data))))))

(ert-deftest typst-overlay-test-preview ()
  "The equation point is in is shown beside it, and kept while edited."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "Text $a$ and\n$\nc\n$ end." #'ignore
    (run-compiles)
    (cl-flet ((move (string)
                (goto-char (point-min))
                (search-forward string)
                (typst-overlay--post-command-update)))
      (move "$a")
      (should (equal (typst-overlay-test--preview) '("beside" "$a$")))
      (should (eq (get-text-property 1 'face (overlay-get typst-overlay--preview
                                                           'after-string))
                  'typst-overlay-preview))
      ;; Its overlay is gone while it is edited; the old image stays.
      (insert "x")
      (typst-overlay--post-command-update)
      (should (equal (typst-overlay-test--preview) '("beside" "$a$")))
      ;; Rendered again, while point is still inside.
      (typst-overlay-refresh)
      (run-compiles)
      (typst-overlay--post-command-update)
      (should (equal (typst-overlay-test--preview) '("beside" "$ax$")))
      (move "\nc")
      (should (equal (typst-overlay-test--preview) '("below" "$\nc\n$")))
      (move " end")
      (should-not (typst-overlay-test--preview))
      (let ((typst-overlay-preview nil))
        (move "$a")
        (should-not (typst-overlay-test--preview)))
      ;; Turning the mode off removes it.
      (move "$a")
      (should (typst-overlay-test--preview))
      (typst-overlay-mode -1)
      (should-not (typst-overlay-test--preview))
      (typst-overlay-mode 1))))

(ert-deftest typst-overlay-test-idle-refresh ()
  "Edits refresh the overlays once Emacs is idle, unless turned off."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((run-with-idle-timer (symbol-function 'run-with-idle-timer))
        timers)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn &rest args)
                 ;; Others, such as ElDoc, use idle timers too.
                 (if (not (eq fn #'typst-overlay--idle-refresh))
                     (apply run-with-idle-timer secs repeat fn args)
                   (push (list secs fn args) timers)
                   (timer-create)))))
      (typst-overlay-test--with-batches "$a$ and $b$" #'ignore
        (run-compiles)
        (setq compiled nil)
        ;; One refresh is scheduled for the edits typed until then.
        (goto-char (point-min))
        (search-forward "$b$")
        (replace-match "$c$" t t)
        (insert " more")
        (should (= (length timers) 1))
        (pcase-let ((`(,secs ,fn ,args) (car timers)))
          (should (= secs typst-overlay-idle-refresh-delay))
          (apply fn args))
        (run-compiles)
        (should (equal compiled '("$c$")))
        (should (equal (typst-overlay-test--shown) '("$a$" "$c$")))
        ;; Turning the mode off cancels a pending refresh.
        (insert "x")
        (should (= (length timers) 2))
        (typst-overlay-mode -1)
        (should-not typst-overlay--idle-timer)
        (should-not (memq #'typst-overlay--schedule-refresh after-change-functions))
        (typst-overlay-mode 1))
      (setq timers nil)
      (let ((typst-overlay-idle-refresh-delay nil))
        (typst-overlay-test--with-batches "$a$" #'ignore
          (insert "x")
          (should-not timers))))))

(ert-deftest typst-overlay-test-flymake ()
  "Failed equations are reported to Flymake, and still marked as failed."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-use-flymake t))
   (typst-overlay-test--with-batches
      (replace-regexp-in-string "x_2" "BAD" (typst-overlay-test--equations 3))
      (typst-overlay-test--error-at "BAD" "unknown variable: BAD")
    (cl-flet ((diagnostics ()
                (mapcar (lambda (diag)
                          (list (buffer-substring-no-properties
                                 (flymake-diagnostic-beg diag)
                                 (flymake-diagnostic-end diag))
                                ;; Flymake 1.4 and later add the
                                ;; origin to the text.
                                (if (fboundp 'flymake-diagnostic-message)
                                    (flymake-diagnostic-message diag)
                                  (flymake-diagnostic-text diag))))
                        (flymake-diagnostics)))
              (error-face ()
                (overlay-get (car (typst-overlay--error-overlays)) 'face)))
      (should (memq #'typst-overlay-flymake flymake-diagnostic-functions))
      ;; Newer Flymake only runs backends marked safe in untrusted buffers.
      (should (function-get 'typst-overlay-flymake 'flymake-always-safe))
      (flymake-mode 1)
      (flymake-start)
      ;; Errors from compiles finishing after the check are reported.
      (should-not (diagnostics))
      (run-compiles)
      (should (equal (diagnostics) '(("$BAD$" "unknown variable: BAD"))))
      (should (eq (error-face) 'typst-overlay-error))
      (flymake-mode -1)
      (flymake-mode 1)
      (flymake-start)
      (should (equal (diagnostics) '(("$BAD$" "unknown variable: BAD"))))
      ;; Turning the mode off clears its diagnostics.
      (typst-overlay-mode -1)
      (should-not (diagnostics))
      (should-not (memq #'typst-overlay-flymake flymake-diagnostic-functions))
      (flymake-mode -1)))))

(ert-deftest typst-overlay-test-flymake-opt-in ()
  "The backend is added where `typst-overlay-use-flymake' applies.
By default, in org buffers only."
  (skip-unless (treesit-language-available-p 'typst))
  (cl-flet ((added-p (setting mode)
              (let ((typst-overlay-use-flymake setting))
                (with-temp-buffer
                  (funcall mode)
                  (typst-overlay--add-flymake-backend)
                  (and (memq #'typst-overlay-flymake flymake-diagnostic-functions)
                       t)))))
    (should (eq (default-value 'typst-overlay-use-flymake) 'org))
    (should (added-p 'org #'org-mode))
    (should-not (added-p 'org #'fundamental-mode))
    (should (added-p t #'fundamental-mode))
    (should-not (added-p nil #'org-mode))))

(ert-deftest typst-overlay-test-flymake-eglot ()
  "With Eglot, the backend reports alongside its language server.
This holds whether Eglot starts before or after the mode."
  (skip-unless (treesit-language-available-p 'typst))
  (let ((typst-overlay-use-flymake t)
        (managed nil)
        saved)
   ;; Eglot says so while it manages the buffer, as 0.5.0 relied on.
   (cl-letf (((symbol-function 'eglot-managed-p) (lambda () managed)))
    (cl-flet ((eglot-on ()
                ;; What Eglot does when it starts managing the buffer.
                (setq managed t
                      saved flymake-diagnostic-functions)
                (setq-local flymake-diagnostic-functions '(eglot-flymake-backend))
                (run-hooks 'eglot-managed-mode-hook))
              (eglot-off ()
                (setq managed nil)
                (setq-local flymake-diagnostic-functions saved)
                (run-hooks 'eglot-managed-mode-hook)))
      ;; Eglot starts after the mode.
      (typst-overlay-test--with-batches (typst-overlay-test--equations 1) #'ignore
        (should (equal flymake-diagnostic-functions '(typst-overlay-flymake t)))
        (eglot-on)
        (should (equal flymake-diagnostic-functions
                       '(typst-overlay-flymake eglot-flymake-backend)))
        (eglot-off)
        (should (equal flymake-diagnostic-functions '(typst-overlay-flymake t))))
      ;; Eglot starts before the mode.
      (typst-overlay-test--with-batches (typst-overlay-test--equations 1) #'ignore
        (typst-overlay-mode -1)
        (eglot-on)
        (typst-overlay-mode 1)
        (should (equal flymake-diagnostic-functions
                       '(typst-overlay-flymake eglot-flymake-backend)))
        (eglot-off)
        (should (memq #'typst-overlay-flymake flymake-diagnostic-functions))
        (typst-overlay-mode -1)
        (should-not (memq #'typst-overlay--add-flymake-backend
                          eglot-managed-mode-hook)))))))

(ert-deftest typst-overlay-test-cache-dir ()
  "Images are kept in a folder per file folder, in the cache directory."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "Text $x$." #'ignore
    (run-compiles)
    ;; Nothing is written next to the file.
    (should (equal (directory-files dir nil "\\`[^.]") '("cache")))
    (should (equal (sort (mapcar #'file-name-nondirectory
                                 (directory-files-recursively typst-overlay-cache-directory
                                                              "" t))
                         #'string<)
                   (list (typst-overlay--cache-dir-name (file-name-as-directory dir))
                         (concat (typst-overlay-element-cache-key
                                  (typst-overlay-record-element
                                   (car (hash-table-values (typst-overlay-registry-records
                                                            typst-overlay--registry)))))
                                 ".svg"))))
    ;; Buffers without a file, such as org-capture's, by their folder.
    (let ((default-directory (file-name-as-directory (expand-file-name "tmp" dir))))
      (make-directory default-directory)
      (typst-overlay-mode -1)
      (setq buffer-file-name nil)
      (typst-overlay-mode 1)
      (run-compiles)
      (should (equal (typst-overlay-test--shown) '("$x$")))
      (should (file-directory-p (typst-overlay--cache-dir)))
      (should (string-suffix-p "!tmp!" (typst-overlay--cache-dir))))))

(ert-deftest typst-overlay-test-cache-dir-name ()
  "Cache folders are named after their folder's path, as backup files are."
  (should (equal (typst-overlay--cache-dir-name "/home/me/notes/") "!home!me!notes!"))
  (should (equal (typst-overlay--cache-dir-name "/a!b/") "!a!!b!"))
  ;; Windows drives, as : is not allowed in file names there.
  (should (equal (typst-overlay--cache-dir-name "c:/Users/me/") "!drive_c!Users!me!"))
  ;; Too long a path is hashed.
  (let ((dir (concat "/" (make-string 120 ?a) "/")))
    (should (equal (typst-overlay--cache-dir-name dir)
                   (md5 (concat "!" (make-string 120 ?a) "!"))))))

(ert-deftest typst-overlay-test-prune-cache ()
  "Images not used for a while are deleted, and nothing else is."
  (let* ((typst-overlay-cache-directory
          (file-name-as-directory (make-temp-file "typst-overlay-test-" t)))
         (typst-overlay-cache-max-age 30)
         (old (time-subtract nil (days-to-time 31)))
         (key (md5 "x")))
    (cl-flet ((make (path &optional time)
                (let ((file (expand-file-name path typst-overlay-cache-directory)))
                  (make-directory (file-name-directory file) t)
                  (write-region "" nil file)
                  (when time (set-file-times file time))))
              (exists (path)
                (file-exists-p (expand-file-name path typst-overlay-cache-directory))))
      (unwind-protect
          (progn
            (make (concat "!a!/" key ".svg") old)
            (make (concat "!b!/" key ".svg") old)
            (make (concat "!b!/" (md5 "y") ".svg"))
            (make "!c!/notes.svg" old)
            (make (concat "other/" key ".svg") old)
            (make-directory (expand-file-name "!empty!" typst-overlay-cache-directory))
            (make-directory (expand-file-name "empty" typst-overlay-cache-directory))
            (typst-overlay--prune-cache)
            ;; Old images and the folders they leave empty are deleted.
            (should-not (exists "!a!"))
            (should-not (exists (concat "!b!/" key ".svg")))
            (should-not (exists "!empty!"))
            ;; Recent images are kept, as is anything not named as the
            ;; cache names it.
            (should (exists (concat "!b!/" (md5 "y") ".svg")))
            (should (exists "!c!/notes.svg"))
            (should (exists (concat "other/" key ".svg")))
            (should (exists "empty"))
            ;; Never, if turned off.
            (make (concat "!a!/" key ".svg") old)
            (let ((typst-overlay-cache-max-age nil))
              (typst-overlay--prune-cache))
            (should (exists (concat "!a!/" key ".svg"))))
        (delete-directory typst-overlay-cache-directory t)))))

(ert-deftest typst-overlay-test-mark-used ()
  "Showing a cached image marks it used, so it is not pruned."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "Text $x$." #'ignore
    (run-compiles)
    (let ((file (car (directory-files-recursively typst-overlay-cache-directory
                                                  "\\.svg\\'")))
          (old (time-subtract nil (days-to-time 31))))
      (set-file-times file old)
      (clrhash typst-overlay--used-images)
      (typst-overlay-mode -1)
      (typst-overlay-mode 1)
      (should (time-less-p old (file-attribute-modification-time
                                (file-attributes file))))
      (typst-overlay--prune-cache)
      (should (file-exists-p file)))))

(ert-deftest typst-overlay-test-schedule-prune ()
  "The cache is pruned at most once a day, when Emacs is idle."
  (let ((typst-overlay--last-prune nil)
        (typst-overlay-cache-max-age 30)
        timers)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_secs _repeat fn &rest _) (push fn timers))))
      (typst-overlay--schedule-prune)
      (typst-overlay--schedule-prune)
      (should (equal timers '(typst-overlay--prune-cache)))
      (setq typst-overlay--last-prune (time-subtract nil (days-to-time 2)))
      (typst-overlay--schedule-prune)
      (should (= (length timers) 2))
      (let ((typst-overlay-cache-max-age nil)
            (typst-overlay--last-prune nil))
        (typst-overlay--schedule-prune)
        (should (= (length timers) 2))))))

(ert-deftest typst-overlay-test-eldoc-org ()
  "ElDoc is turned on in org buffers, which have no ElDoc function."
  (skip-unless (treesit-language-available-p 'typst))
  (typst-overlay-test--with-batches "Text $x$." #'ignore
    (should eldoc-mode)
    (should (memq #'typst-overlay--eldoc eldoc-documentation-functions))
    (typst-overlay-mode -1)
    (should-not (memq #'typst-overlay--eldoc eldoc-documentation-functions))))

(ert-deftest typst-overlay-test-read-svg ()
  "Black is drawn in the text color; other colors are kept."
  (let ((file (make-temp-file "typst-overlay-test-" nil ".svg"
                              "<path fill=\"#000000\" stroke=\"#000000\"/><path fill=\"#ff0000\"/>")))
    (unwind-protect
        (should (equal (typst-overlay--read-svg file)
                       "<path fill=\"currentColor\" stroke=\"currentColor\"/><path fill=\"#ff0000\"/>"))
      (delete-file file))))

;;; Fonts

(defconst typst-overlay-test--font-args-cases
  '(("default: system fonts skipped" nil nil ("--ignore-system-fonts"))
    ("system fonts" t nil ())
    ("font folders" nil ("/a" "/b")
     ("--ignore-system-fonts" "--font-path" "/a" "--font-path" "/b")))
  "Font argument cases: (NAME SYSTEM-FONTS FONT-PATHS EXPECTED).")

(ert-deftest typst-overlay-test-font-args ()
  "Typst skips the system's fonts unless asked, and searches the font folders."
  (dolist (case typst-overlay-test--font-args-cases)
    (pcase-let ((`(,name ,system-fonts ,font-paths ,expected) case))
      (let ((typst-overlay-system-fonts system-fonts)
            (typst-overlay-font-paths font-paths))
        (should (equal (list name expected)
                       (list name (typst-overlay--font-args))))))))

(ert-deftest typst-overlay-test-font-cache-key ()
  "Font options change the cache key, but the defaults keep earlier keys."
  (let* ((node (make-typst-overlay-math-node :beg 1 :end 4 :text "$x$"
                                             :text-hash (md5 "$x$")))
         (key (lambda () (typst-overlay-element-cache-key
                          (typst-overlay--make-element node nil))))
         (typst-overlay-extra-prelude ""))
    (should (equal (funcall key)
                   (md5 (concat (md5 "$x$") (md5 "") (md5 "")
                                typst-overlay--render-version))))
    (let ((typst-overlay-system-fonts t))
      (should-not (equal (funcall key)
                         (md5 (concat (md5 "$x$") (md5 "") (md5 "")
                                      typst-overlay--render-version)))))))

(ert-deftest typst-overlay-test-unknown-font-warning ()
  "A font typst could not find is warned about once, unless system fonts are on."
  (let ((output (concat "<stdin>:4:16: warning: unknown font family: fira math\n"
                        "<stdin>:9:16: warning: unknown font family: fira math\n"))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      (let ((typst-overlay--warned-fonts nil))
        (let ((typst-overlay-system-fonts t))
          (typst-overlay--warn-unknown-fonts output))
        (should-not warnings)
        (typst-overlay--warn-unknown-fonts output)
        (typst-overlay--warn-unknown-fonts output)
        (should (= (length warnings) 1))
        (should (string-search "\"fira math\"" (car warnings)))))))

;;; Image size

(defconst typst-overlay-test--image-size-cases
  '(("real typst header, default scale"
     "<svg viewBox=\"0 0 13.2893 11.2268\" width=\"13.2893pt\" height=\"11.2268pt\" xmlns=\"http://www.w3.org/2000/svg\"><g/></svg>"
     1.3 (:height (1.3268 . em)))
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

;;; Baseline

(defconst typst-overlay-test--ascent-cases
  '(("no descender: almost all above the baseline"
     "<a href=\"typst-overlay-depth:0.121\"/>" 6.983 84)
    ("descender" "<a href=\"typst-overlay-depth:2.255\"/>" 9.117 64)
    ("rounding error below zero" "<a href=\"typst-overlay-depth:-1e-16\"/>" 7.0 86)
    ("display equation: no depth" "<g/>" 24.11 center)
    ("depth deeper than the image is clamped"
     "<a href=\"typst-overlay-depth:50\"/>" 10.0 0))
  "Ascent cases: (NAME SVG HEIGHT EXPECTED).
HEIGHT is the image's height in points, including its 1pt margins.")

(ert-deftest typst-overlay-test-image-ascent ()
  "Inline equations sit on the baseline; others are centered."
  (dolist (case typst-overlay-test--ascent-cases)
    (pcase-let ((`(,name ,svg ,height ,expected) case))
      (should (equal (list name expected)
                     (list name (typst-overlay--image-ascent svg height)))))))

(provide 'typst-overlay-test)

;;; typst-overlay-test.el ends here
