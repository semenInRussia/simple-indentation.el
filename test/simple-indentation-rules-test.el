;;; simple-indentation-rules-test.el --- Tests for simple-indentation.el

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for simple-indentation-rules.el

;;; Code:

(require 'ert)
(require 'mocker)

(require 'simple-indentation)

(ert-deftest simple-indentation-rules-test-predicate
    ()
  (let ((rule
         (simple-indentation-rules-make
          :predicate (lambda () (eq 0 (% (line-number-at-pos) 2))))))
    (with-temp-buffer
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-multi-predicate
    ()
  (let ((rule
         (simple-indentation-rules-make
          :predicate (lambda () (eq 0 (% (line-number-at-pos) 2)))
          :predicate (lambda () (eq 0 (% (line-number-at-pos) 3))))))
    (with-temp-buffer
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-default-predicate
    ()
  (let ((rule (simple-indentation-rules-make)))
    (with-temp-buffer
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-add-predicate
    ()
  (let ((rule (simple-indentation-rules-make)))
    (setq rule
          (simple-indentation-rules-add-predicate
           (lambda () (eq 0 (% (line-number-at-pos) 2)))
           rule))
    (with-temp-buffer
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-chars
    ()
  (let ((rule (simple-indentation-rules-make :on-chars "}{[]()")))
    (with-temp-buffer
      (insert "I am semen []")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-chars-in-code
    ()
  (let ((rule (simple-indentation-rules-make :on-chars-in-code "!")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "I am semen!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "; I am semen!")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-chars-in-code-regexp-symbols
    ()
  (let ((rule (simple-indentation-rules-make :on-chars-in-code "[")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "I am semen [tell]!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "; I am semen [tell]!")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-keywords
    ()
  (let ((rule
         (simple-indentation-rules-make :on-keywords "lisp" "rust")))
    (with-temp-buffer
      (insert "I am lisp or rust coder")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-keywords-separated-with-slash
    ()
  (let ((rule
         (simple-indentation-rules-make :on-keywords "lisp" "rust")))
    (with-temp-buffer
      (insert "I am not lisp/rust coder")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-keywords-single-keyword
    ()
  (let ((rule (simple-indentation-rules-make :on-keywords "word")))
    (with-temp-buffer
      (insert "word")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-keywords-in-code
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-keywords-in-code "lisp" "rust")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "I am lisp or rust coder")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (beginning-of-line)
      (insert ";; ")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-check-on-prev-line
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "1"
          :check-on-prev-line)))
    (with-temp-buffer
      (insert "line0")
      (insert "line1")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "line2")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-check-on-prev-text-line
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "1"
          :check-on-prev-text-line)))
    (with-temp-buffer
      (insert "line0")
      (insert "line1")
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "\t\t ")            ; Empty Line
      (newline)
      (insert "line2")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-check-on-prev-code-line
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "1"
          :check-on-prev-code-line)))
    (with-temp-buffer
      (insert "line1")
      (newline)
      (insert ";; I am rubbish")
      (insert ";; I am rubbish too")
      (insert "line2")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-current-or-previous-line
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "!"
          :on-current-or-previous-line)))
    (with-temp-buffer
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (insert "!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-current-or-previous-text-line
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "!"
          :on-current-or-previous-text-line)))
    (with-temp-buffer
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (insert "!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "\t\t\t")
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-on-current-or-previous-code-line
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "!"
          :on-current-or-previous-code-line)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (insert "!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (insert "; Sjrfjregfjrgjrigj ome kdkd keifj iine...")
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-indent-func
    ()
  (let ((rule
         (simple-indentation-rules-make
          :indent-func (lambda () (insert "  ")))))
    (with-temp-buffer
      (simple-indentation-rules-call-indent-function rule)
      (should (equal (buffer-string) "  ")))))

(ert-deftest simple-indentation-rules-test-default-indent-func
    ()
  (let ((rule (simple-indentation-rules-make)))
    (with-temp-buffer
      (insert "Cool Line")
      (simple-indentation-rules-call-indent-function rule)
      (should (equal (thing-at-point 'line t) "Cool Line"))
      (newline)
      (insert "Cool Line 2")
      (simple-indentation-rules-call-indent-function rule)
      (should (equal (thing-at-point 'line t) "Cool Line 2")))))

(ert-deftest simple-indentation-rules-test-add-indent
    ()
  (let ((rule (simple-indentation-rules-make :add-indent)))
    (with-temp-buffer
      (setq-local simple-indentation-increment-indent-level-function
                  (lambda () (insert "  ")))
      (simple-indentation-rules-call-indent-function rule)
      (should (equal (buffer-string) "  ")))))

(ert-deftest simple-indentation-rules-test-deindent
    ()
  (let ((rule (simple-indentation-rules-make :deindent)))
    (with-temp-buffer
      (setq-local simple-indentation-decrement-indent-level-function
                  (lambda () (beginning-of-buffer) (delete-char 2)))
      (insert "  My Cool Text")
      (simple-indentation-rules-call-indent-function rule)
      (should (equal (thing-at-point 'line t) "My Cool Text")))))

(ert-deftest simple-indentation-rules-test-or
    ()
  (let ((rule
         (simple-indentation-rules-make
          :predicate (lambda () (eq 0 (% (line-number-at-pos) 2)))
          :or :on-chars "!")))
    (with-temp-buffer
      (insert "Indent This line (1)!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "I am even (2)")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-not
    ()
  (let ((rule (simple-indentation-rules-make :not :on-chars "!")))
    (with-temp-buffer                   ;nofmt
      (insert "Line without indent!")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "Line with indent (no exclamation point)")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-not-more-complex
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "."
          :and :not :on-chars "!")))
    (with-temp-buffer                   ;nofmt
      (insert "Line without indent!")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "Line also without indent because hasn't dot")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "Line without indent too.  because has !")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "Line with indent...")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-or-with-change-excusion
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "*"
          :check-on-prev-line :or
          :on-chars "!"
          :check-on-prev-line)))
    (with-temp-buffer
      (insert "Indent This line!")
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "I am *")
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-and
    ()
  (let ((rule
         (simple-indentation-rules-make
          :predicate (lambda () (eq 0 (% (line-number-at-pos) 2)))
          :and :on-chars "!")))
    (with-temp-buffer
      (insert "Indent This line (1), but I amn't even!")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "I am even (2)!")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (newline)
      (insert "I am even, but not aggresive (4)")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-and-with-change-excusion
    ()
  (let ((rule
         (simple-indentation-rules-make
          :on-chars "*"
          :check-on-prev-line :and
          :on-chars "!"
          :check-on-prev-line)))
    (with-temp-buffer
      (insert "Indent This line!")
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "I am * !")
      (newline)
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "I am even, but not aggresive*")
      (newline)
      (should-not
       (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-begin-end
    ()
  (let ((rule
         (simple-indentation-rules-make :begin
                                        :on-chars "l"
                                        :on-keywords "lisp"
                                        :end

                                        :and

                                        :begin :on-chars "r"
                                        :on-keywords "rust"
                                        :end)))
    (with-temp-buffer
      (insert "I am l")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (insert "and r")
      (should (simple-indentation-rules-indent-current-line-p rule))
      (newline)
      (insert "I am lisp")
      (should-not
       (simple-indentation-rules-indent-current-line-p rule))
      (insert "and rust")
      (should (simple-indentation-rules-indent-current-line-p rule)))))

(ert-deftest simple-indentation-rules-test-union
    ()
  (mocker-let
      ((f1 () ((:occur 1)))
       (f3 () ((:occur 1)))
       (f2 () ((:occur 1))))
    (let* ((rule-1
            (simple-indentation-rules-make :indent-func 'f1
                                           :on-chars "1"))
           (rule-2
            (simple-indentation-rules-make :indent-func 'f2
                                           :on-chars "2"))
           (rule-3
            (simple-indentation-rules-make :indent-func 'f3
                                           :on-chars "3"))
           (rules-union
            (simple-indentation-rules-union rule-1 rule-2 rule-3)))
      (with-temp-buffer
        (insert "1")
        (simple-indentation-rules-do rules-union)
        (newline)
        (insert "3")
        (simple-indentation-rules-do rules-union)
        (newline)
        (insert "2")
        (simple-indentation-rules-do rules-union)
        (newline)))))

(ert-deftest simple-indentation-rules-test-union-with-rules-predicates-changing-position
    ()
  (let* ((simple-indentation-increment-indent-level-function
          (lambda () (insert "  ")))
         (simple-indentation-decrement-indent-level-function
          (lambda () (delete-char 2)))
         (rule-adding-indent
          (simple-indentation-rules-make
           :add-indent :on-chars "{"
           :check-on-prev-line))
         (deindenting-rule
          (simple-indentation-rules-make :deindent :on-chars "}"))
         (rule
          (simple-indentation-rules-union rule-adding-indent
                                          deindenting-rule)))
    (with-temp-buffer
      (insert "{")
      (simple-indentation-rules-do rule)
      (should (equal (thing-at-point 'line) "{"))
      (newline)
      (insert "code...")
      (simple-indentation-rules-do rule)
      (should (equal (thing-at-point 'line) "  code..."))
      (newline)
      (insert "  }")
      (simple-indentation-rules-do rule)
      (should (equal (thing-at-point 'line) "}")))))

;;; simple-indentation-rules-test.el ends here
