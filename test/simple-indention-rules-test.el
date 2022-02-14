;;; simple-indention-rules-test.el --- Tests for simple-indention.el

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

;; Tests for simple-indention-rules.el

;;; Code:

(require 'ert)
(require 'simple-indention)


(ert-deftest simple-indention-rules-test-predicate ()
    (let ((rule (simple-indention-rules-make
                 :predicate (lambda () (eq 0 (% (line-number-at-pos) 2))))))
        (with-temp-buffer
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-multi-predicate ()
    (let ((rule (simple-indention-rules-make
                 :predicate (lambda () (eq 0 (% (line-number-at-pos) 2)))
                 :predicate (lambda () (eq 0 (% (line-number-at-pos) 3))))))
        (with-temp-buffer
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-default-predicate ()
    (let ((rule (simple-indention-rules-make)))
        (with-temp-buffer
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-add-predicate ()
    (let ((rule (simple-indention-rules-make)))
        (setq rule
              (simple-indention-rules-add-predicate (lambda ()
                                                        (eq
                                                         0
                                                         (% (line-number-at-pos)
                                                            2)))
                                                    rule))
        (with-temp-buffer
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-chars ()
    (let ((rule (simple-indention-rules-make :on-chars "}{[]()")))
        (with-temp-buffer
            (insert "I am semen []")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-chars-in-code ()
    (let ((rule (simple-indention-rules-make :on-chars-in-code "!")))
        (with-temp-buffer
            (emacs-lisp-mode)
            (insert "I am semen!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "; I am semen!")
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-chars-in-code-regexp-symbols ()
    (let ((rule (simple-indention-rules-make :on-chars-in-code "[")))
        (with-temp-buffer
            (emacs-lisp-mode)
            (insert "I am semen [tell]!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "; I am semen [tell]!")
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-keywords ()
    (let ((rule (simple-indention-rules-make :on-keywords "lisp" "rust")))
        (with-temp-buffer
            (insert "I am lisp or rust coder")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-keywords-without-spaces ()
    (let ((rule (simple-indention-rules-make :on-keywords "lisp" "rust")))
        (with-temp-buffer
            (insert "I am not lisp/rust coder")
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-keywords-single-keyword ()
    (let ((rule (simple-indention-rules-make :on-keywords "word")))
        (with-temp-buffer
            (insert "word")
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-keywords-multi-keyword ()
    (let ((rule (simple-indention-rules-make :on-keywords "super man")))
        (with-temp-buffer
            (insert "super man")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "super    man")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "super   man")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "superman")
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-keywords-in-code ()
    (let ((rule (simple-indention-rules-make
                 :on-keywords-in-code "lisp" "rust")))
        (with-temp-buffer
            (emacs-lisp-mode)
            (insert "I am lisp or rust coder")
            (should (simple-indention-rules-indent-current-line-p rule))
            (beginning-of-line)
            (insert ";; ")
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-check-on-prev-line ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "1"
                 :check-on-prev-line)))
        (with-temp-buffer
            (insert "line0")
            (insert "line1")
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "line2")
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-check-on-prev-text-line ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "1"
                 :check-on-prev-text-line)))
        (with-temp-buffer
            (insert "line0")
            (insert "line1")
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "\t\t ")            ; Empty Line
            (newline)
            (insert "line2")
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-check-on-prev-code-line ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "1"
                 :check-on-prev-code-line)))
        (with-temp-buffer
            (insert "line1")
            (newline)
            (insert ";; I am rubbish")
            (insert ";; I am rubbish too")
            (insert "line2")
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-current-or-previous-line ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "!"
                 :on-current-or-previous-line)))
        (with-temp-buffer
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (insert "!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-current-or-previous-text-line ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "!"
                 :on-current-or-previous-text-line)))
        (with-temp-buffer
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (insert "!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "\t\t\t")
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-on-current-or-previous-code-line ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "!"
                 :on-current-or-previous-code-line)))
        (with-temp-buffer
            (emacs-lisp-mode)
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (insert "!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (insert "; Sjrfjregfjrgjrigj ome kdkd keifj iine...")
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-indent-func ()
    (let ((rule (simple-indention-rules-make
                 :indent-func (lambda () (insert "  ")))))
        (with-temp-buffer
            (simple-indention-rules-call-indent-function rule)
            (should (equal (thing-at-point 'line t) "  ")))))


(ert-deftest simple-indention-rules-test-default-indent-func ()
    (let ((rule (simple-indention-rules-make)))
        (with-temp-buffer
            (insert "Cool Line")
            (simple-indention-rules-call-indent-function rule)
            (should (equal (thing-at-point 'line t) "Cool Line"))
            (newline)
            (insert "Cool Line 2")
            (simple-indention-rules-call-indent-function rule)
            (should (equal (thing-at-point 'line t) "Cool Line 2")))))


(ert-deftest simple-indention-rules-test-add-indent ()
    (let ((rule (simple-indention-rules-make :add-indent)))
        (with-temp-buffer
            (setq-local simple-indention-increment-indent-level-function
                        (lambda () (insert "  ")))
            (simple-indention-rules-call-indent-function rule)
            (should (equal (thing-at-point 'line t) "  ")))))


(ert-deftest simple-indention-rules-test-deindent ()
    (let ((rule (simple-indention-rules-make :deindent)))
        (with-temp-buffer
            (setq-local simple-indention-decrement-indent-level-function
                        (lambda ()
                            (beginning-of-buffer)
                            (delete-char 2)))
            (insert "  My Cool Text")
            (simple-indention-rules-call-indent-function rule)
            (should (equal (thing-at-point 'line t) "My Cool Text")))))


(ert-deftest simple-indention-rules-test-or ()
    (let ((rule (simple-indention-rules-make
                 :predicate (lambda () (eq 0 (% (line-number-at-pos) 2)))
                 :or
                 :on-chars "!")))
        (with-temp-buffer
            (insert "Indent This line (1)!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "I am even (2)")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-or-with-change-excusion ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "*"
                 :check-on-prev-line
                 :or
                 :on-chars "!"
                 :check-on-prev-line)))
        (with-temp-buffer
            (insert "Indent This line!")
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "I am *")
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-and ()
    (let ((rule (simple-indention-rules-make
                 :predicate (lambda () (eq 0 (% (line-number-at-pos) 2)))
                 :and
                 :on-chars "!")))
        (with-temp-buffer
            (insert "Indent This line (1), but I amn't even!")
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "I am even (2)!")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (newline)
            (insert "I am even, but not aggresive (4)")
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-and-with-change-excusion ()
    (let ((rule (simple-indention-rules-make
                 :on-chars "*"
                 :check-on-prev-line
                 :and
                 :on-chars "!"
                 :check-on-prev-line)))
        (with-temp-buffer
            (insert "Indent This line!")
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "I am * !")
            (newline)
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "I am even, but not aggresive*")
            (newline)
            (should-not (simple-indention-rules-indent-current-line-p rule)))))


(ert-deftest simple-indention-rules-test-begin-end ()
    (let ((rule (simple-indention-rules-make :begin
                                             :on-chars "l"
                                             :on-keywords "lisp"
                                             :end

                                             :and

                                             :begin
                                             :on-chars "r"
                                             :on-keywords "rust"
                                             :end)))
        (with-temp-buffer
            (insert "I am l")
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (insert "and r")
            (should (simple-indention-rules-indent-current-line-p rule))
            (newline)
            (insert "I am lisp")
            (should-not (simple-indention-rules-indent-current-line-p rule))
            (insert "and rust")
            (should (simple-indention-rules-indent-current-line-p rule)))))



;;; simple-indention-rules-test.el ends here
