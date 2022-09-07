;;; simple-indentation-utils-test.el --- Tests for simple-indentation.el

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

;; Tests for simple-indentation-utils.el

;;; Code:

(require 'ert)
(require 'simple-indentation-utils)

(ert-deftest simple-indentation-utils-test-previous-line
    ()
  (with-temp-buffer
    (insert "\n")
    (should (simple-indentation-utils-previous-line))
    (should (eq (point-min) (point)))))

(ert-deftest simple-indentation-utils-test-previous-line-in-bob
    ()
  (with-temp-buffer
    (should-not (simple-indentation-utils-previous-line))
    (should (eq (point-min) (point)))))

(ert-deftest simple-indentation-utils-test-clear-indention
    ()
  (with-temp-buffer
    (insert "    this is 4 spaces, this is indention")
    (simple-indentation-utils-clear-indention)
    (should
     (equal
      (thing-at-point 'line t)
      "this is 4 spaces, this is indention"))))

(ert-deftest simple-indentation-utils-test-clear-indention-without-indention
    ()
  (with-temp-buffer
    (insert "No Indention!")
    (simple-indentation-utils-clear-indention)
    (should (equal (thing-at-point 'line t) "No Indention!"))))

(ert-deftest simple-indentation-utils-test-previous-text-line
    ()
  (with-temp-buffer
    (insert "Line 1")

    (insert "\n")
    (insert "   ")
    (insert "\n")                  ;This is empty lines
    (insert "\t")
    (insert "\n")

    (should (simple-indentation-utils-previous-text-line))
    (should (equal (thing-at-point 'line t) "Line 1\n"))))

(ert-deftest simple-indentation-utils-test-previous-text-line-in-bob
    ()
  (with-temp-buffer
    (insert "Line 1")
    (should-not (simple-indentation-utils-previous-text-line))
    (should (equal (thing-at-point 'line t) "Line 1"))))

(ert-deftest simple-indentation-utils-test-previous-code-line
    ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "Line 1")
    (newline)
    (insert ";; Ignore me")
    (newline)
    (insert ";; Ignore me alse")
    (newline)
    (should (simple-indentation-utils-previous-code-line))
    (should (equal (thing-at-point 'line t) "Line 1\n"))))

(ert-deftest simple-indentation-utils-test-previous-code-line-ignore-empty
    ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "Line 1")
    (newline)
    (insert "\t\t\t\t") ; Ignore me
    (newline)
    (insert "; Ignore me, please")
    (should (simple-indentation-utils-previous-code-line))
    (should (eq (line-number-at-pos (point)) 1))))

(ert-deftest simple-indentation-utils-test-previous-code-line-in-bob
    ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "Line 1")
    (should (not (simple-indentation-utils-previous-code-line)))))

(ert-deftest simple-indentation-utils-test-empty-current-line-p
    ()
  (with-temp-buffer
    (insert "Line 1")
    (should-not (simple-indentation-utils-empty-current-line-p))
    (insert "\n     \t")
    (should (simple-indentation-utils-empty-current-line-p))))

(ert-deftest simple-indentation-utils-test-empty-current-line-p-in-last-empty-line
    ()
  (with-temp-buffer
    (insert "Some Stupid Texts...")
    (newline)
    (should (simple-indentation-utils-empty-current-line-p))))

(ert-deftest simple-indentation-utils-test-empty-current-line-p
    ()
  (with-temp-buffer
    (insert "Line 1")
    (should-not (simple-indentation-utils-empty-current-line-p))
    (insert "\n     \t")
    (should (simple-indentation-utils-empty-current-line-p))))

(ert-deftest simple-indentation-utils-test-if-empty-clear
    ()
  (with-temp-buffer
    (insert "\t\t\t    ")           ; This is empty line
    (simple-indentation-utils-if-empty-clear)
    (should (equal (buffer-string) ""))
    (insert "Tesjtgjseijfjei fjxxt is Text")
    (should-not (equal (buffer-string) ""))))

(ert-deftest simple-indentation-utils-test-current-line
    ()
  (with-temp-buffer
    (insert "LINE")
    (should (equal (simple-indentation-utils-current-line) "LINE"))))

(ert-deftest simple-indentation-utils-test-current-line-in-last-empty-line
    ()
  (with-temp-buffer
    (insert "Some Clever Texts...")
    (newline)
    (should (equal (simple-indentation-utils-current-line) ""))))

(ert-deftest simple-indentation-utils-compose-with-prev-line
    ()
  (with-temp-buffer
    (insert "LINE 1")
    (newline)
    (insert "LINE 2")
    (should
     (equal
      (funcall
       (simple-indentation-utils-compose-with-prev-line
        'simple-indentation-utils-current-line))
      "LINE 1"))))

(ert-deftest simple-indentation-utils-compose-with-prev-text-line
    ()
  (with-temp-buffer
    ;; Text Line
    (insert "LINE 1")
    ;; Empty Lines
    (newline)
    (newline)
    (insert "\t\t ")
    (newline)
    ;; Text Line
    (insert "LINE 2")
    (should
     (equal
      (funcall
       (simple-indentation-utils-compose-with-prev-text-line
        'simple-indentation-utils-current-line))
      "LINE 1"))))

(ert-deftest simple-indentation-utils-compose-with-prev-code-line
    ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "LINE 1")
    (newline)
    (insert ";; Mussor")
    (newline)
    (insert "LINE 2")
    (should
     (equal
      (funcall
       (simple-indentation-utils-compose-with-prev-code-line
        'simple-indentation-utils-current-line))
      "LINE 1"))))

(ert-deftest simple-indentation-utils-test-get-indent-in-str
    ()
  (should
   (equal
    (simple-indentation-utils-get-indent-of-string "   CODE")
    "   "))
  (should
   (equal
    (simple-indentation-utils-get-indent-of-string "\t\t CODE")
    "\t\t ")))

(ert-deftest simple-indentation-utils-test-get-indent-in-str-without-indent
    ()
  (should
   (equal (simple-indentation-utils-get-indent-of-string "CODE") "")))

(ert-deftest simple-indentation-utils-test-get-indent-in-line
    ()
  (with-temp-buffer
    (insert "   Some Gavno Code...")
    (should
     (equal (simple-indentation-utils-get-indent-of-line) "   "))))

(ert-deftest simple-indentation-utils-duplicate-indention-of-prev-line
    ()
  (with-temp-buffer
    (insert "    This is 4 spaces, This is INDENT!")
    (newline)
    (simple-indentation-utils-duplicate-indention-of-prev-line)
    (should (equal (thing-at-point 'line t) "    "))))

(ert-deftest simple-indentation-utils-code-line-has-chars-p
    ()
  (with-temp-buffer
    (insert "I am coder ()")
    (should (simple-indentation-utils-code-line-has-chars-p "(|"))
    (emacs-lisp-mode)
    (beginning-of-line)
    (insert ";")
    (should-not (simple-indentation-utils-code-line-has-chars-p "("))))

(ert-deftest simple-indentation-utils-line-has-keywords
    ()
  (with-temp-buffer
    (insert "I am end")
    (should
     (simple-indentation-utils-line-has-keywords-p '("end" "final")))
    (newline)
    (insert "I am final")
    (should
     (simple-indentation-utils-line-has-keywords-p '("end" "final")))
    (newline)
    (insert "I am begin")
    (should-not
     (simple-indentation-utils-line-has-keywords-p '("end" "final")))))

(ert-deftest simple-indentation-utils-line-has-keywords-start
    ()
  (with-temp-buffer
    (insert "end final was ignored")
    (beginning-of-line)
    (should
     (not (simple-indentation-utils-line-has-keywords-p
           '("end" "final")
           10)))))

(ert-deftest simple-indentation-utils-code-line-has-keywords
    ()
  (with-temp-buffer
    (insert "I am end")
    (should
     (simple-indentation-utils-code-line-has-keywords
      '("end" "final")))
    (emacs-lisp-mode)
    (beginning-of-line)
    (insert ";")
    (should-not
     (simple-indentation-utils-code-line-has-keywords
      '("end" "final")))))

(ert-deftest simple-indentation-utils-test-compose-one-argument
    ()
  (should
   (eq (funcall (simple-indentation-utils-compose '1+ '1+) 1) 3)))

(ert-deftest simple-indentation-utils-test-compose-order
    ()
  (should
   (equal
    (funcall
     (simple-indentation-utils-compose 'number-to-string '1+)
     2)
    "3")))

(ert-deftest simple-indentation-utils-test-compose-zero-args
    ()
  (should
   (equal
    (funcall (simple-indentation-utils-compose '1+ (-const 3)))
    4)))

(ert-deftest simple-indentation-utils-test-compose-one-plus-args
    ()
  (should
   (equal (funcall (simple-indentation-utils-compose '1+ '+) 2 2) 5)))

(ert-deftest simple-indentation-utils-test-compose-rest-args
    ()
  (should
   (equal
    (funcall
     (simple-indentation-utils-compose 'length 's-concat)
     "a" "b" "c")
    3)))

(ert-deftest simple-indentation-utils-test-including-take-including-while
    ()
  (should
   (equal
    (simple-indentation-utils-take-including-while
     (lambda (x) (<= x 2))
     '(1 2 3 4 5))
    '(1 2 3))))

;;; simple-indentation-utils-test.el ends here
