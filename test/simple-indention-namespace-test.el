;;; simple-indention-namespace-test.el --- Tests for simple-indention.el

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

;; Tests for simple-indention-namespace.el

;;; Code:

(require 'ert)
(require 'simple-indention-namespace)

(ert-deftest simple-indention-namespace-test-from ()
    (should (eq (simple-indention-namespace-from python indent-line)
                'python-indent-line)))


(ert-deftest simple-indention-namespace-test-for-symbols ()
    (should (eq (simple-indention-namespace-for-symbols 'python 'indent-line)
                'python-indent-line)))


(ert-deftest simple-indention-namespace-test-for-symbols ()
    (should (equal (macroexpand-1
                    '(simple-indention-namespace-defun python print ()
                      "Doc."
                      (interactive "dkd")
                      (message "body")))
                   '(defun python-print ()
                     "Doc."
                     (interactive "dkd")
                     (message "body")))))


(ert-deftest simple-indention-namespace-test-for-symbols ()
    (should (equal (macroexpand-1
                    '(simple-indention-namespace-defcustom
                      python name
                      "Doc."
                      :group 'sheet))
                   '(defcustom python-name "Doc." :group 'sheet))))


(ert-deftest simple-indention-namespace-test-var()
    (should (equal (macroexpand-1
                    '(simple-indention-namespace-var
                      python stupid-var))
                   '(eval python-stupid-var))))


(ert-deftest simple-indention-namespace-test-setq ()
    (let ((tst-var 0))
        (simple-indention-namespace-setq tst var "Something.")
        (should (equal tst-var "Something."))))


(ert-deftest simple-indention-namespace-test-funcall ()
    ;; We have fun (simple-indention-test-always-42), it's always return 42
    (should (eq (simple-indention-namespace-funcall simple-indention-test
                                                    always-42)
                42)))

;;; simple-indention-namespace-test.el ends here
