;;; simple-indentation-test.el --- Tests for simple-indentation.el

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

;; Tests for simple-indentation.el

;;; Code:

(require 'ert)
(require 'simple-indentation)

(ert-deftest simple-indentation-test-define-for-major-mode-empty-rules
    ()
  (let ((text "this\ndon't\nindented"))
    (simple-indentation-with-define-for-major-mode
     (:rules nil)
     (insert text)
     (funcall indent-line-function)
     (should (equal (buffer-string) text))
     (indent-region (point-min) (point-max))
     (should (equal (buffer-string) text)))))

(ert-deftest simple-indentation-test-define-for-major-mode-clear-empty-lines
    ()
  (simple-indentation-with-define-for-major-mode
   nil
   (insert "folowing sheet

will cleared")
   (indent-region (point-min) (point-max))
   (should (equal (buffer-string) "folowing sheet

will cleared"))))

(ert-deftest simple-indentation-test-define-for-major-mode-not-clear-empty-lines
    ()
  (simple-indentation-with-define-for-major-mode
   (:clear-empty-lines nil :copy-indention-of-previous-line nil)
   (let ((text "folowing sheet

will not cleared"))
     (insert text)
     (indent-region (point-min) (point-max))
     (funcall indent-line-function)
     (should (equal (buffer-string) text)))))

(ert-deftest
    simple-indentation-define-for-major-mode-test-copy-indention-of-prev-line
    ()
  (simple-indentation-with-define-for-major-mode
   nil
   (insert "   this indent from 3 spaces will copied")
   (newline)
   (funcall indent-line-function)
   (should (equal "   " (thing-at-point 'line t)))))

(ert-deftest simple-indentation-define-for-major-mode-test-clear-old-indent
    ()
  (simple-indentation-with-define-for-major-mode
   (:clear-old-indention t :copy-indention-of-previous-line nil)
   (insert "   this indent cleared")
   (funcall indent-line-function)
   (should (equal "this indent cleared" (buffer-string)))))

(ert-deftest simple-indentation-define-for-major-mode-test-each-line-hooks
    ()
  (simple-indentation-with-define-for-major-mode
   nil
   (insert "\n\n\n")
   (add-hook each-line-before-indent-hook
             (lambda () (beginning-of-line) (insert "ANGEL")))
   (add-hook each-line-after-indent-hook
             (lambda () (beginning-of-line) (insert "B")))
   (add-hook each-line-after-indent-hook
             (lambda () (end-of-line) (backward-delete-char 2)))
   (indent-region (point-min) (point-max))
   (should (equal "BANG\nBANG\nBANG\n"(buffer-string)))))

(provide 'simple-indentation-test)

;;; simple-indentation-test.el ends here
