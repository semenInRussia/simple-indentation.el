;;; simple-indention-hooks-test.el --- Tests for simple-indention.el

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
(require 'simple-indention-hooks)
(require 'mocker)

(ert-deftest simple-indention-hooks-test-union-to ()
    (mocker-let ((tst-func () ((:occur 2))))
        (let ((tst-a-hook nil)
              (tst-b-hook nil)
              (tst-c-hook nil))
            (simple-indention-hooks-union-to 'tst-c-hook
                                        ; ---
                                             'tst-a-hook
                                             'tst-b-hook)
            (add-to-list 'tst-c-hook 'tst-func)
            (run-hooks 'tst-b-hook)
            (run-hooks 'tst-a-hook))))


(ert-deftest simple-indention-hooks-test-union-from-namespace-to ()
    (mocker-let ((tst-func () ((:occur 2))))
        (let ((tst-a-hook nil)
              (tst-b-hook nil)
              (tst-c-hook nil))
            (simple-indention-hooks-union-from-namespace-to tst
                                                            c-hook
                                        ; ---
                                                            a-hook
                                                            b-hook)
            (add-to-list 'tst-c-hook 'tst-func)
            (run-hooks 'tst-b-hook)
            (run-hooks 'tst-a-hook))))


(ert-deftest simple-indention-hooks-test-union-from ()
    (should (eq 'python-mode-hook
                (simple-indention-hooks-from-namespace python-mode))))


(ert-deftest simple-indention-hooks-test-run-from-namespace ()
    (mocker-let ((tst-func () ((:occur 1))))
        (let ((tst-super-hook '(tst-func)))
            (simple-indention-hooks-run-from-namespace tst super-hook))))


(ert-deftest simple-indention-hooks-test-add-to-hook-from-namespace ()
    (let ((tst-super-hook nil))
        (simple-indention-hooks-add-to-hook-from-namespace tst super-hook
                                                           'tst-func)
        (should (equal tst-super-hook '(tst-func)))))


(ert-deftest simple-indention-hooks-test-remove-from-hook-from-namespace ()
    (let ((tst-super-hook '(tst-func)))
        (simple-indention-hooks-remove-from-hook-from-namespace tst super-hook
                                                                'tst-func)
        (should (equal tst-super-hook nil))))


(ert-deftest simple-indention-hooks-test-from-namespace-add-hook ()
    (let ((tst-super-hook nil))
        (simple-indention-hooks-from-namespace-add-hook tst
                                                        super-hook
                                                        func)
        (should (equal tst-super-hook '(tst-func)))))

(ert-deftest simple-indention-hooks-test-from-namespace-remove-hook ()
    (let ((tst-super-hook '(tst-func)))
        (simple-indention-hooks-from-namespace-remove-hook tst
                                                           super-hook
                                                           func)
        (should (equal tst-super-hook nil))))


;;; simple-indention-hooks-test.el ends here
