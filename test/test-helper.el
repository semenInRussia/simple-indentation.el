;;; test-helper.el --- Helper functions to test simple-indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'simple-indentation-namespace)

(declare-function undercover "undercover")

(defmacro simple-indentation-with-define-for-major-mode (args &rest body)
    "Set indent things in BODY to funcs created by `define-for-major-mode' ARGS.
Indent things:
- `indent-region-function'
- `indent-line-function'
- `each-line-before-indent-hook'
- `each-line-after-indent-hook'"
    (let* ((special-major-mode (gensym))
           (new-indent-region
            (simple-indentation-namespace-for-symbols special-major-mode
                                                      'indent-region))
           (new-indent-line
            (simple-indentation-namespace-for-symbols special-major-mode
                                                      'indent-line))
           (each-line-before-indent-hook
            (simple-indentation-namespace-for-symbols
             special-major-mode 'each-line-before-indent-hook))
           (each-line-after-indent-hook
            (simple-indentation-namespace-for-symbols
             special-major-mode 'each-line-after-indent-hook)))
        `(with-temp-buffer
             (let ((each-line-before-indent-hook ',each-line-before-indent-hook)
                   (each-line-after-indent-hook ',each-line-after-indent-hook))
                 (simple-indentation-define-for-major-mode ,special-major-mode
                                                           ,special-major-mode
                                                           ,@args)
                 (setq-local indent-line-function ',new-indent-line)
                 (setq-local indent-region-function ',new-indent-region)
                 ,@body))))


(defun simple-indentation-test-always-42 (&rest _)
    "Always, always return 42."
    42)

(when (require 'undercover nil t)
    (undercover "simple-indentation.el"))


;;; test-helper.el ends here
