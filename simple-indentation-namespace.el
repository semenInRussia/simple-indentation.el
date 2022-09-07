;;; simple-indentation-namespace.el --- Operations on namespaces -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/semenInRussia/simple-indentation.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package created for work on namespaces, example of work on namespaces:

;; (simple-indentation-namespace-from python hook) => 'python-hook
;;
;; (funcall-from-namespace python indent-line-function)
;;       => (python-indent-function)

;;; Code:

(require 'dash)
(require 's)

(defmacro simple-indentation-namespace-defun ;nofmt
    (namespace func-name arglist docstring &rest body)
  "Define function in `NAMESPACE` with name `FUNC-NAME`.
With respective `ARGLIST`, `DOCSTRING` and `BODY`."
  `(defun ,(simple-indentation-namespace-for-symbols namespace func-name)
       ,arglist
     ,docstring
     ,@body))

(defmacro simple-indentation-namespace-defcustom ;nofmt
    (namespace var-name standard docstring &rest args)
  "Define custom variable from `NAMESPACE`.
`VAR-NAME` is name of variable.  `STANDARD` is default value of variable.
`DOCSTRING` is documentation's string for variable.  `ARGS` is additional
arguments for `defcustom`."
  `(defcustom ,(simple-indentation-namespace-for-symbols namespace var-name)
     ,standard
     ,docstring
     ,@args))

(defmacro simple-indentation-namespace-funcall (namespace func-name &rest args)
  "Call function with FUNC-NAME from NAMESPACE with ARGS."
  `(,(simple-indentation-namespace-for-symbols namespace func-name)
     ,@args))

(defmacro simple-indentation-namespace-var (namespace var-name)
  "Get something symbol with `VAR-NAME` from `NAMESPACE`."
  `(eval
    ,(simple-indentation-namespace-for-symbols namespace var-name)))

(defmacro simple-indentation-namespace-setq (namespace var-name value)
  "Setq var with VAR-NAME from NAMESPACE to VALUE."
  `(set
    (simple-indentation-namespace-from ,namespace ,var-name)
    ,value))

(defmacro simple-indentation-namespace-from (namespace something-name)
  "Get something symbol with `SOMETHING-NAME` from `NAMESPACE`."
  `(intern
    ,(s-concat
      (symbol-name namespace)
      "-"
      (symbol-name something-name))))

(defun simple-indentation-namespace-for-symbols ;nofmt
    (namespace-symb something-name-symb)
  "Get symbol with SOMETHING-NAME-SYMB from NAMESPACE-SYMB."
  (intern
   (s-concat
    (symbol-name namespace-symb)
    "-"
    (symbol-name something-name-symb))))

(provide 'simple-indentation-namespace)
;;; simple-indentation-namespace.el ends here
