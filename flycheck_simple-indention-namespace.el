;;; simple-indention-namespace.el --- Operations on namespaces -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/semenInRussia/simple-indention.el

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

;; (simple-indention-namespace-from python hook) => 'python-hook
;;
;; (funcall-from-namespace python indent-line-function)
;;       => (python-indent-function)

;;; Code:

(require 'dash)


(defmacro simple-indention-namespace-defun (namespace func-name
                                            arglist
                                            docstring
                                            &rest body)
    "Define function in `NAMESPACE` with name `FUNC-NAME`.
With respective `ARGLIST`, `DOCSTRING` and `BODY`."
    `(defun
         ,(simple-indention-namespace-for-symbols namespace func-name) ,arglist
         ,docstring
         ,@body))


(defmacro simple-indention-namespace-defcustom (namespace
                                                var-name
                                                standard
                                                docstring &rest args)
    "Define custom variable from `NAMESPACE`.
`VAR-NAME` is name of variable.  `STANDARD` is default value of variable.
`DOCSTRING` is documentation's string for variable.  `ARGS` is additional
arguments for `defcustom`."
    `(defcustom ,(simple-indention-namespace-for-symbols namespace var-name)
       ,standard
       ,docstring
       ,@args))


(defmacro simple-indention-namespace-funcall (namespace func-name &rest args)
    "Call function with FUNC-NAME from NAMESPACE with ARGS."
    `(,(simple-indention-namespace-for-symbols namespace func-name) ,@args))


(defmacro simple-indention-namespace-var (namespace var-name)
    "Get something symbol with `VAR-NAME` from `NAMESPACE`."
    `(eval ,(simple-indention-namespace-for-symbols namespace var-name)))


(defmacro simple-indention-namespace-setq (namespace var-name value)
    "Setq var with VAR-NAME from NAMESPACE to VALUE."
    `(set (simple-indention-namespace-from ,namespace ,var-name) ,value))


(defmacro simple-indention-namespace-from (namespace something-name)
    "Get something symbol with `SOMETHING-NAME` from `NAMESPACE`."
    `(intern ,(s-concat (symbol-name namespace)
                        "-"
                        (symbol-name something-name))))


(defun simple-indention-namespace-for-symbols
    (namespace-symb something-name-symb)
    "Get symbol with SOMETHING-NAME-SYMB from NAMESPACE-SYMB."
    (intern (s-concat (symbol-name namespace-symb)
                      "-"
                      (symbol-name something-name-symb))))

(provide 'simple-indention-namespace)
;;; simple-indention-namespace.el ends here
