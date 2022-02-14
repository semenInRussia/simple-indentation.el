;;; simple-indention-hooks.el --- Operations on hooks -*- lexical-binding: t; -*-

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

;; Operations on Emacs hooks.

;;; Code:

(require 'dash)

(require 'simple-indention-namespace)


(defmacro simple-indention-hooks-union-to (destination-hook &rest hooks)
    "Add hook to run DESTINATION-HOOK to HOOKS."
    `(--each (list ,@hooks)
        (add-hook it (lambda () (run-hooks ,destination-hook)))))


(defmacro simple-indention-hooks-union-from-namespace-to (namespace
                                                          dest-hook
                                                          &rest hooks)
    "Add hook to run DEST-HOOK to HOOKS from NAMESPACE.
Example:
    (simple-indention-hooks-union-from-namespace-to calc
                                   embeded-hook
                                   embeded-mode-hook
                                   embedded-new-buffer-hook)

    --→ (simple-indention-hooks-union-to 'calc-embedded-hook
                        'calc-embeded-mode-hook
                        'calc-embedded-new-buffer-hook)"
    (setq dest-hook
          (simple-indention-namespace-for-symbols namespace dest-hook))
    (->> hooks
         (--map (simple-indention-namespace-for-symbols namespace it))
         (-map (lambda (el) `(quote ,el)))
         (setq hooks))
    `(simple-indention-hooks-union-to ',dest-hook ,@hooks))


(defmacro simple-indention-hooks-add-to-hook-from-namespace
    (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (simple-indention-namespace-for-symbols namespace hook))
    `(add-hook ',hook ,function))


(defmacro simple-indention-hooks-from-namespace-add-hook
    (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (simple-indention-namespace-for-symbols namespace hook))
    (setq function (simple-indention-namespace-for-symbols namespace function))
    `(add-hook ',hook #',function))


(defmacro simple-indention-hooks-from-namespace-remove-hook
    (namespace hook function)
    "Remove to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not removed if already present."
    (setq hook (simple-indention-namespace-for-symbols namespace hook))
    `(remove-hook ',hook
                  #',(simple-indention-namespace-for-symbols namespace
                                                             function)))


(defmacro simple-indention-hooks-remove-from-hook-from-namespace
    (namespace hook function)
    "Add to the value of HOOK from NAMESPACE the function FUNCTION.
FUNCTION is not added if already present."
    (setq hook (simple-indention-namespace-for-symbols namespace hook))
    `(remove-hook ',hook ,function))


(defmacro simple-indention-hooks-run-from-namespace (namespace &rest hooks)
    "Run all HOOKS from NAMESPACE."
    (->> hooks
         (--map (simple-indention-namespace-for-symbols namespace it))
         (-map (lambda (el) `(quote ,el)))
         (setq hooks))
    `(run-hooks ,@hooks))

(defmacro simple-indention-hooks-from-namespace (namespace)
    "Return hooks of NAMESPACE.
For example:
python => python-hook;
clang => clang-hook;"
    `(simple-indention-namespace-from ,namespace hook))


(provide 'simple-indention-hooks)
;;; simple-indention-hooks.el ends here
