;;; simple-indentation.el --- Simplify writing indentation functions, alternative to SMIE  -*- lexical-bindings: t -*-

;; Copyright (C) 2022 semenInRussia

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; URL: https://github.com/semenInRussia/simple-indentation.el
;; Package-Requires: ((emacs "24.3") (dash "2.18.0") (s "1.12.0"))

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

;; Simplify writing major-mode indentation functions.  Alternative to `SMIE'

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 's)

(require 'simple-indentation-hooks)
(require 'simple-indentation-rules)
(require 'simple-indentation-namespace)

(defgroup simple-indentation nil
  "Package for easy define indention functions and vars."
  :group 'elisp)

(defcustom simple-indentation-default-one-indent "    "
  "Default indention when define for major mode."
  :group 'simple-indentation
  :type 'string)

(defvar simple-indentation-decrement-indent-level-function nil
  "This is function, which deindent current line.
Please, don't touch, this is change automatically.")

(defvar simple-indentation-increment-indent-level-function nil
  "This is function, which add indent to current line.
Please, don't touch, this is change automatically.")

(cl-defmacro simple-indentation-define-for-major-mode ;nofmt
    (major-mode
     &optional namespace
     &key
       rules
       (one-indent simple-indentation-default-one-indent)
       (copy-indention-of-previous-line t)
       (clear-old-indention nil)
       (clear-empty-lines t))
  "Create all variables and functions for indent code in `MAJOR-MODE`.
All vars and functions will save in NAMESPACE.  RULES is list of
rules which you can create with `indention/make-rule`.
For documentation of ONE-INDENT, COPY-INDENTION-OF-PREVIOUS-LINE,
CLEAR-OLD-INDENTION, CLEAR-EMPTY-LINES see docs."
  (let ((major-mode-name (symbol-name major-mode))
        (one-indent-of-mode
         `(eval
           (simple-indentation-namespace-from ,namespace
                                              one-indent))))
    `(progn
       (simple-indentation-namespace-defcustom
        ,namespace indention-rules
        nil
        ,(s-lex-format "Rules of indention for ${major-mode-name}.")
        :group ',major-mode
        :type '(repeat (list function function)))

       (simple-indentation-namespace-setq ,namespace
                                          indention-rules
                                          ,rules)

       (simple-indentation-namespace-defcustom
        ,namespace each-line-before-indent-hook
        nil
        "Hooks which run before each indent line."
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace each-line-after-indent-hook
        nil
        "Hooks which run after each indent line."
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace before-run-indent-func-hook
        nil
        "Hooks which run before runing indent function."
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace after-run-indent-func-hook
        nil
        "Hooks which run after runing indent function."
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace before-indent-line-hook
        nil
        ,(s-lex-format
          "This hooks run before `${major-mode-name}-indent-line'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace after-indent-line-hook
        nil
        ,(s-lex-format
          "This hooks run after `${major-mode-name}-indent-line'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace before-indent-region-hook
        nil
        ,(s-lex-format
          "This hooks run before `${major-mode-name}-indent-region'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace after-indent-region-hook
        nil
        ,(s-lex-format
          "This hooks run after `${major-mode-name}-indent-region'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace before-indent-lines-hook
        nil
        ,(s-lex-format
          "This hooks run before `${major-mode-name}-indent-lines'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace after-indent-lines-hook
        nil
        ,(s-lex-format
          "This hooks run after `${major-mode-name}-indent-lines'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace before-indent-some-lines-hook
        nil
        ,(s-lex-format
          "This hooks run before `${major-mode-name}-indent-some-lines'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace after-indent-some-lines-hook
        nil
        ,(s-lex-format
          "This hooks run after `${major-mode-name}-indent-some-lines'.")
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace indent-some-lines-before-each-line-indent-hook
        nil
        "This hook run before each indent line, when indent some lines."
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-namespace-defcustom
        ,namespace indent-some-lines-after-each-line-indent-hook
        nil
        "This hook run after each indent line, when indent some lines."
        :group ',major-mode
        :type '(repeat function))

       (simple-indentation-hooks-union-from-namespace-to
        ,namespace
        before-indent-some-lines-hook
        ;; ---
        before-indent-lines-hook
        before-indent-region-hook)

       (simple-indentation-hooks-union-from-namespace-to
        ,namespace
        after-indent-some-lines-hook
        ;; ---
        after-indent-lines-hook
        after-indent-region-hook)

       (simple-indentation-hooks-union-from-namespace-to
        ,namespace
        before-run-indent-func-hook
        ;; ---
        before-indent-region-hook
        before-indent-lines-hook
        before-indent-line-hook)

       (simple-indentation-hooks-union-from-namespace-to
        ,namespace
        after-run-indent-func-hook
        ;; ---
        after-indent-region-hook
        after-indent-lines-hook
        after-indent-line-hook)

       (simple-indentation-hooks-add-to-hook-from-namespace
        ,namespace
        before-run-indent-func-hook
        (lambda ()
          (setq simple-indentation-increment-indent-level-function
                (simple-indentation-namespace-from
                 ,namespace increment-real-indent-level))
          (setq simple-indentation-decrement-indent-level-function
                (simple-indentation-namespace-from
                 ,namespace decrement-real-indent-level))))

       (simple-indentation-hooks-add-to-hook-from-namespace
        ,namespace
        after-run-indent-func-hook
        'simple-indentation-to-defaults-change-indent-function)

       (simple-indentation-namespace-defun
        ,namespace
        add-indent-some-lines-before-each-line-indent-hook
        ()
        ""
        (simple-indentation-hooks-from-namespace-add-hook
         ,namespace
         before-each-line-indent-hook
         run-indent-some-lines-before-each-line-indent-hook)

        (simple-indentation-hooks-from-namespace-add-hook
         ,namespace
         after-each-line-indent-hook
         run-indent-some-lines-after-each-line-indent-hook))

       (simple-indentation-namespace-defun
        ,namespace
        remove-indent-some-lines-before-each-line-indent-hook
        ()
        ""
        (simple-indentation-hooks-from-namespace-remove-hook
         ,namespace
         before-each-line-indent-hook
         run-indent-some-lines-before-each-line-indent-hook)

        (simple-indentation-hooks-from-namespace-remove-hook
         ,namespace
         after-each-line-indent-hook
         run-indent-some-lines-after-each-line-indent-hook))

       (simple-indentation-namespace-defun
        ,namespace
        run-indent-some-lines-before-each-line-indent-hook
        ()
        "Run `indent-some-lines-before-each-line-indent-hook'."
        (simple-indentation-hooks-run-from-namespace
         ,namespace indent-some-lines-before-each-line-indent-hook))

       (simple-indentation-namespace-defun
        ,namespace run-indent-some-lines-after-each-line-indent-hook
        ()
        "Run `indent-some-lines-before-each-line-indent-hook'."
        (simple-indentation-hooks-run-from-namespace
         ,namespace
         indent-some-lines-after-each-line-indent-hook))

       (simple-indentation-hooks-from-namespace-add-hook
        ,namespace
        before-indent-some-lines-hook
        add-indent-some-lines-before-each-line-indent-hook)

       (simple-indentation-hooks-from-namespace-add-hook
        ,namespace
        after-indent-some-lines-hook
        remove-indent-some-lines-before-each-line-indent-hook)

       ,(if copy-indention-of-previous-line
            `(simple-indentation-hooks-add-to-hook-from-namespace
              ,namespace
              each-line-before-indent-hook
              'simple-indentation-utils-duplicate-indention-of-prev-line))

       ,(if clear-old-indention
            `(simple-indentation-hooks-add-to-hook-from-namespace
              ,namespace
              each-line-before-indent-hook
              'simple-indentation-utils-clear-indention))

       ,(if clear-empty-lines
            `(simple-indentation-hooks-add-to-hook-from-namespace
              ,namespace
              indent-some-lines-before-each-line-indent-hook
              'simple-indentation-utils-if-empty-clear))

       (simple-indentation-namespace-defcustom
        ,namespace one-indent
        ,one-indent
        ,(s-lex-format
          "One level of indention for ${major-mode-name}.")
        :group ',major-mode
        :type 'string)

       (simple-indentation-namespace-defun
        ,namespace change-real-indent-level
        (n)
        "Change real indent level to N."
        (let ((indent-level
               (simple-indentation-namespace-funcall
                ,namespace get-real-indent-level)))
          (simple-indentation-namespace-funcall
           ,namespace relative-change-real-indent-level
           (- n indent-level))))

       (simple-indentation-namespace-defun
        ,namespace get-real-indent-level
        ()
        "Get real indent level of current line."
        (let* ((line (simple-indentation-utils-current-line))
               (cursor 0)
               (oi
                (simple-indentation-namespace-var ,namespace
                                                  one-indent))
               (step (length oi)))
          (while (s-prefix-p oi (substring line cursor))
            (incf cursor step))
          (/ cursor step)))

       (simple-indentation-namespace-defun
        ,namespace relative-change-real-indent-level
        (n)
        "Increment or decrement real indent level on N units."
        (if (> n 0)
            (--dotimes n
              (simple-indentation-namespace-funcall
               ,namespace increment-real-indent-level))
          (--dotimes
              (- n)
            (simple-indentation-namespace-funcall
             ,namespace decrement-real-indent-level))))

       (simple-indentation-namespace-defun
        ,namespace increment-real-indent-level
        ()
        "Raise indention for current line."
        (interactive)
        (beginning-of-line)
        (insert ,one-indent-of-mode))

       (simple-indentation-namespace-defun
        ,namespace decrement-real-indent-level
        ()
        "Deindent current line."
        (interactive)
        (when (s-prefix-p ,one-indent-of-mode
                          (simple-indentation-utils-current-line))
          (beginning-of-line)
          (delete-forward-char (length ,one-indent-of-mode))))

       (simple-indentation-namespace-defun
        ,namespace indent-line
        (&optional line-num)
        ,(s-lex-format
          "Indent line with LINE-NUM, for `${major-mode-name}'.")
        (interactive)
        (when line-num (goto-line line-num))
        (simple-indentation-hooks-run-from-namespace
         ,namespace before-indent-line-hook)
        (simple-indentation-namespace-funcall
         ,namespace indent-line-without-run-cmd-hooks line-num)
        (simple-indentation-hooks-run-from-namespace
         ,namespace after-indent-line-hook)
        (when (simple-indentation-utils-empty-current-line-p)
          (end-of-line)))

       (simple-indentation-namespace-defun
        ,namespace indent-line-without-run-cmd-hooks
        (&optional line-num)
        "Indent line with `LINE-NUM`, but don't run command hooks."
        (interactive)
        (when line-num (goto-line line-num))
        (simple-indentation-rules-indent-line-with-sorted-rules
         (eval
          (simple-indentation-namespace-from ,namespace
                                             indention-rules))
         :each-line-before-indent-hook ;nofmt
         (simple-indentation-namespace-from ,namespace
                                            each-line-before-indent-hook)
         :each-line-after-indent-hook ;nofmt
         (simple-indentation-namespace-from ,namespace
                                            each-line-after-indent-hook)))

       (simple-indentation-namespace-defun
        ,namespace indent-lines
        (beg end)
        ,(s-lex-format
          "Indent lines for ${major-mode-name} from `BEG` to `END`.
`BEG` and `END` are numbers of lines.")
        (interactive
         (list
          (line-number-at-pos (region-beginning))
          (line-number-at-pos (region-end))))
        (simple-indentation-hooks-run-from-namespace
         ,namespace before-indent-lines-hook)
        (simple-indentation-namespace-funcall
         ,namespace indent-lines-without-run-cmd-hooks beg end)
        (simple-indentation-hooks-run-from-namespace
         ,namespace after-indent-lines-hook))

       (simple-indentation-namespace-defun
        ,namespace indent-lines-without-run-cmd-hooks
        (beg end)
        "Indent lines without run cmd hooks from `BEG` to `END`.
`BEG` and `END` are numbers of lines."
        (interactive
         (list
          (line-number-at-pos (region-beginning))
          (line-number-at-pos (region-end))))
        (unless (= (- end beg) 0)
          (simple-indentation-namespace-funcall
           ,namespace indent-line-without-run-cmd-hooks beg)
          (simple-indentation-namespace-funcall
           ,namespace indent-lines-without-run-cmd-hooks
           (1+ beg)
           end)))

       (simple-indentation-namespace-defun
        ,namespace indent-region
        (beg end)
        ,(s-lex-format
          "Indent region from `BEG` to `END` for ${major-mode-name}")
        (interactive "r")
        (simple-indentation-hooks-run-from-namespace
         ,namespace before-indent-region-hook)
        (simple-indentation-namespace-funcall
         ,namespace indent-region-without-run-cmd-hooks beg end)
        (simple-indentation-hooks-run-from-namespace
         ,namespace after-indent-region-hook))

       (simple-indentation-namespace-defun
        ,namespace indent-region-without-run-cmd-hooks
        (beg end)
        "Indent region from `BEG` to `END` without run cmd hooks."
        (interactive "r")
        (simple-indentation-namespace-funcall ,namespace indent-lines
                                              (line-number-at-pos beg)
                                              (line-number-at-pos end)))

       (simple-indentation-namespace-defun
        ,namespace indent-two-lines
        ()
        ,(s-lex-format
          "Indent previous and current lines in `${major-mode-name}'.")
        (save-excursion
          (when (simple-indentation-utils-previous-line)
            (simple-indentation-namespace-funcall ,namespace
                                                  indent-region
                                                  (point-at-bol)
                                                  (point-at-eol))))
        (simple-indentation-namespace-funcall ,namespace indent-line)
        (forward-line)
        (simple-indentation-namespace-funcall ,namespace indent-line)))))

(defun simple-indentation-to-defaults-change-indent-function ()
  "Set to nil `simple-indentation' indent level functions."
  (setq simple-indentation-increment-indent-level-function nil)
  (setq simple-indentation-decrement-indent-level-function nil))

(defun simple-indentation-increment-indent-level (&optional n)
  "Increment indent level on `N`, defaults to 1."
  (interactive)
  (setq n (or n 1))
  (--dotimes n
    (funcall simple-indentation-increment-indent-level-function)))

(defun simple-indentation-decrement-indent-level (&optional n)
  "Decrement indent level on `N` (deindent current line).
`N` defaults to 1."
  (interactive)
  (setq n (or n 1))
  (--dotimes n
    (funcall simple-indentation-decrement-indent-level-function)))

(provide 'simple-indentation)

;;; simple-indentation.el ends here
