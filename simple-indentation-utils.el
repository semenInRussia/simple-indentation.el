;;; simple-indentation-utils.el --- Simple functions -*- lexical-binding: t; -*-

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

;; This is part of `simple-indentation` which contains basic function of Emacs.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(defgroup simple-indentation-utils nil
  "Simple functions for Emacs."
  :group 'elisp)

(defcustom simple-indentation-utils-text-symbol-regexp "[^\t \n]"
  "Regexp for indicate text symbols."
  :group 'simple-indentation-utils
  :type 'string)

(defcustom simple-indentation-utils-whitespace-symbol-regexp "[\t \n]"
  "Regexp for indicate non text symbols."
  :group 'simple-indentation-utils
  :type 'string)

(defun simple-indentation-utils-compose-with-prev-line (f)
  "Return func, which goto previous line and run function F.
If impossible go to previous line, then return nil."
  (lambda ()
    (when (simple-indentation-utils-previous-line) (funcall f))))

(defun simple-indentation-utils-previous-line (&optional n)
  "Move on previous line N times, return amount of moved lines."
  (interactive)
  (setq n (or n 1))
  (eq 0 (forward-line (- n))))

(defun simple-indentation-utils-previous-code-line ()
  "Go to the previous no comment line.

Return non-nil, when successively."
  (interactive)
  (cl-block nil
    (while t
      (unless (simple-indentation-utils-previous-text-line)
        (cl-return nil))
      (unless (simple-indentation-utils-comment-line-p)
        (cl-return t)))))

(defun simple-indentation-utils-regexp-in-string-start (regexp)
  "Make REGEXP, whcih match on REGEXP only in start of string."
  (s-concat "^" regexp))

(defun simple-indentation-utils-regexp-repeat (regexp)
  "Make REGEXP, whcih match on REGEXP repeated 0+ times."
  (s-concat regexp "*"))

(defun simple-indentation-utils-get-indent-of-string (str)
  "Get indention of STR."
  (or
   (->> str
        (s-match
         (simple-indentation-utils-regexp-in-string-start
          (simple-indentation-utils-regexp-repeat
           simple-indentation-utils-whitespace-symbol-regexp)))
        (-first-item))
   ""))

(defun simple-indentation-utils-get-indent-of-line ()
  "Get indention of current line."
  (simple-indentation-utils-get-indent-of-string
   (simple-indentation-utils-current-line)))

(defun simple-indentation-utils-duplicate-indention-of-prev-line ()
  "Duplicate for current line indention of previous line."
  (interactive)
  (let ((prev-indent
         (save-excursion
           (simple-indentation-utils-previous-text-line)
           (simple-indentation-utils-get-indent-of-line))))
    (simple-indentation-utils-clear-indention)
    (save-excursion (beginning-of-line) (insert prev-indent))))

(defun simple-indentation-utils-previous-text-line ()
  "Navigate to end of backward line not empty (has 1+ not whitespace symbol).
If empty line not found, then return nil, if ok, then return t."
  (interactive)
  (when (simple-indentation-utils-previous-line)
    (end-of-line)
    (search-backward-regexp "[^ \n\t]" nil t)))

(defun simple-indentation-utils-compose-with-prev-text-line (f)
  "Return func, which goto previous text line and run function F.
If impossible go to previous line, then return nil."
  (lambda ()
    (when (simple-indentation-utils-previous-text-line) (funcall f))))

(defun simple-indentation-utils-compose-with-prev-code-line (f)
  "Return func, which goto previous code line and run function F.
If impossible go to previous line, then return nil."
  (lambda ()
    (when (simple-indentation-utils-previous-code-line) (funcall f))))

(defun simple-indentation-utils-empty-current-line-p ()
  "Is current line empty (\"   \", \" \", \"\")?."
  (s-blank-p (s-trim (simple-indentation-utils-current-line))))

(defun simple-indentation-utils-clear-indention ()
  "Clear region indention of current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (->>
     (simple-indentation-utils-get-indent-of-line)
     (length)
     (delete-char))))

(defun simple-indentation-utils-point-at-forward-regexp (regexp &optional bound)
  "Return `point' at forward REGEXP in buffer.
If regexp not found, then get nil, otherwise get `point'.  Max `point' is
BOUND."
  (save-excursion (search-forward-regexp regexp bound t)))

(defun simple-indentation-utils-point-at-forward-match (string &optional bound)
  "Return `point' at forward match with STRING in buffer.
If string not found, then get nil, otherwise get `point'.  Max `point' is
BOUND."
  (save-excursion (search-forward string bound t)))

(defun simple-indentation-utils-if-empty-clear ()
  "If current line is empty, then clear line and navigate to next line."
  (interactive)
  (when (simple-indentation-utils-empty-current-line-p)
    (delete-region (pos-bol) (pos-eol))
    (forward-line)))

(defun simple-indentation-utils-current-line ()
  "Get content of current string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun simple-indentation-utils-code-p (&optional point)
  "Return t, when context at POINT isn't in comment or string."
  (setq point (or point (point)))
  (not (syntax-ppss-context (syntax-ppss (point)))))

(defun simple-indentation-utils-comment-line-p ()
  "Return t, when current line isn't a comment line."
  (comment-only-p (pos-bol) (pos-eol)))

(defun simple-indentation-utils-code-line-has-chars-p (chars)
  "Return t, if current line, ignoring strings and comments has one of CHARS."
  (--any
   (simple-indentation-utils-code-line-has-one-char-p
    (char-to-string it))
   (string-to-list chars)))

(defun simple-indentation-utils-code-line-has-one-char-p (char)
  "Return t, if current line, ignoring strings and comments has CHAR."
  (let ((end (pos-eol))
        found)
    (save-excursion
      (beginning-of-line)
      (while (and (search-forward char end t) (not found))
        (setq found (simple-indentation-utils-code-p)))
      found)))

(defun simple-indentation-utils-line-has-keywords-p (keywords)
  "If current line consist one of KEYWORDS, return non-nil.

\"Current line consist keyword\" mean that line at the cursor consist keyword
with delimeters around"
  (--any
   (simple-indentation-utils-line-has-this-keyword-p it)
   keywords))

(defun simple-indentation-utils-line-has-this-keyword-p (keyword)
  "If current line consists KEYWORD, then return point at it, otherwise nil.

\"Line consists KEYWORDS\" mean that KEYWORD with delimeters symbols around
placed at the current line."
  (save-excursion
    (beginning-of-line)
    (simple-indentation-utils-forward-word keyword (pos-eol))))

(defun simple-indentation-utils-forward-word (word &optional bound)
  "Go to the next matched WORD.

Matched word mean the literal string WORD with space symbols around (space
symbols is special for each major-mode).  Bound the word search with point
BOUND.

If the WORD is not found, then return nil, otherwise return non-nil."
  (->
   (rx word-start (literal word) word-end)
   (search-forward-regexp bound t nil)))

(defun simple-indentation-utils-code-line-has-keywords (keywords)
  "Get t, if current line ignoring strings and comments has one of KEYWORDS."
  (--any
   (simple-indentation-utils-code-line-has-one-keyword-p it)
   keywords))

(defun simple-indentation-utils-code-line-has-one-keyword-p (keyword)
  "Return non-nil, current line ignoring comments and string consists KEYWORD."
  (save-excursion
    (let ((end (pos-eol))
          found)
      (beginning-of-line)
      (while (and
              (simple-indentation-utils-forward-word keyword end)
              (not found))
        (setq found (simple-indentation-utils-code-p)))
      found)))

(defun simple-indentation-utils-compose (&rest funs)
  "Return function composed of FUNS."
  (let* ((something-function (-last-item funs))
         (one-argument-functions (-take (1- (length funs)) funs))
         (one-argument-general-function
          (apply #'-compose one-argument-functions)))
    (lambda (&rest args)
      (funcall one-argument-general-function
               (apply something-function args)))))

(defun simple-indentation-utils-take-including-while (whil list)
  "Take while WHIL from LIST, including last value which on WHIL get t."
  (let ((counted 0))
    (-each-while list whil (lambda (_) (cl-incf counted)))
    (unless (eq (length list) counted) (cl-incf counted))
    (-take counted list)))

(provide 'simple-indentation-utils)
;;; simple-indentation-utils.el ends here
