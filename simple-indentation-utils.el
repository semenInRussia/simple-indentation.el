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


(defun simple-indentation-utils-previous-code-line (&optional n)
    "Move on previous no comment line N times.
Return t, when successively."
    (interactive)
    (setq n (or n 1))
    (let ((ok t))
        (setq ok (simple-indentation-utils-previous-text-line))
        (while (and ok
                    (not (simple-indentation-utils-code-p (point-at-eol))))
            (setq ok (simple-indentation-utils-previous-text-line)))
        ok))


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
        (when (simple-indentation-utils-previous-text-line)
            (funcall f))))


(defun simple-indentation-utils-compose-with-prev-code-line (f)
    "Return func, which goto previous code line and run function F.
If impossible go to previous line, then return nil."
    (lambda ()
        (when (simple-indentation-utils-previous-code-line)
            (funcall f))))


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
If regexp not found, then get nil, otherwise get `point'.  Max `point' is
BOUND."
    (save-excursion (search-forward string bound t)))


(defun simple-indentation-utils-if-empty-clear ()
    "If current line is empty, then clear line and navigate to next line."
    (interactive)
    (when (simple-indentation-utils-empty-current-line-p)
        (delete-region (point-at-bol) (point-at-eol))
        (forward-line)))


(defun simple-indentation-utils-current-line ()
    "Get content of current string."
    (buffer-substring (line-beginning-position) (line-end-position)))


(defun simple-indentation-utils-code-p (&optional point)
    "Return t, when POINT isn't in comment or string."
    (setq point (or point (point)))
    (eq (syntax-ppss-context (syntax-ppss point)) nil))


(defun simple-indentation-utils-code-line-has-chars-p (chars)
    "Return t, if current line, ignoring strings and comments has one of CHARS."
    (--any
     (simple-indentation-utils-code-line-has-one-char
      (char-to-string it))
     (string-to-list chars)))


(defun simple-indentation-utils-code-line-has-one-char (char)
    "Return t, if current line, ignoring strings and comments has CHAR."
    (let ((end (point-at-eol))
          (found nil))
        (save-excursion
            (beginning-of-line)
            (while (-when-let
                       (next-char
                        (simple-indentation-utils-point-at-forward-match
                         char end))
                       (setq found
                             (simple-indentation-utils-code-p next-char))
                       (not found)))
            found)))


(defun simple-indentation-utils-line-has-keywords-p (keywords &optional start)
    "If current string, has one of KEYWORDS, return t.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces.  If start non-nil then before check keywords
go to START point."
    (--any
     (simple-indentation-utils-line-has-this-keyword-p it start)
     keywords))


(defun simple-indentation-utils-line-has-this-keyword-p (keyword &optional start)
    "If current line has KEYWORD, then return point to this, otherwise get nil.
If keyword has space(s), then this is parse as some words separated (1+)
spaces.  If start non-nil then before check keywords go to START point."
    (setq start (or start 0))
    (let ((keyword-regexp
           (->> keyword
                (s-split-words)
                (simple-indentation-utils-regexp-words-separated-spaces)
                (simple-indentation-utils-spaces-around-regexp))))
        (save-excursion
            (beginning-of-line)
            (forward-char start)
            (simple-indentation-utils-point-at-forward-regexp keyword-regexp))))


(defun simple-indentation-utils-regexp-words-separated-spaces (words)
    "Get regexp, which match all WORDS separated spaces."
    (s-join " +" words))


(defun simple-indentation-utils-spaces-around-regexp (regexp)
    "Add spaces around REGEXP."
    (s-concat "^" regexp "$"
              "\\|"
              "^" regexp " "
              "\\|"
              " " regexp "$"
              "\\|"
              " " regexp " "))


(defun simple-indentation-utils-code-line-has-keywords (keywords)
    "Get t, if current line, ignoring strings and comments has one of KEYWORDS."
    (--any
     (simple-indentation-utils-code-line-has-one-keyword it)
     keywords))


(defun simple-indentation-utils-code-line-has-one-keyword (keyword)
    "Get t, when in current line ignoring comments and string has KEYWORD.
Spaces in keywords will converted to (1+) spaces.  For example:

    \"else if\" will match with:
           \"else if\"
           \"else      if\"
           \"else   if\"

KEYWORD is regexp, you must be care with special regexp symbols (.*$^[])."
    (save-excursion
        (let ((found nil)
              (offset 0)
              (begin (point-at-bol)))
            (while (-when-let
                       (next-keyword
                        (simple-indentation-utils-line-has-this-keyword-p
                         keyword offset))
                       (setq found
                             (simple-indentation-utils-code-p next-keyword))
                       (setq offset (- next-keyword begin))
                       (not found)))
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
