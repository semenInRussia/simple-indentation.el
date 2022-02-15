;;; simple-indentation-rules.el --- Rules for `simple-indentation' -*- lexical-binding: t; -*-

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

;; Rules for `simple-indentation'

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)


(require 'simple-indentation-utils)


(defcustom simple-indentation-rules-make-basic-keywords
  '(:predicate
    :indent-func :on-chars
    :check-on-prev-line :on-keywords
    :add-indent :deindent
    :check-on-prev-text-line :on-current-or-previous-line
    :on-current-or-previous-text-line :on-current-or-previous-code-line
    :on-chars-in-code :on-keywords-in-code
    :check-on-prev-code-line)
  "Keywords of rule-make which has handler simple-indentation-handler-:<word>."
  :group 'simple-indentation
  :type '(repeat symbol))


(defcustom simple-indentation-rules-make-binary-keywords
  '(:or :and)
  "Keywords of rule-make which has handler `simple-indentation-handler-:<word>'."
  :group 'simple-indentation
  :type '(repeat symbol))


(defcustom simple-indentation-rules-make-general-keywords-handlers
  (list
   (list 'simple-indentation-rules-basic-keyword-p
         'simple-indentation-rules-basic-handler)
   (list 'simple-indentation-rules-binary-keyword-p
         'simple-indentation-rules-binary-handler)
   (list :begin 'simple-indentation-rules-begin-block-handler)
   (list
    (-const t)
    'simple-indentation-rules-not-found-keyword-handler))
  "This is list of pairs from predicate or keyword and handlers.
Predicate take keyword, rule, values and return t or nil, when nil then,
handler will executing, handler take keyword and return modified,
rule.  If instead of predicate passed keyword, then predicate is function which
return true when keywords is equal.  This functions will call with rule
 and values, when call `simple-indentation-rules-make.'.  Default handler is
`simple-indentation-rules-handler-<keyword>'."
  :group 'simple-indentation
  :type '(repeat '(predicate function)))


(defun simple-indentation-rules-make (&rest args)
    "Create indention rule.
INDENT-FUNC is function which call when PREDICATE returns
non-nil value.  Possible ARGS:
* :check-on-prev-line
Before run PREDICATE, move to previous line.
* :check-on-prev-text-line
Before run PREDICATE, move to previous not empty line.
* :check-on-prev-code-line
Before run PREDICATE, move to previous no comment line.
* :on-keywords <keywords>
Call INDENT-FUNC When line has one of keywords.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces.
* :on-keywords-in-code <keywords>
Call INDENT-FUNC when line, ignoring comments and strings has one of keywords.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces.
* :on-chars <chars>
Call INDENT-FUNC When line has one of chars.
* :on-chars-in-code <chars>
Call INDENT-FUNC when line, ignoring comments and strings, has one of CHARS.
* :and
Take predicate left of :and, set predicate to current predicate and left
* :or
Take predicate left of :or, set predicate to current predicate or left
* :add-indent
INDENT-FUNC utils add `one-indent' to current line.
* :deindent
INDENT-FUNC is deindent current line"
    (let* ((rule (simple-indentation-rules-empty))
           (right-args args)
           keyword
           values
           handled-args
           rule-and-handled-args)
        (while right-args
            (setq keyword (car right-args))
            (setq right-args (cdr right-args))
            (setq values
                  (--take-while (not (keywordp it)) right-args))
            (setq rule-and-handled-args
                  (simple-indentation-rules-keyword keyword
                                                    rule
                                                    values
                                                    right-args))
            (setq rule (car rule-and-handled-args))
            (setq handled-args (-second-item rule-and-handled-args))
            (setq right-args (-drop handled-args right-args)))
        rule))


(defun simple-indentation-rules-keyword (keyword rule values right-args)
    "Handle KEYWORD, then modify RULE with VALUES with RIGHT-ARGS.
Return number of handled modified RULE and RIGHT-ARGS."
    (let ((handle-funtion
           (simple-indentation-rules-get-handler-of keyword)))
        (funcall handle-funtion keyword rule values right-args)))


(defun simple-indentation-rules-basic-keyword-p (keyword)
    "Return t, when KEYWORD has basic handler."
    (-contains-p simple-indentation-rules-make-basic-keywords keyword))


(defun simple-indentation-rules-basic-handler (keyword rule values _right-args)
    "Handle KEYWORD by basic handler with VALUES return modified RULE."
    (let ((basic-handler
           (simple-indentation-rules-get-basic-handler-of keyword))
          (handled-args (length values)))
        (list (funcall basic-handler rule values) handled-args)))


(defun simple-indentation-rules-get-basic-handler-of (keyword)
    "Get basic handler of KEYWORD for `simple-indentation-rules-make'."
    (intern
     (s-concat "simple-indentation-rules-handler-"
               (s-chop-prefix ":" (format "%s" keyword)))))


(defun simple-indentation-rules-binary-keyword-p (keyword)
    "Return t, when KEYWORD has binary handler."
    (-contains-p simple-indentation-rules-make-binary-keywords keyword))


(defun simple-indentation-rules-binary-handler (keyword rule _values right-args)
    "Handle KEYWORD by binary handler with RIGHT-ARGS, return modified RULE."
    (let ((handler
           (simple-indentation-rules-get-binary-handler-of keyword)))
        (list (funcall handler rule right-args) (length right-args))))


(defun simple-indentation-rules-get-binary-handler-of (keyword)
    "Get basic handler of KEYWORD for `simple-indentation-rules-make'."
    (intern
     (s-concat "simple-indentation-rules-bin-handler-"
               (s-chop-prefix ":" (format "%s" keyword)))))


(defun simple-indentation-rules-begin-block-handler (_keyword rule _values right-args)
    "Handler of :begin keyword in `simple-indentation-rules-make'.
Return RULE with number of handled RIGHT-ARGS."
    (let* ((body nil)
           pred)
        (-if-let
            (end-index (-elem-index :end right-args))
            (progn
                (setq body (-take end-index right-args))
                (setq pred
                      (simple-indentation-rules-predicate
                       (apply #'simple-indentation-rules-make body)))
                (list
                 (simple-indentation-rules-set-predicate pred rule)
                 (1+ (length body))))
            (progn
                (simple-indentation-rules-report "Not found closing :end token!"
                                                 :begin right-args)
                (list rule (length body))))))


(defun simple-indentation-rules-report (message keyword right-args)
    "Debug MESSAGE with active KEYWORD, debug RIGHT-ARGS for clarity."
    (let (result)
        (setq result
              (s-lex-format
               "
(simple-indentation-rules-make
  ...
  ${keyword}    ; ${message}"))
        (--each right-args
            (setq result (s-concat result "\n" "  " (pp it))))
        (setq result (s-concat result ")"))
        (message "%s" result)))


(defun simple-indentation-rules-not-found-keyword-handler (keyword rule _values right-args)
    "Message that, handler for KEYWORD not found, and return RULE.
Debug VALUES and RIGHT-ARGS, passed to non exists handler."
    (message
     (s-lex-format
      "Handler of keyword `${keyword}` non exists, this is very bad!
If you are has basic handler, just rename its to this template
`simple-indentation-rules-handler-${keyword}'

If your handler need to special rules, see to
`simple-indentation-rules-make-general-keywords-handlers'"))
    (simple-indentation-rules-report "Here not found KEYWORD!"
                                     keyword
                                     right-args)
    (list rule (length right-args)))


(defun simple-indentation-rules-get-handler-of (keyword)
    "Get handler of KEYWORD for `simple-indentation-rules-make'."
    (let ((handler
           (-second-item
            (--find
             (simple-indentation-rules-special-handler-of-keyword-p keyword it)
             simple-indentation-rules-make-general-keywords-handlers))))
        (lambda (keyword rule values right-args)
            (funcall handler keyword rule values right-args))))


(defun simple-indentation-rules-special-handler-of-keyword-p (keyword pred-and-handler)
    "Get t, when PRED-AND-HANDLER is special handler of KEYWORD.
PRED-AND-HANDLER is list from predicate, which take keyword and return
t when is suitable for keyword handler, and function which take rule and
values and return new rule.  Instead of predicate, also able be keyword,
when this keyword is equal other keyword, return t, by example with basic
predicate."
    (let ((pred-or-keyword (-first-item pred-and-handler)))
        (when (keywordp pred-or-keyword)
            (setq pred-or-keyword
                  (apply-partially #'eq pred-or-keyword)))
        (funcall pred-or-keyword keyword)))


(defun simple-indentation-rules-empty ()
    "Return indention rule by defaults."
    (list (-const nil) nil))


(defun simple-indentation-rules-handler-predicate (rule values)
    "Handler for `:on-chars' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((pred (-first-item values)))
        (simple-indentation-rules-add-predicate pred rule)))


(defun simple-indentation-rules-handler-indent-func (rule values)
    "Handler for `:on-chars' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((indent-func (-first-item values)))
        (simple-indentation-rules-set-indent-func indent-func rule)))


(defun simple-indentation-rules-handler-on-chars (rule values)
    "Handler for `:on-chars' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((chars (-first-item values))
           (new-predicate
            (simple-indentation-rules-make-line-has-chars-p chars)))
        (simple-indentation-rules-add-predicate new-predicate rule)))


(defun simple-indentation-rules-handler-on-chars-in-code (rule values)
    "Handler for `:on-chars-in-code' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((chars (-first-item values))
           (new-predicate
            (simple-indentation-rules-make-line-has-chars-in-code-p
             chars)))
        (simple-indentation-rules-add-predicate new-predicate rule)))


(defun simple-indentation-rules-handler-on-keywords (rule keywords)
    "Handler for `:on-keywords' in `simple-indentation-rules-make'.
RULE is old rule, KEYWORDS is list of keywords"
    (let ((new-predicate
           (simple-indentation-rules-make-line-has-keywords-p keywords)))
        (simple-indentation-rules-add-predicate new-predicate rule)))


(defun simple-indentation-rules-handler-on-keywords-in-code (rule keywords)
    "Handler for `:on-keywords-in-code' in `simple-indentation-rules-make'.
RULE is old rule, KEYWORDS is list of keywords"
    (let ((new-predicate
           (simple-indentation-rules-make-line-has-keywords-in-code-p keywords)))
        (simple-indentation-rules-add-predicate new-predicate rule)))


(defun simple-indentation-rules-handler-check-on-prev-line (rule &rest _)
    "Handler for `:check-on-prev-line' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (simple-indentation-rules-apply-to-predicate
     'simple-indentation-utils-compose-with-prev-line rule))


(defun simple-indentation-rules-handler-check-on-prev-text-line (rule &rest _)
    "Handler for `:check-on-prev-text-line' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (simple-indentation-rules-apply-to-predicate
     'simple-indentation-utils-compose-with-prev-text-line rule))


(defun simple-indentation-rules-handler-check-on-prev-code-line (rule &rest _)
    "Handler for `:check-on-prev-code-line' in `simple-indentation-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (simple-indentation-rules-apply-to-predicate
     'simple-indentation-utils-compose-with-prev-code-line rule))


(defun simple-indentation-rules-handler-on-current-or-previous-line (rule &rest _)
    "Handler of `:on-current-or-previous-line' in `simple-indentation-rules-make'.
Return new modified RULE."
    (simple-indentation-rules-set-predicate
     (simple-indentation-rules-utils-or-run-func-before-indent-current-line-p
      rule 'simple-indentation-utils-previous-line)
     rule))


(defun simple-indentation-rules-handler-on-current-or-previous-text-line (rule &rest _)
    "Handler of `:on-current-or-previous-text-line' in `rules-make'.
Return new modified RULE."
    (simple-indentation-rules-set-predicate
     (simple-indentation-rules-utils-or-run-func-before-indent-current-line-p
      rule 'simple-indentation-utils-previous-text-line)
     rule))


(defun simple-indentation-rules-handler-on-current-or-previous-code-line (rule &rest _)
    "Handler of `:on-current-or-previous-code-line' in `rules-make'.
Return new modified RULE."
    (simple-indentation-rules-set-predicate
     (simple-indentation-rules-utils-or-run-func-before-indent-current-line-p
      rule 'simple-indentation-utils-previous-code-line)
     rule))


(defun simple-indentation-rules-utils-or-run-func-before-indent-current-line-p (rule func)
    "Get func getting result of `indent-current-line-p' or run before FUNC.
Result of `indent-current-line-p' is computed on RULE."
    (lambda ()
        (or
         (simple-indentation-rules-indent-current-line-p rule)
         (progn
             (funcall func)
             (simple-indentation-rules-indent-current-line-p rule)))))


(defun simple-indentation-rules-handler-add-indent (rule &rest _)
    "Handler for `:add-indent' in `simple-indentation-rules-make'.
Return new modified RULE."
    (simple-indentation-rules-set-indent-func
     'simple-indentation-increment-indent-level rule))


(defun simple-indentation-rules-handler-deindent (rule &rest _)
    "Handler for `:add-indent' in `simple-indentation-rules-make'.
Return new modified RULE."
    (simple-indentation-rules-set-indent-func
     'simple-indentation-decrement-indent-level rule))


(defun simple-indentation-rules-bin-handler-or (rule right-args)
    "Binary handler for `:or' for make rules.
Return new modified RULE.  Use RIGHT-ARGS for create second rule."
    (let* ((other-rule
            (apply #'simple-indentation-rules-make right-args))
           (pred
            (lambda ()
                (or
                 (simple-indentation-rules-indent-current-line-p rule)
                 (simple-indentation-rules-indent-current-line-p other-rule)))))
        (simple-indentation-rules-set-predicate pred rule)))


(defun simple-indentation-rules-bin-handler-and (rule right-args)
    "Binary handler for `:and' for make rules.
Return new modified RULE.  Use RIGHT-ARGS for create second rule."
    (let* ((other-rule
            (apply #'simple-indentation-rules-make right-args))
           (pred
            (lambda ()
                (and
                 (simple-indentation-rules-indent-current-line-p other-rule)
                 (simple-indentation-rules-indent-current-line-p rule)))))
        (simple-indentation-rules-set-predicate pred rule)))


(defun simple-indentation-rules-make-line-has-keywords-p (keywords)
    "Make func, which if line has one of KEYWORDS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda ()
        (simple-indentation-utils-line-has-keywords-p keywords)))


(defun simple-indentation-rules-make-line-has-keywords-in-code-p (keywords)
    "Make func, which if line has one of KEYWORDS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda ()
        (simple-indentation-utils-code-line-has-keywords keywords)))


(defun simple-indentation-rules-make-line-has-chars-p (chars)
    "Make func, which if line has one of CHARS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda () (simple-indentation-rules-line-has-chars-p chars)))


(defun simple-indentation-rules-make-line-has-chars-in-code-p (chars)
    "Make func, which get t, when code part of line has one of CHARS."
    (declare (pure t) (side-effect-free t))
    (lambda ()
        (simple-indentation-utils-code-line-has-chars-p chars)))


(defun simple-indentation-rules-line-has-chars-p (chars)
    "If S, has one of CHARS, return t."
    (let ((line (simple-indentation-utils-current-line)))
        (--any
         (s-contains-p (char-to-string it) line)
         (string-to-list chars))))


(defun simple-indentation-rules-predicate (rule)
    "Get predicate of indention RULE."
    (-second-item rule))


(defun simple-indentation-rules-indent-current-line-p (rule)
    "Check this RULE must indent current line."
    (-when-let
        (pred (simple-indentation-rules-predicate rule))
        (save-excursion (funcall pred))))


(defun simple-indentation-rules-indent-current-line-with-func-p (func)
    "Check this FUNC must indent current line."
    (save-excursion (funcall func)))


(defun simple-indentation-rules-set-indent-func (new-indent-func rule)
    "Set indent function of RULE to NEW-INDENT-FUNC."
    (-replace-at 0 new-indent-func rule))


(defun simple-indentation-rules-call-indent-function (rule)
    "Call function for indent current line of `RULE`."
    (save-excursion (beginning-of-line) (funcall (-first-item rule))))


(defun simple-indentation-rules-set-predicate (new-predicate rule)
    "Set predicate of RULE to NEW-PREDICATE."
    (-replace-at 1 new-predicate rule))


(defun simple-indentation-rules-add-predicate (new-predicate rule &optional compose-function)
    "Add predicate NEW-PREDICATE to RULE use for this COMPOSE-FUNCTION.
COMPOSE-FUNCTION take two nil/t values, and return new nil/t value.
COMPOSE-FUNCTION defaults to `or'."
    (setq compose-function
          (or compose-function (lambda (a b) (or a b))))
    (let ((final-pred new-predicate))
        (when (simple-indentation-rules-predicate rule)
            (setq final-pred
                  (lambda ()
                      (funcall
                       compose-function
                       (simple-indentation-rules-indent-current-line-with-func-p
                        new-predicate)
                       (simple-indentation-rules-indent-current-line-p rule)))))
        (simple-indentation-rules-set-predicate final-pred rule)))


(defun simple-indentation-rules-apply-to-predicate (f rule)
    "Apply F to predicate of RULE, and return updated RULE."
    (let* ((old-pred (simple-indentation-rules-predicate rule))
           (new-pred (funcall f old-pred)))
        (simple-indentation-rules-set-predicate new-pred rule)))


(cl-defun simple-indentation-rules-indent-line-with-sorted-rules (sorted-rules
                                                                  &key
                                                                    (each-line-before-indent-hook nil)
                                                                    (each-line-after-indent-hook nil))
    "Indent or don't indent current line depending on SORTED-RULES.
Before each indent of line call EACH-LINE-BEFORE-INDENT-HOOK, after
EACH-LINE-AFTER-INDENT-HOOK"
    (run-hooks each-line-before-indent-hook)
    (-when-let
        (rule
         (--find
          (simple-indentation-rules-indent-current-line-p it)
          sorted-rules))
        (simple-indentation-rules-call-indent-function rule))
    (run-hooks each-line-after-indent-hook))



(provide 'simple-indentation-rules)
;;; simple-indentation-rules.el ends here
