;;; simple-indention-rules.el --- Rules for `simple-indention' -*- lexical-binding: t; -*-

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

;; Rules for `simple-indention'

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)


(require 'simple-indention-utils)


(defcustom simple-indention-rules-make-basic-keywords
  '(:predicate
    :indent-func :on-chars
    :check-on-prev-line :on-keywords
    :add-indent :deindent
    :check-on-prev-text-line :on-current-or-previous-line
    :on-current-or-previous-text-line :on-current-or-previous-code-line
    :on-chars-in-code :on-keywords-in-code
    :check-on-prev-code-line)
  "Keywords of rule-make which has handler simple-indention-handler-:<word>."
  :group 'simple-indention
  :type '(repeat symbol))


(defcustom simple-indention-rules-make-binary-keywords
  '(:or :and)
  "Keywords of rule-make which has handler `simple-indention-handler-:<word>'."
  :group 'simple-indention
  :type '(repeat symbol))


(defcustom simple-indention-rules-make-general-keywords-handlers
  (list
   (list 'simple-indention-rules-basic-keyword-p
         'simple-indention-rules-basic-handler)
   (list 'simple-indention-rules-binary-keyword-p
         'simple-indention-rules-binary-handler)
   (list :begin 'simple-indention-rules-begin-block-handler)
   (list
    (-const t)
    'simple-indention-rules-not-found-keyword-handler))
  "This is list of pairs from predicate or keyword and handlers.
Predicate take keyword, rule, values and return t or nil, when nil then,
handler will executing, handler take keyword and return modified,
rule.  If instead of predicate passed keyword, then predicate is function which
return true when keywords is equal.  This functions will call with rule
 and values, when call `simple-indention-rules-make.'.  Default handler is
`simple-indention-rules-handler-<keyword>'."
  :group 'simple-indention
  :type '(repeat '(predicate function)))


(defun simple-indention-rules-make (&rest args)
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
    (let* ((rule (simple-indention-rules-empty))
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
                  (simple-indention-rules-keyword keyword
                                                  rule
                                                  values
                                                  right-args))
            (setq rule (car rule-and-handled-args))
            (setq handled-args (-second-item rule-and-handled-args))
            (setq right-args (-drop handled-args right-args)))
        rule))


(defun simple-indention-rules-keyword (keyword rule values right-args)
    "Handle KEYWORD, then modify RULE with VALUES with RIGHT-ARGS.
Return number of handled modified RULE and RIGHT-ARGS."
    (let ((handle-funtion
           (simple-indention-rules-get-handler-of keyword)))
        (funcall handle-funtion keyword rule values right-args)))


(defun simple-indention-rules-basic-keyword-p (keyword)
    "Return t, when KEYWORD has basic handler."
    (-contains-p simple-indention-rules-make-basic-keywords keyword))


(defun simple-indention-rules-basic-handler (keyword rule values _right-args)
    "Handle KEYWORD by basic handler with VALUES return modified RULE."
    (let ((basic-handler
           (simple-indention-rules-get-basic-handler-of keyword))
          (handled-args (length values)))
        (list (funcall basic-handler rule values) handled-args)))


(defun simple-indention-rules-get-basic-handler-of (keyword)
    "Get basic handler of KEYWORD for `simple-indention-rules-make'."
    (intern
     (s-concat "simple-indention-rules-handler-"
               (s-chop-prefix ":" (format "%s" keyword)))))


(defun simple-indention-rules-binary-keyword-p (keyword)
    "Return t, when KEYWORD has binary handler."
    (-contains-p simple-indention-rules-make-binary-keywords keyword))


(defun simple-indention-rules-binary-handler (keyword rule _values right-args)
    "Handle KEYWORD by binary handler with RIGHT-ARGS, return modified RULE."
    (let ((handler
           (simple-indention-rules-get-binary-handler-of keyword)))
        (list (funcall handler rule right-args) (length right-args))))


(defun simple-indention-rules-get-binary-handler-of (keyword)
    "Get basic handler of KEYWORD for `simple-indention-rules-make'."
    (intern
     (s-concat "simple-indention-rules-bin-handler-"
               (s-chop-prefix ":" (format "%s" keyword)))))


(defun simple-indention-rules-begin-block-handler (_keyword rule _values right-args)
    "Handler of :begin keyword in `simple-indention-rules-make'.
Return RULE with number of handled RIGHT-ARGS."
    (let* ((body nil)
           pred)
        (-if-let
            (end-index (-elem-index :end right-args))
            (progn
                (setq body (-take end-index right-args))
                (setq pred
                      (simple-indention-rules-predicate
                       (apply #'simple-indention-rules-make body)))
                (list
                 (simple-indention-rules-set-predicate pred rule)
                 (1+ (length body))))
            (progn
                (simple-indention-rules-report "Not found closing :end token!"
                                               :begin right-args)
                (list rule (length body))))))


(defun simple-indention-rules-report (message keyword right-args)
    "Debug MESSAGE with active KEYWORD, debug RIGHT-ARGS for clarity."
    (let (result)
        (setq result
              (s-lex-format
               "
(simple-indention-rules-make
  ...
  ${keyword}    ; ${message}"))
        (--each right-args
            (setq result (s-concat result "\n" "  " (pp it))))
        (setq result (s-concat result ")"))
        (message "%s" result)))


(defun simple-indention-rules-not-found-keyword-handler (keyword rule _values right-args)
    "Message that, handler for KEYWORD not found, and return RULE.
Debug VALUES and RIGHT-ARGS, passed to non exists handler."
    (message
     (s-lex-format
      "Handler of keyword `${keyword}` non exists, this is very bad!
If you are has basic handler, just rename its to this template
`simple-indention-rules-handler-${keyword}'

If your handler need to special rules, see to
`simple-indention-rules-make-general-keywords-handlers'"))
    (simple-indention-rules-report "Here not found KEYWORD!"
                                   keyword
                                   right-args)
    (list rule (length right-args)))


(defun simple-indention-rules-get-handler-of (keyword)
    "Get handler of KEYWORD for `simple-indention-rules-make'."
    (let ((handler
           (-second-item
            (--find
             (simple-indention-rules-special-handler-of-keyword-p keyword it)
             simple-indention-rules-make-general-keywords-handlers))))
        (lambda (keyword rule values right-args)
            (funcall handler keyword rule values right-args))))


(defun simple-indention-rules-special-handler-of-keyword-p (keyword pred-and-handler)
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


(defun simple-indention-rules-empty ()
    "Return indention rule by defaults."
    (list (-const nil) nil))


(defun simple-indention-rules-handler-predicate (rule values)
    "Handler for `:on-chars' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((pred (-first-item values)))
        (simple-indention-rules-add-predicate pred rule)))


(defun simple-indention-rules-handler-indent-func (rule values)
    "Handler for `:on-chars' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((indent-func (-first-item values)))
        (simple-indention-rules-set-indent-func indent-func rule)))


(defun simple-indention-rules-handler-on-chars (rule values)
    "Handler for `:on-chars' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((chars (-first-item values))
           (new-predicate
            (simple-indention-rules-make-line-has-chars-p chars)))
        (simple-indention-rules-add-predicate new-predicate rule)))


(defun simple-indention-rules-handler-on-chars-in-code (rule values)
    "Handler for `:on-chars-in-code' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (let* ((chars (-first-item values))
           (new-predicate
            (simple-indention-rules-make-line-has-chars-in-code-p
             chars)))
        (simple-indention-rules-add-predicate new-predicate rule)))


(defun simple-indention-rules-handler-on-keywords (rule keywords)
    "Handler for `:on-keywords' in `simple-indention-rules-make'.
RULE is old rule, KEYWORDS is list of keywords"
    (let ((new-predicate
           (simple-indention-rules-make-line-has-keywords-p keywords)))
        (simple-indention-rules-add-predicate new-predicate rule)))


(defun simple-indention-rules-handler-on-keywords-in-code (rule keywords)
    "Handler for `:on-keywords-in-code' in `simple-indention-rules-make'.
RULE is old rule, KEYWORDS is list of keywords"
    (let ((new-predicate
           (simple-indention-rules-make-line-has-keywords-in-code-p keywords)))
        (simple-indention-rules-add-predicate new-predicate rule)))


(defun simple-indention-rules-handler-check-on-prev-line (rule &rest _)
    "Handler for `:check-on-prev-line' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (simple-indention-rules-apply-to-predicate
     'simple-indention-utils-compose-with-prev-line rule))


(defun simple-indention-rules-handler-check-on-prev-text-line (rule &rest _)
    "Handler for `:check-on-prev-text-line' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (simple-indention-rules-apply-to-predicate
     'simple-indention-utils-compose-with-prev-text-line rule))


(defun simple-indention-rules-handler-check-on-prev-code-line (rule &rest _)
    "Handler for `:check-on-prev-code-line' in `simple-indention-rules-make'.
RULE is old rule, VALUES is list, in which first item is string of chars.
Return new rule."
    (simple-indention-rules-apply-to-predicate
     'simple-indention-utils-compose-with-prev-code-line rule))


(defun simple-indention-rules-handler-on-current-or-previous-line (rule &rest _)
    "Handler of `:on-current-or-previous-line' in `simple-indention-rules-make'.
Return new modified RULE."
    (simple-indention-rules-set-predicate
     (simple-indention-rules-utils-or-run-func-before-indent-current-line-p
      rule 'simple-indention-utils-previous-line)
     rule))


(defun simple-indention-rules-handler-on-current-or-previous-text-line (rule &rest _)
    "Handler of `:on-current-or-previous-text-line' in `rules-make'.
Return new modified RULE."
    (simple-indention-rules-set-predicate
     (simple-indention-rules-utils-or-run-func-before-indent-current-line-p
      rule 'simple-indention-utils-previous-text-line)
     rule))


(defun simple-indention-rules-handler-on-current-or-previous-code-line (rule &rest _)
    "Handler of `:on-current-or-previous-code-line' in `rules-make'.
Return new modified RULE."
    (simple-indention-rules-set-predicate
     (simple-indention-rules-utils-or-run-func-before-indent-current-line-p
      rule 'simple-indention-utils-previous-code-line)
     rule))


(defun simple-indention-rules-utils-or-run-func-before-indent-current-line-p (rule func)
    "Get func getting result of `indent-current-line-p' or run before FUNC.
Result of `indent-current-line-p' is computed on RULE."
    (lambda ()
        (or
         (simple-indention-rules-indent-current-line-p rule)
         (progn
             (funcall func)
             (simple-indention-rules-indent-current-line-p rule)))))


(defun simple-indention-rules-handler-add-indent (rule &rest _)
    "Handler for `:add-indent' in `simple-indention-rules-make'.
Return new modified RULE."
    (simple-indention-rules-set-indent-func
     'simple-indention-increment-indent-level rule))


(defun simple-indention-rules-handler-deindent (rule &rest _)
    "Handler for `:add-indent' in `simple-indention-rules-make'.
Return new modified RULE."
    (simple-indention-rules-set-indent-func
     'simple-indention-decrement-indent-level rule))


(defun simple-indention-rules-bin-handler-or (rule right-args)
    "Binary handler for `:or' for make rules.
Return new modified RULE.  Use RIGHT-ARGS for create second rule."
    (let* ((other-rule
            (apply #'simple-indention-rules-make right-args))
           (pred
            (lambda ()
                (or
                 (simple-indention-rules-indent-current-line-p rule)
                 (simple-indention-rules-indent-current-line-p other-rule)))))
        (simple-indention-rules-set-predicate pred rule)))


(defun simple-indention-rules-bin-handler-and (rule right-args)
    "Binary handler for `:and' for make rules.
Return new modified RULE.  Use RIGHT-ARGS for create second rule."
    (let* ((other-rule
            (apply #'simple-indention-rules-make right-args))
           (pred
            (lambda ()
                (and
                 (simple-indention-rules-indent-current-line-p other-rule)
                 (simple-indention-rules-indent-current-line-p rule)))))
        (simple-indention-rules-set-predicate pred rule)))


(defun simple-indention-rules-make-line-has-keywords-p (keywords)
    "Make func, which if line has one of KEYWORDS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda ()
        (simple-indention-utils-line-has-keywords-p keywords)))


(defun simple-indention-rules-make-line-has-keywords-in-code-p (keywords)
    "Make func, which if line has one of KEYWORDS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda ()
        (simple-indention-utils-code-line-has-keywords keywords)))


(defun simple-indention-rules-make-line-has-chars-p (chars)
    "Make func, which if line has one of CHARS, return t."
    (declare (pure t) (side-effect-free t))
    (lambda () (simple-indention-rules-line-has-chars-p chars)))


(defun simple-indention-rules-make-line-has-chars-in-code-p (chars)
    "Make func, which get t, when code part of line has one of CHARS."
    (declare (pure t) (side-effect-free t))
    (lambda () (simple-indention-utils-code-line-has-chars-p chars)))


(defun simple-indention-rules-line-has-chars-p (chars)
    "If S, has one of CHARS, return t."
    (let ((line (simple-indention-utils-current-line)))
        (--any
         (s-contains-p (char-to-string it) line)
         (string-to-list chars))))


(defun simple-indention-rules-predicate (rule)
    "Get predicate of indention RULE."
    (-second-item rule))


(defun simple-indention-rules-indent-current-line-p (rule)
    "Check this RULE must indent current line."
    (-when-let
        (pred (simple-indention-rules-predicate rule))
        (save-excursion (funcall pred))))


(defun simple-indention-rules-indent-current-line-with-func-p (func)
    "Check this FUNC must indent current line."
    (save-excursion (funcall func)))


(defun simple-indention-rules-set-indent-func (new-indent-func rule)
    "Set indent function of RULE to NEW-INDENT-FUNC."
    (-replace-at 0 new-indent-func rule))


(defun simple-indention-rules-call-indent-function (rule)
    "Call function for indent current line of `RULE`."
    (save-excursion (beginning-of-line) (funcall (-first-item rule))))


(defun simple-indention-rules-set-predicate (new-predicate rule)
    "Set predicate of RULE to NEW-PREDICATE."
    (-replace-at 1 new-predicate rule))


(defun simple-indention-rules-add-predicate (new-predicate rule &optional compose-function)
    "Add predicate NEW-PREDICATE to RULE use for this COMPOSE-FUNCTION.
COMPOSE-FUNCTION take two nil/t values, and return new nil/t value.
COMPOSE-FUNCTION defaults to `or'."
    (setq compose-function
          (or compose-function (lambda (a b) (or a b))))
    (let ((final-pred new-predicate))
        (when (simple-indention-rules-predicate rule)
            (setq final-pred
                  (lambda ()
                      (funcall
                       compose-function
                       (simple-indention-rules-indent-current-line-with-func-p
                        new-predicate)
                       (simple-indention-rules-indent-current-line-p rule)))))
        (simple-indention-rules-set-predicate final-pred rule)))


(defun simple-indention-rules-apply-to-predicate (f rule)
    "Apply F to predicate of RULE, and return updated RULE."
    (let* ((old-pred (simple-indention-rules-predicate rule))
           (new-pred (funcall f old-pred)))
        (simple-indention-rules-set-predicate new-pred rule)))


(cl-defun simple-indention-rules-indent-line-with-sorted-rules (sorted-rules
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
          (simple-indention-rules-indent-current-line-p it)
          sorted-rules))
        (simple-indention-rules-call-indent-function rule))
    (run-hooks each-line-after-indent-hook))



(provide 'simple-indention-rules)
;;; simple-indention-rules.el ends here
