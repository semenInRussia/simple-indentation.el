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
  '(:or :and :not)
  "Keywords for `rule-make'.

For each keyword should be available function
`simple-indentation-bin-handler-<word>'.  These functions should take rule and
arguments after the keyword handling and return a modified rule"
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
  "Pairs from predicate/keyword, handler for `simple-indentation-rules-make'.

Predicate is a function taking the following arguments

- keyword, handling keyword passed to `simple-indentation-rules-make'
- rule, rule which was builded with previous keywords
- values, values after a handling keyword

Predicate should return non-nil, if handler should be executed.  If
instead of predicate given a keyword, then predicate is function which
compare a taking keyword with the given

Handler is a function return a modified rule, it take same args as predicate"
  :group 'simple-indentation
  :type '(repeat '(predicate function)))

(defun simple-indentation-rules-make (&rest args)
  "Create indention rule.

INDENT-FUNC is function which call when PREDICATE returns
non-nil value.  Possible ARGS:
- :check-on-prev-line
Before run PREDICATE, move to previous line.
- :check-on-prev-text-line
Before run PREDICATE, move to previous not empty line.
- :check-on-prev-code-line
Before run PREDICATE, move to previous no comment line.
- :on-keywords <keywords>
Call INDENT-FUNC When line has one of keywords.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces.
- :on-keywords-in-code <keywords>
Call INDENT-FUNC when line, ignoring comments and strings has one of keywords.
If keyword of keywords has space, then this keywords parsed as keyword 1 and
keyword2 splitted (1+) spaces.
- :on-chars <chars>
Call INDENT-FUNC When line has one of chars.
- :on-chars-in-code <chars>
Call INDENT-FUNC when line, ignoring comments and strings, has one of CHARS.
- :and
Take predicate left of :and, set predicate to current predicate and left
- :or
Take predicate left from :or, set predicate to current predicate or left
- :add-indent
Change INDENT-FUNC to function adding `one-indent' to the current line.
This function must be saved within the
`simple-indentation-increment-indent-level-function' variable
- :deindent
Change INDENT-FUNC to function deindenting current line.

This function must be saved within the
`simple-indentation-decrement-indent-level-function' variable"
  (let* ((rule (simple-indentation-rules-empty))
         (right-args args)
         keyword
         values
         handled-args
         rule-and-handled-args)
    (while right-args
      (setq keyword (car right-args))
      (setq right-args (cdr right-args))
      (setq values (--take-while (not (keywordp it)) right-args))
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
  "Handle KEYWORD, after modify RULE with VALUES and RIGHT-ARGS.

Return list from the modified RULE and number of handled arguments from
RIGHT-ARGS"
  (let ((handle-funtion
         (simple-indentation-rules-get-handler-of keyword)))
    (funcall handle-funtion keyword rule values right-args)))

(defun simple-indentation-rules-basic-keyword-p (keyword)
  "Return non-nil, when KEYWORD has a basic handler for the make rule func."
  (-contains-p simple-indentation-rules-make-basic-keywords keyword))

(defun simple-indentation-rules-basic-handler (keyword rule values _right-args)
  "Handle KEYWORD with basic handler pass to it VALUES, return a modified RULE."
  (let ((basic-handler
         (simple-indentation-rules-get-basic-handler-of keyword))
        ;; mark all values beetween handling keyword and next keyword as handled
        (handled-args (length values)))
    (list (funcall basic-handler rule values) handled-args)))

(defun simple-indentation-rules-get-basic-handler-of (keyword)
  "Get a basic handler for KEYWORD for `simple-indentation-rules-make'."
  (intern
   (s-concat "simple-indentation-rules-handler-"
             (s-chop-prefix ":" (format "%s" keyword)))))

(defun simple-indentation-rules-binary-keyword-p (keyword)
  "Return t, if KEYWORD has binary handler for `simple-indentation-rules-make'."
  (-contains-p simple-indentation-rules-make-binary-keywords keyword))

(defun simple-indentation-rules-binary-handler (keyword rule _values right-args)
  "Handle KEYWORD with binary handler for `simple-indentation-rules-make' args.

Pass to this handler RULE and RIGHT-ARGS.  Return modified RULE.  You can add
handler by updating `simple-indentation-rules-make-binary-keywords' variable"
  (let ((handler
         (simple-indentation-rules-get-binary-handler-of keyword))
        ;; mark ALL values after handling keyword as handled
        (handled-args-count (length right-args)))
    (list (funcall handler rule right-args) handled-args-count)))

(defun simple-indentation-rules-get-binary-handler-of (keyword)
  "Get a basic handler of KEYWORD for `simple-indentation-rules-make'."
  (intern
   (s-concat "simple-indentation-rules-bin-handler-"
             (s-chop-prefix ":" (format "%s" keyword)))))

(defun simple-indentation-rules-begin-block-handler (_keyword ;nofmt
                                                     rule
                                                     _values
                                                     right-args)
  "Handler of :begin keyword in `simple-indentation-rules-make'.

Return a list from the RULE and number of handled args from RIGHT-ARGS."
  (-if-let*
      ((end-index (-elem-index :end right-args))
       (body (-take end-index right-args))
       (body-rule (apply #'simple-indentation-rules-make body))
       (pred (simple-indentation-rules-predicate body-rule))
       (handled-args-number (1+ (length body))))
      (list
       (simple-indentation-rules-set-predicate pred rule)
       handled-args-number)
    (progn
      (simple-indentation-rules-report "Not found closing :end token!"
                                       :begin right-args)
      (list rule (length right-args)))))

(defun simple-indentation-rules-report (message keyword right-args)
  "Debug MESSAGE with active KEYWORD, debug RIGHT-ARGS for clarity."
  (let ((report-string
         (s-lex-format
          "
(simple-indentation-rules-make
  ...
  ${keyword}    ; ${message}")))
    (--each right-args
      (setq report-string (s-concat report-string "\n" "  " (pp it))))
    (setq report-string (s-concat report-string ")"))
    (display-warning 'simple-indentation report-string :error)))

(defun simple-indentation-rules-not-found-keyword-handler (keyword ;nofmt
                                                           rule
                                                           _values
                                                           right-args)
  "Message that handler for KEYWORD isn't found and return a modifed RULE.

Debug VALUES and RIGHT-ARGS, passed to non exists handler."
  (display-warning
   'simple-indentation
   (s-lex-format
    "Handler of keyword `${keyword}' isn't exists.
If you has a basic handler, just rename it to the following template
`simple-indentation-rules-handler-${keyword}'

If your handler need to special rules, see
`simple-indentation-rules-make-general-keywords-handlers'")
   :error)
  (simple-indentation-rules-report "Here not found KEYWORD!"
                                   keyword
                                   right-args)
  (list rule (length right-args)))

(defun simple-indentation-rules-get-handler-of (keyword)
  "Get a handler for KEYWORD for `simple-indentation-rules-make' args parsing."
  (let ((handler
         (-second-item
          (--find
           (simple-indentation-rules-special-handler-of-keyword-p keyword it)
           simple-indentation-rules-make-general-keywords-handlers))))
    (lambda (keyword rule values right-args)
      (funcall handler keyword rule values right-args))))

(defun simple-indentation-rules-special-handler-of-keyword-p (keyword ;nofmt
                                                              pred-and-handler)
  "Get t, if PRED-AND-HANDLER is suited to KEYWORD in the rule make function.

PRED-AND-HANDLER is a item from the
`simple-indentation-rules-make-general-keywords-handlers'."
  (let ((pred-or-keyword (-first-item pred-and-handler)))
    (when (keywordp pred-or-keyword)
      (setq pred-or-keyword (apply-partially #'eq pred-or-keyword)))
    (funcall pred-or-keyword keyword)))

(defun simple-indentation-rules-empty ()
  "Return default empty indention rule."
  (list (-const nil) nil))

(defun simple-indentation-rules-handler-predicate (rule values)
  "Handler of `:predicate' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:predicate <pred>

Where <pred> is a function returning non-nil to indicate RULE execution.
VALUES is list from the arguments of `simple-indentation-rules-make'
after `:predicate' keyword"
  (let* ((pred (-first-item values)))
    (simple-indentation-rules-add-predicate pred rule)))

(defun simple-indentation-rules-handler-indent-func (rule values)
  "Handler of `:indent-func' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:indent-func <func>

Where <func> is a function changing a buffer, it will be called when a
predicate of the RULE returns non-nil.  VALUES is list from the
arguments of `simple-indentation-rules-make' after `:predicate'
keyword"
  (let* ((indent-func (-first-item values)))
    (simple-indentation-rules-set-indent-func indent-func rule)))

(defun simple-indentation-rules-handler-on-chars (rule values)
  "Handler of `:on-chars' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:on-chars <chars>

Where <chars> is a string from chars if one of its placed at the current line,
then predicate will return non-nil VALUES is list from the arguments
of `simple-indentation-rules-make' after `:on-chars' keyword.  Return given RULE
with new predicate"
  (let* ((chars (-first-item values))
         (new-predicate
          (simple-indentation-rules-make-line-has-chars-p chars)))
    (simple-indentation-rules-add-predicate new-predicate rule)))

(defun simple-indentation-rules-handler-on-chars-in-code (rule values)
  "Handler of `:on-chars-in-code' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:on-chars-in-code <chars>

Where <chars> is a string from chars, if one of its placed at the
current line (ignore commentaries), then predicate will return non-nil
VALUES is list from the arguments of `simple-indentation-rules-make'
after `:on-chars-in-code' keyword.  Return given RULE with new
predicate"
  (let* ((chars (-first-item values))
         (new-predicate
          (simple-indentation-rules-make-line-has-chars-in-code-p chars)))
    (simple-indentation-rules-add-predicate new-predicate rule)))

(defun simple-indentation-rules-handler-on-keywords (rule keywords)
  "Handler of `:on-keywords' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:on-keywords <words>

Where <words> is a list from words, if one of its placed at the
current line, then predicate will return non-nil VALUES is list from
the arguments of `simple-indentation-rules-make' after `:on-keywords'
keyword.  Return given RULE with new predicate.

KEYWORDS is a list from the arguments of `simple-indentation-rules-make' for
`:on-keywords', each of it is a keyword"
  (let ((new-predicate
         (simple-indentation-rules-make-line-has-keywords-p keywords)))
    (simple-indentation-rules-add-predicate new-predicate rule)))

(defun simple-indentation-rules-handler-on-keywords-in-code (rule keywords)
  "Handler of `:on-keywords-in-code' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:on-keywords-in-code <words>

Where <words> is a list from words, if one of its placed at the
current line (ignore commentaries), then predicate will return non-nil
VALUES is list from the arguments of `simple-indentation-rules-make'
after `:on-keywords-in-code' keyword.  Return given RULE with new
predicate.  If you call this function literally you can pass value of <words>
with argument KEYWORDS."
  (let ((new-predicate
         (simple-indentation-rules-make-line-has-keywords-in-code-p keywords)))
    (simple-indentation-rules-add-predicate new-predicate rule)))

(defun simple-indentation-rules-handler-check-on-prev-line (rule &rest _)
  "Handler of `:check-on-prev-line' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

... some predicates
:check-on-prev-line

Where ... some predicates is some keywords of `simple-indentation-rules-make'
updating RULE predicate.  Using this keyword predicate will be called at the
previous line"
  (simple-indentation-rules-apply-to-predicate
   'simple-indentation-utils-compose-with-prev-line rule))

(defun simple-indentation-rules-handler-check-on-prev-text-line (rule &rest _)
  "Handler of `:check-on-prev-text-line' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

... some predicates
:check-on-prev-text-line

Where ... some predicates is some keywords of `simple-indentation-rules-make'
updating RULE predicate.  Using this keyword predicate will be called at the
previous text line (ignore blank lines)"
  (simple-indentation-rules-apply-to-predicate
   'simple-indentation-utils-compose-with-prev-text-line rule))

(defun simple-indentation-rules-handler-check-on-prev-code-line (rule &rest _)
  "Handler of `:check-on-prev-code-line' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

... some predicates
:check-on-prev-code-line

Where ... some predicates is some keywords of `simple-indentation-rules-make'
updating RULE predicate.  Using this keyword predicate will be called at the
previous code line (ignore commentaries lines)"
  (simple-indentation-rules-apply-to-predicate
   'simple-indentation-utils-compose-with-prev-code-line rule))

(defun simple-indentation-rules-handler-on-current-or-previous-line (rule ;nofmt
                                                                     &rest _)
  "Handler of :on-current-or-previous-line for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

... some predicates
:on-current-or-previous-line

Where ... some predicates is some keywords of `simple-indentation-rules-make'
updating RULE predicate.  Using this keyword predicate will be called at the
previous line and at the current, the predicate will return non-nil if the
one of both calls return non-nil"
  (simple-indentation-rules-set-predicate
   (simple-indentation-rules-predicate-with-possible-action-before
    rule 'simple-indentation-utils-previous-line)
   rule))

(defun simple-indentation-rules-handler-add-indent (rule &rest _)
  "Handler of `:add-indent' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:add-indent

Using this keyword, the indent function of RULE will be setted to value of
`simple-indentation-increment-indent-level-function'.  Return a rule with new
indent function"
  (simple-indentation-rules-set-indent-func
   'simple-indentation-increment-indent-level
   rule))

(defun simple-indentation-rules-handler-deindent (rule &rest _)
  "Handler of `:deindent' for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:deindent

Using this keyword, the indent function of RULE will be setted to value of
`simple-indentation-decrement-indent-level-function'.  Return a rule with new
indent function"
  (simple-indentation-rules-set-indent-func
   'simple-indentation-decrement-indent-level rule))

(defun simple-indentation-rules-handler-on-current-or-previous-code-line (rule
                                                                          ;;nofm
                                                                          &rest
                                                                          _)
  "Handler of `:on-current-or-previous-code-line' in `rules-make'.
Return new modified RULE."
  (simple-indentation-rules-set-predicate
   (simple-indentation-rules-predicate-with-possible-action-before
    rule 'simple-indentation-utils-previous-code-line)
   rule))

(defun simple-indentation-rules-handler-on-current-or-previous-text-line (rule
                                                                          ;;nofm
                                                                          &rest
                                                                          _)
  "Simple-indentation-rules-make handler of :on-current-or-previous-text-line.

In `simple-indentation-rules-make' should be used by following way

... some predicates
:on-current-or-previous-text-line

Where ... some predicates is some keywords of `simple-indentation-rules-make'
updating RULE predicate.  Using this keyword predicate will be called at the
previous code line (ignore blank) and at the current, the predicate
will return non-nil if the one of both calls return non-nil"
  (simple-indentation-rules-set-predicate
   (simple-indentation-rules-predicate-with-possible-action-before
    rule 'simple-indentation-utils-previous-text-line)
   rule))

(defun simple-indentation-rules-bin-handler-or (rule right-args)
  "Binary handler of the `:or' keyword for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

...predicate1
:or
...predicate2

Where ...predicate1 and ...predicate2 are lines with keywords changing
predicate of a RULE (sush as `:predicate')

Using this keyword predicate predicate of a RULE will be setted to function
returning result of predicate1 or result of predicate2.  Return a modified RULE
with new predicate

RIGHT-ARGS is args after the `:or' keyword"
  (let* ((other-rule
          (apply #'simple-indentation-rules-make right-args))
         (pred
          (lambda ()
            (or
             (simple-indentation-rules-indent-current-line-p rule)
             (simple-indentation-rules-indent-current-line-p other-rule)))))
    (simple-indentation-rules-set-predicate pred rule)))

(defun simple-indentation-rules-bin-handler-and (rule right-args)
  "Binary handler of the `:and' keyword for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

...predicate1
:and
...predicate2

Where ...predicate1 and ...predicate2 are lines with keywords changing
predicate of a RULE (sush as `:predicate')

Using this keyword predicate predicate of a RULE will be setted to function
returning result of predicate1 logic and result of predicate2.  Return
a modified RULE with new predicate

RIGHT-ARGS is args after the `:and' keyword"
  (let* ((other-rule
          (apply #'simple-indentation-rules-make right-args))
         (pred
          (lambda ()
            (and
             (simple-indentation-rules-indent-current-line-p other-rule)
             (simple-indentation-rules-indent-current-line-p rule)))))
    (simple-indentation-rules-set-predicate pred rule)))

(defun simple-indentation-rules-bin-handler-not (rule right-args)
  "Binary handler of the `:not' keyword for `simple-indentation-rules-make'.

In `simple-indentation-rules-make' should be used by following way

:not
...predicates

Where ...predicates is lines with keywords changing
predicate of a RULE (sush as `:predicate')

Using this keyword predicate predicate of a RULE will be setted to function
returning result logical not of result of predicate builded with the keywords
after `:not'

Here RIGHT-ARGS is a list from the values which passed to
the `run-command-rules-make'.  This handler make from RIGHT-ARGS new rule and
change its predicate to negative."
  (let* ((other-rule
          (apply #'simple-indentation-rules-make right-args))
         (other-pred
          (-not (simple-indentation-rules-predicate other-rule))))
    (simple-indentation-rules-set-predicate other-pred rule)))

(defun simple-indentation-rules-predicate-with-possible-action-before (rule
                                                                       ;;nofmt
                                                                       func)
  "Get a function, returning RULE predicate's result or a RULE predicate result with FUNC call before."
  ;; sorry all to this long message
  (lambda ()
    (or
     (simple-indentation-rules-indent-current-line-p rule)
     (progn
       (funcall func)
       (simple-indentation-rules-indent-current-line-p rule)))))

(defun simple-indentation-rules-make-line-has-keywords-p (keywords)
  "Make function, return non-nil if line has one of KEYWORDS."
  (declare (pure t) (side-effect-free t))
  (lambda ()
    (simple-indentation-utils-line-has-keywords-p keywords)))

(defun simple-indentation-rules-make-line-has-keywords-in-code-p (keywords)
  "Make function, return non-nil if code line has one of KEYWORDS."
  (declare (pure t) (side-effect-free t))
  (lambda ()
    (simple-indentation-utils-code-line-has-keywords keywords)))

(defun simple-indentation-rules-make-line-has-chars-p (chars)
  "Make function, return non-nil if line has one of CHARS."
  (declare (pure t) (side-effect-free t))
  (lambda () (simple-indentation-rules-line-has-chars-p chars)))

(defun simple-indentation-rules-make-line-has-chars-in-code-p (chars)
  "Make function, return non-nil if code line has one of CHARS."
  (declare (pure t) (side-effect-free t))
  (lambda () (simple-indentation-utils-code-line-has-chars-p chars)))

(defun simple-indentation-rules-line-has-chars-p (chars)
  "If content of the current line has one of CHARS, return non-nil."
  (let ((line (simple-indentation-utils-current-line)))
    (--any
     (s-contains-p (char-to-string it) line)
     (string-to-list chars))))

(defun simple-indentation-rules-predicate (rule)
  "Get predicate of `simple-indentation' RULE."
  (-second-item rule))

(defun simple-indentation-rules-indent-current-line-p (rule)
  "Check that RULE want to indent current line."
  (-when-let
      (pred (simple-indentation-rules-predicate rule))
    (simple-indentation-rules-pred-indent-current-line-p pred)))

(defun simple-indentation-rules-pred-indent-current-line-p (pred)
  "Return non-nil, when a predicate PRED want to indent current line."
  (save-excursion (funcall pred)))

(defun simple-indentation-rules-set-indent-func (new-indent-func rule)
  "Replace indent function of RULE with NEW-INDENT-FUNC."
  (-replace-at 0 new-indent-func rule))

(defun simple-indentation-rules-call-indent-function (rule)
  "Call indent function of RULE."
  (save-excursion (beginning-of-line) (funcall (-first-item rule))))

(defun simple-indentation-rules-set-predicate (new-predicate rule)
  "Replace a predicate of RULE with NEW-PREDICATE."
  (-replace-at 1 new-predicate rule))

(defun simple-indentation-rules-add-predicate (new-predicate rule ;nofmt
                                                             &optional ;nofmt
                                                             compose-function)
  "Add a predicate NEW-PREDICATE to RULE, use for this COMPOSE-FUNCTION.

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
               (simple-indentation-rules-pred-indent-current-line-p
                new-predicate)
               (simple-indentation-rules-indent-current-line-p rule)))))
    (simple-indentation-rules-set-predicate final-pred rule)))

(defun simple-indentation-rules-apply-to-predicate (f rule)
  "Apply F to predicate of RULE, and return updated RULE."
  (let* ((old-pred (simple-indentation-rules-predicate rule))
         (new-pred (funcall f old-pred)))
    (simple-indentation-rules-set-predicate new-pred rule)))

(defun simple-indentation-rules-do (rule)
  "If RULE should be called, do it and return non-nil, otherwise return nil.

\"RULE should be called\" mean that the indent function of the RULE should be
called, RULE should be called when its predicate return non-nil"
  (and
   (simple-indentation-rules-indent-current-line-p rule)
   (simple-indentation-rules-call-indent-function rule)))

(cl-defun simple-indentation-rules-indent-line-with-sorted-rules (sorted-rules
                                                                  &key
                                                                  (each-line-before-indent-hook nil)
                                                                  (each-line-after-indent-hook nil))
  "Indent or don't indent current line depending on SORTED-RULES.

Before each line indenting call hooks EACH-LINE-BEFORE-INDENT-HOOK, after
call EACH-LINE-AFTER-INDENT-HOOK"
  (run-hooks each-line-before-indent-hook)
  (-find 'simple-indentation-rules-do sorted-rules)
  (run-hooks each-line-after-indent-hook))

(defun simple-indentation-rules-union (&rest rules)
  "Return a union rule of the all given RULES.

Return the rule which will be called when one of the rules' predicates return
non-nil, if a predicate is found, then call respective indent function."
  (simple-indentation-rules-make
   :predicate (->>
               rules
               (-map 'simple-indentation-rules-predicate)
               (apply '-orfn))
   :indent-func (lambda
                  ()
                  (simple-indentation-rules-indent-line-with-sorted-rules rules))))

(provide 'simple-indentation-rules)
;;; simple-indentation-rules.el ends here
