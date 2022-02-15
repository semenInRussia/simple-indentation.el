# `simple-indentation-rules-make` --- Simple Indention

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [`rules-make` --- Simple Indention](#rules-make-----simple-indentation)
    - [General Termins](#general-termins)
    - [Run and Check Rules](#run-and-check-rules)
    - [:predicate](#predicate)
    - [:check-on-prev-line](#check-on-prev-line)
    - [:check-on-prev-code-line](#check-on-prev-code-line)
    - [:check-on-prev-code-line](#check-on-prev-code-line-1)
    - [:on-current-or-previous-line](#on-current-or-previous-line)
    - [:on-current-or-previous-code-line](#on-current-or-previous-code-line)
    - [:on-current-or-previous-text-line](#on-current-or-previous-text-line)
    - [:on-keywords](#on-keywords)
    - [:on-keywords-in-code](#on-keywords-in-code)

<!-- markdown-toc end -->

## General Termins
Rule is just type, which has `predicate` and `indent-func`.  Predicate is just function which no take arguments and return t, when current line needs to indent.  "Line needs to indent" is means, that current line needs to run `indent-func`. Indent Function (`indent-func`) is function, which no take arguments and just change current line (by idea).

Indention function defaults to simple function, which no take arguments and no return values and don't change Emacs' state

Predicate defaults to function always return t.


For create use:

```elisp
(simple-indentation-rules-make
 :predicate (lambda () (eq (% (line-number-at-pos) 2) 0))
 :indent-func (lambda () (insert "   ")))
```

For details see below!


## Run and Check Rules
For this `simple-indentation` has 3 functions:
  * `simple-indentation-rules-indent-current-line-p`
  * `simple-indentation-rules-call-indent-function`
  * `simple-indentation-rules-indent-current-line-with-func-p`
  
`simple-indentation-rules-indent-current-line-p` take rule, and return t, when this rule needs to indent current line.

`simple-indentation-rules-call-indent-function` take rule, and call `indent-func` of this rule.

`simple-indentation-rules-indent-current-line-with-func-p` is more high level function, take list of rules, search first rule needs to indent current line, and run `indent-func` of this rule.

## Mangaing with Predicates
### :predicate

Each rule can has some predicates.  If rule has (1+) predicates, then main predicate will just return t, when on of some predicates return t.

Example
```elisp
(simple-indentation-rules-make
 :predicate (lambda () (eq (% (line-number-at-pos) 2) 0))
 :predicate (lambda () (eq (% (line-number-at-pos) 3) 0)))
```

### :check-on-prev-line

This is just goto previous line before check predicate, if goto previous line is failed, then predicate will return nil.

*NOTE*: add `:check-on-prev-line` after predicate, before you want goto previous line

```elisp
(simple-indentation-rules-make
 :predicate (lambda () (eq (% (line-number-at-pos) 2) 0))
 :check-on-prev-line)
```

### :check-on-prev-code-line
This is work as [:check-on-prev-line](#:check-on-prev-line "Item of Current Page (:check-on-prev-line)"), but insead of goto previous line, this kewords go to previous no empty line.
### :check-on-prev-code-line
This is work as [:check-on-prev-line](#:check-on-prev-line "Item of Current Page (:check-on-prev-line)"), but insead of goto previous line, this kewords go to previous code (no commentary and string, empty) line.
### :on-current-or-previous-line

If predicate get nil in current line, then go to previous line and check predicate here.

*NOTE*: add `:on-current-or-previous-line` after predicate, for which you may be need to go to previous line.

Example:

```elisp
(simple-indentation-rules-make
 :on-chars "!"
 :on-current-or-previous-line)
```

### :on-current-or-previous-code-line

If predicate get nil in current line, then go to previous code line (see [:check-on-prev-code-line](#:check-on-prev-code-line)) and check predicate here.

*NOTE*: add `:on-current-or-previous-code-line` after predicate, for which you may be need to go to previous line.

Example:

```elisp
(simple-indentation-rules-make
 :on-chars "!"
 :on-current-or-previous-code-line)
```

### :on-current-or-previous-text-line

If predicate get nil in current line, then go to previous text line (see [:check-on-prev-text-line](#:check-on-prev-text-line)) and check predicate here.

*NOTE*: add `:on-current-or-previous-text-line` after predicate, for which you may be need to go to previous line.

Example:

```elisp
(simple-indentation-rules-make
 :on-chars "!"
 :on-current-or-previous-text-line)
```
### :on-keywords
Make predicate, which return t, when current line has one of passed keywords.

This keywords needs to required folowed argument - list of strings (keywords).

*NOTE*: In a lot of cases you should use `:on-keywords-in-code` for ignore keywords in commentaries

Example (super simple):
```elisp
(simple-indentation-rules-make
 :on-keywords "end" "endif" "endfor")
```

Example 2 (with `:check-on-prev-code-line`):
```elisp
(simple-indentation-rules-make
 :on-keywords "end" "endif" "endfor"
 :check-on-prev-code-line)
 ```
 
Very important, that `:check-on-prev-code-line` go after `:on-keywords` in section about `:check-on-prev-line` I say, that this keyword needs to put after predicate, you must keep in mind, that `:on-keywords` is also "predicate"

### :on-keywords-in-code
Make predicate, which return t, when current line, ignoring comments and strings, has one of passed keywords.

This keywords needs to required folowed argument - list of strings (keywords).

Example (super simple):
```elisp
(simple-indentation-rules-make
 :on-keywords-in-code "end" "endif" "endfor")
```

Example 2 (with `:check-on-prev-code-line`):
```elisp
(simple-indentation-rules-make
 :on-keywords-in-code "end" "endif" "endfor"
 :check-on-prev-code-line)
 ```
 
Very important, that `:check-on-prev-code-line` go after `:on-keywords` in section about `:check-on-prev-line` I say, that this keyword needs to put after predicate, you must keep in mind, that `:on-keywords-in-code` is also "predicate"
### :on-chars
Make predicate, which return t, when current line, has one of chars of passed string.

This keywords needs to required folowed argument - string (chars).

*NOTE*: In a lot of cases you should use `:on-chars-in-code` for ignore chars in commentaries

Example (super simple):
```elisp
(simple-indentation-rules-make
 :on-chars "{}")
```

Example 2 (with `:check-on-prev-code-line`):
```elisp
(simple-indentation-rules-make
 :on-chars "{}
 :check-on-prev-code-line)
 ```
 
Very important, that `:check-on-prev-code-line` go after `:on-chars` in section about `:check-on-prev-line` I say, that this keyword needs to put after predicate, you must keep in mind, that `:on-chars` is also "predicate"
### :on-chars-in-code
Make predicate, which return t, when current line, ignoring comments and strings, has one of chars of passed string.

This keywords needs to required folowed argument - string (chars).

Example (super simple):
```elisp
(simple-indentation-rules-make
 :on-chars-in-code "{}")
```

Example 2 (with `:check-on-prev-code-line`):
```elisp
(simple-indentation-rules-make
 :on-chars-in-code "{}"
 :check-on-prev-code-line)
 ```
 
Very important, that `:check-on-prev-code-line` go after `:on-chars` in section about `:check-on-prev-line` I say, that this keyword needs to put after predicate, you must keep in mind, that `:on-chars-in-code` is also "predicate"
    
    
## Mangaing with `indent-func`s
### :indent-func
`:indent-func` set `indent-func` of current rule to passed function. Each rule can't have some `indent-func`, so if you use `:indent-func` some times, that `indent-func` of rule will be last passed `indent-func`

Example:
```elisp
(simple-indentation-rules-make
 :indent-func (lambda () (insert "    ")))
```
### :add-indent
Set `indent-func` of current rule to function on value of `simple-indentation-increment-indent-level-function`, this variable automatic set after run one of functions defines by `simple-indentation-define-for-major-mode`, by idea this function just insert `one-indent` for current major mode.

```elisp
(simple-indentation-rules-make
 :add-indent)
```

### :deindent
Set `indent-func` of current rule to function on value of `simple-indentation-decrement-indent-level-function`, this variable automatic set after run one of functions defines by `simple-indentation-define-for-major-mode`, by idea this function chop `one-indent` when current line start with `one-indent`.

```elisp
(simple-indentation-rules-make
 :deindent)
```
