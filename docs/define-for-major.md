# `simple-indention-define-for-major-mode` --- Simple Indention

## Pass Namespace and Major Mode

Namespace is utils word with its each function will started (for example `python-indent`)

Major Mode is name of Emacs' `major-mode` for which this functions will created.

For pass Namespace and Major Mode you must use followed code:

```elisp
(simple-indention-define-for-major-mode python python-indent)
```

## Pass rules
Rules is utils list of rules (Really?). Rule is main conception of Simple Indention (see [Rules Make](rules-make.md "Page about Simple Indention's Rules"))

For pass rules to `simple-indention-define-for-major-mode` use something similar to followed code:

```elisp
(simple-indention-define-for-major-mode
 rust rust-mode
 :rules
 (list
  (simple-indention-rules-make
   :on-chars-in-code "{}")
  (simple-indention-rules-make
   :on-chars-in-code "[]")))
```

## Copy Indention of Previous Text Line
You may be need, that each rule start with line with indent of previous text line. This option enable by default, because this very useful.

All examples in current case is extra
## Set One Indention
By default when run one of indent functions `simple-indention`  set `simple-indention-{increment/decrement}-indent-level-function` to functions work with indent as 4 spaces, but you can customize this value, for this use keyword `:one-indent` and followed value, this create set variable `<namespace>-one-indent` to passed value.

Example:
```elisp
(simple-indention-define-for-major-mode js js
                                        :one-indent "  ")
```
This set `js-one-indent` to 2 spaces. After this `js-change-indent-level` and other indent functions will work with 2 spaces. User also can customize this variable

## Delete Old Indention
You may be need, that each rule start with indent line without indent for this use keyword `:clear-old-indention` and `t`

*NOTE* more useful, that each rule start with indent same with indent of previous text line, this option enabled by default.

Example:

```elisp
(simple-indention-define-for-major-mode js js-indention
                                        :clear-old-indention t)
```

## Clear Empty Lines
In a lot of cases you may need to don't indent empty lines (lines from only tabs, spaces, newline), instead of indent empty lines, you may need to clear this lines (replace all string to one newline).

This option enable by default.

For disable this option use keyword `:clear-empty-lines` with `nil`

Example:
```elisp
(simple-indention-define-for-major-mode
 rust rust
 :clear-empty-lines nil)
```
