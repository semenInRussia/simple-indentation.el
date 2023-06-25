# simple-indentation

[![Run Tests and Eldev Linter](https://github.com/semenInRussia/just.el/actions/workflows/test.yml/badge.svg)](https://github.com/semenInRussia/simple-indentation.el/actions/workflows/test.yml)


## Summary

Simple Define Indent's Funcs for Your Cool Major Modes in Emacs

## Small Example

Here I am implement indention functions for `js`:

```elisp
(require 'simple-indentation)

(simple-indentation-define-for-major-mode
 js js
 :one-indent "  " ; By default 4 spaces
 :rules
 (list
  (simple-indentation-rules-make
   ;; This rule ignore other rules
   ;; on situations:
   ;; if {log()}
   ;; |
   ;;
   ;; OR
   ;;
   ;; if {log()}|
   ;;
   :begin
   :on-chars-in-code "}"
   :and
   :on-chars-in-code "{"
   :end
   :on-current-or-previous-code-line)
  (simple-indentation-rules-make
   ;; Indent when previous line has "{"
   ;; Example:
   ;;
   ;; if (ready) {
   ;; |
   ;;
   :add-indent
   :on-chars-in-code "{"
   :check-on-prev-code-line)
  (simple-indentation-rules-make
   ;; Deindent when line has "}"
   ;; Example:
   ;;
   ;;   hack
   ;;   }|
   ;;
   :deindent
   :on-chars-in-code "}")))

```

This create some functions:
- `js-indent-region`
- `js-indent-line`
- `js-indent-two-lines` for indent current and previous lines
- `js-indent-lines` take starting line's number, and ending line's number, indent region beetwen its

Some hooks:
- `js-each-line-before-indent`
- `js-each-line-after-indent`
- `js-before-indent-line-hook` run before `js-indent-line`
- `js-after-indent-line-hook` run after `js-indent-line`
- `js-before-indent-lines-hook` run before `js-indent-lines`
- `js-after-indent-lines-hook` run after `js-indent-lines`
- `js-before-indent-region-hook` run before `js-indent-region`
- `js-after-indent-region-hook` run after `js-indent-region`
- `js-before-indent-some-lines-hook` run before `js-indent-region` and `js-indent-lines`
- `js-after-indent-some-lines-hook` run after `js-indent-region` and `js-indent-lines`
- `js-before-run-indent-func-hook` run after `js-indent-region`, `js-indent-lines` and `js-indent-line`
- `js-after-run-indent-func-hook` run after `js-indent-region`, `js-indent-lines` and `js-indent-line`

And some variables:
- `js-indention-rules` list of `simple-indentation-rules`
- `js-one-indent` in this example 2 spaces

## Usage
Two main function of `simple-indentation` is:

  * `simple-indentation-rules-make` ([link](docs/rules-make.md "Link to Full Documentaion about simple-indentation-rules-make") to full doc)
  * `simple-indentation-define-for-major-mode`. ([link](docs/define-for-major.md "Link to Full Documentaion about simple-indentation-define-for-major-mode") to full doc)

First is very big for function for create indention rules, this is main conception of `simple-indentation` (rules -> indent function)

Second is just easy wrapper on `simple-indentation-rules-make`, which create some useful indent functions and a lot of hooks and cople of variables (see [Small Example][Small Example])
## Contributing

Yes, please do! See [CONTRIBUTING][CONTRIBUTING.md] for guidelines.

## License

Copyright (c) 2022 Semen Khramtsov.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
