# Contributing

Contributions are welcome. If you discover bugs or issues, or have ideas for improvements or new features, please file a report on the issue tracker for this repository. Follow the guidelines below to make sure everything goes smoothly.

## Issue reporting

- Check that the issue has not already been reported
- Check that the issue has not already been fixed in the latest code
- Open an issue with a clear title
- Write as grammatically correct as you can in the description.

## Pull requests

- Perform all changes on a topic branch for easier merging
- Follow the coding conventions already in use
- Verify Emacs Lisp code with `melpazoid`, it's available at the CI/CD tab
- Add unit tests whenever possible
- Open a [pull request](https://help.github.com/articles/using-pull-requests)
  relating to a single issue.


### Naming

- Use a `simple-indentation-` prefix for all public names.
- Use a `simple-indentation--` prefix for all internal names.
- If you write a code at the module named `simple-indentation-hooks`,
  then use a `simple-indentation-hooks-` for all public names and a
  `simple-indentation-hooks--` prefix for all internal names.  For
  each module this rule works also

### Docstrings

Write meaningful docstrings for all functions and vars.

- Document all functions and variables as directed by `checkdoc`.
- Consider using [Flycheck](https://github.com/flycheck/flycheck) to automate
  `checkdoc` while you're editing.
- You can also check a result of the `melpazoid` when available at the
  CI/CD tab at the Git repository page

### Common Lisp functions

Use `cl-lib` instead of `cl`. The `cl` library pollutes the global namespace and
its usage is therefore discouraged.

- Use `cl-lib`, which adds prefixes to all cl function names
- Use [noflet](https://github.com/nicferrier/emacs-noflet) instead of `flet`
  when you need to dynamically rebind functions.
