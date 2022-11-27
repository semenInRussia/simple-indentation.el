;;; simple-indentation-lib.el --- Library of the useful rules for `simple-indentation'  -*- lexical-binding: t; -*-

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

;; Library of the useful rules for `simple-indentation'.  You can use them for
;; your targets for indentation rules of most of the languages

;;; Code:

(require 'dash)
(require 's)

(require 'simple-indentation-utils)
(require 'simple-indentation-rules)

(defun simple-indentation-lib-symbol-paren (open-paren close-paren)
  "Return a rule for indentation of things changing indent depends on 2 chars.

Returning rule will be indent current line when the previous line (ignoring
comments and strings literals consist OPEN-PAREN character and deindent current
line if it is consist CLOSE-PAREN.

For example this function will return working rule for C-languages if you pass
to it \"{\" and \"}\", resulting rule will correct indent the following code:

int main () {
  if (a == 0) {
    printf(\"%d\", a);
  }
}"
  (simple-indentation-rules-union
   (simple-indentation-rules-make
    :add-indent :begin :on-chars-in-code open-paren
    :and :not :on-chars-in-code close-paren
    :end :check-on-prev-code-line)
   (simple-indentation-rules-make
    :deindent :on-chars-in-code close-paren
    :and :not :on-chars-in-code open-paren)))

(provide 'simple-indentation-lib)
;;; simple-indentation-lib.el ends here
