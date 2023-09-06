#!/bin/zsh

echo "(defun ultimate-js-mode--partial-queries-$1 (language)
  \"Custom highlighting queries\"
  (list
   :language language
   :feature 'highlight
   '(_
"

sed -e '
s/@\([a-z.]*\)/@font-lock-\1-face/g
s/@font-lock-js-face--fontify-template-string/@js--fontify-template-string/g
s/@font-lock-ts-face--fontify-template-string/@ts--fontify-template-string/g
s/\(.* \. .*\)/)\n   :language language\n   :feature '\''highlight\n   "\1"\n\n   :language language\n   :feature '\''highlight\n   '\''(_/g
s/^\([[(]\)/)\n   :language language\n   :feature '\''highlight\n   '\''(\1/g
s/^./   &/g
s/#match? \(@[^ ]*\) \(\".*\"\)/:match \2 \1/g
s/#eq? \(@[^ ]*\) \(\".*\"\)/:match \2 \1/g
/:match/ s/"\^/"\\\\\`/g
/:match/ s/\$/\\\\'\''/g
/:match/ s/|/\\\\\|/g
s/@font-lock-variable-face/@font-lock-variable-name-face/g
s/@font-lock-constructor-face/@font-lock-function-name-face/g
s/@font-lock-variable.builtin-face/@font-lock-builtin-face/g
s/@font-lock-function.builtin-face/@font-lock-builtin-face/g
s/@font-lock-constant.builtin-face/@font-lock-builtin-face/g
s/@font-lock-type.builtin-face/@font-lock-builtin-face/g
s/@font-lock-function-face/@font-lock-variable-name-face/g
s/@font-lock-punctuation.delimiter-face/@font-lock-delimiter-face/g
s/@font-lock-punctuation.special-face/@font-lock-punctuation-face/g
s/@font-lock-punctuation.bracket-face/@font-lock-bracket-face/g
s/@font-lock-string.special-face/@font-lock-regexp-face/g
s/@font-lock-string.special-face/@font-lock-regexp-face/g
s/@font-lock-type.parameter-face/@font-lock-type-parameter-face/g
'

echo ")))"
