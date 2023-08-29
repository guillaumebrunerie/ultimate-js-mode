#!/bin/zsh

./convert.sh js < queries/highlights-jsx.scm < queries/highlights-js.scm > highlights-js.el
./convert.sh ts < queries/highlights-ts.scm < queries/highlights-js.scm > highlights-ts.el
./convert.sh tsx < queries/highlights-ts.scm < queries/highlights-jsx.scm < queries/highlights-js.scm > highlights-tsx.el
