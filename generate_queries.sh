#!/bin/zsh

./convert.sh js < queries/highlights-js.scm < queries/highlights-jsx.scm > highlights-js.el
./convert.sh ts < queries/highlights-js.scm < queries/highlights-ts.scm > highlights-ts.el
./convert.sh tsx < queries/highlights-js.scm < queries/highlights-jsx.scm < queries/highlights-ts.scm > highlights-tsx.el
