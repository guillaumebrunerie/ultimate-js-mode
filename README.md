ultimate-js-mode: A major mode for JS/JSX/TS/TSX
================================================

Introduction
------------

This package is a major mode for editing JS/JSX/TS/TSX files. After having used
`js2-mode`, `rjsx-mode` and `typescript-mode`, it is the final stop in my search
for an ultimate major mode that supports all of those variants of Javascript
simultaneously.

I suggest you combine it with LSP mode (or maybe eglot) for a more complete
developer experience. You may also be interested in my `init.el` and my theme,
which you can find at `github.com/guillaumebrunerie/dotfiles`.

It provides:
- syntax highlighting, based on tree-sitter with customized grammars and
  highlighting queries as there are quite a few imperfections in the existing
  ones
- indentation, *not* based on tree-sitter (see below) but on js-mode, tweaked to
  work in a satisfactory way for Typescript/TSX as well
- electricity for JSX/TSX tags (inspired by RJSX-mode)
- syntax-aware electricity for parentheses: when placing an opening parenthesis,
  it places the closing parenthesis at the end of the subtree starting at point.
  Maybe should be split up into a separate package as it is not particularly
  specific to Javascript/Typescript.

It is in early phase of development and you’ll need to compile the grammars
yourself for now (see Installation section below), but I am using it every day
at work and at home, and I am very happy with it.


Main contributions
------------------

- Typescript/TSX indentation based on js-mode and with the help of tree-sitter.
- Improved grammars and syntax highlighting compared to upstream and `tree-sitter-langs`


Installation
------------

- Make sure you have at least Emacs version 27 (indentation for JSX uses the
  built-in `js-mode` which added support for JSX in Emacs 27).
- Install `ultimate-js-mode` and enable it for files with extension `[jt]sx?|json`.
  The files in the `libs` and `queries` directory need to be next to the loaded
  mode file.
  For instance using `straight.el` and `use-package`:
  ```
  (use-package ultimate-js-mode
    :straight (:host github :repo "guillaumebrunerie/ultimate-js-mode" :files (:defaults "libs" "queries"))
    :mode ("\\.[jt]sx?\\'" "\\.json\\'")
  ```
- From the `ultimate-js-mode` directory, compile the tree-sitter grammars into
  the `libs` directory using the following commands (works on Linux and Mac, no
  idea about Windows)
  `gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-javascript/src/{,scanner.c,parser.c} -o libs/javascript.so`
  `gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-typescript/typescript/src/{,scanner.c,parser.c} -o libs/typescript.so`
  `gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-typescript/tsx/src/{,scanner.c,parser.c} -o libs/tsx.so`


Syntax highlighting
-------------------

Syntax highlighting uses tree-sitter and tweaked versions of the upstream
highlighting queries. I also tried the queries from `tree-sitter-langs` but it
had quite a few issues, so I decided to base my version on the upstream queries
instead.

There are a few rules I disabled because I disagree with them.

For instance I don’t think we should attempt to color differently variables
representing functions, otherwise the following two lines will give different
colors for `f`:
`const f = (x, y) => x + y;`
`const f = memoize((x, y) => x + y);`

I also don’t think that all caps identifiers should be colored differently,
otherwise `React` and `PIXI` would be colored differently in:
`import * as React from "react";`
`import * as PIXI from "pixi.js";`

I do want capitalized identifiers to be colored differently than regular
variables, though, as they typically represent either React components or
namespaces/modules, but rarely regular values.

The general principle about identifiers is that there are three different kind
of identifiers: values, types, and special values (capitalized values), and each
occurence of an identifier is either a binding/definition/parameter or a usage.
This means there should be 3×2 different colors for identifier.
In my current theme, types are green (brighter for binders), special values are
blue (brighter for binders), and values are yellow/white for binders/uses.


Indentation
-----------

I hoped for a while that I would be able to use `tree-sitter` to determine
perfect indentation, but I realized after some time that it was significantly
harder than I expected, and even hopeless in some situations. The issue is that
indentation should work even in incomplete files (when you are in the process of
writing code) but `tree-sitter` (understandably) often gives bogus trees if the
file is not syntactically correct. It is acceptable for syntax highlighting, but
not for indentation unless you add a bunch of special cases. Indenting empty
lines is also very tricky with `tree-sitter` as there is no node to refer to.

Instead I decided to use indentation from the built-in js-mode, which does an
amazing job at indenting JS/JSX files, and adapted it for TS/TSX. It took
surprisingly little code (5 lines) and works very well.
