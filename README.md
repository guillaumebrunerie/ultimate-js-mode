ultimate-js-mode
================

Description
-----------

This package is a major mode for editing JS/JSX/TS/TSX files. I am developing it
due to the lack of TSX mode for Emacs, and also because it doesn’t make much
sense to me to use different modes for JS (js2-mode), JSX (rjsx-mode), TS
(typescript-mode), and TSX (no good option at the moment). So this major mode is
meant to work for all of them. This is what the name refers to, a single
Javascript mode that works for all variants of Javascript that I care about.

It is in early phase of development and is a bit more complicated to install
than regular packages, but I’m using it every day and it works just fine for me.

What it provides is mostly syntax highlighting, indentation, and some
electricity. I suggest to combine it with something like LSP mode to get a more
complete editing experience

For syntax highlighting I disabled some of the upstream rules that I disagree
with. For instance I don’t think variables representing functions should be
colored differently, otherwise the following two lines will give different
colors for `f`,
`const f = (x, y) => x + y;`
`const f = memoize((x, y) => x + y);`
and I don’t think that all caps identifiers should be colored differently,
otherwise `React` and `PIXI` would be colored differently in:
`import * as React from "react";`
`import * as PIXI from "pixi.js";`
I do want capitalized identifiers to be colored differently, though, as they
typically represent either React components or "modules", rarely normal values.

I rarely use classes, so they may have more highlighting issues.


Installation
------------

- Make sure you have at least Emacs version 27 (indentation for JSX uses the
  built-in `js-mode` which added support for JSX in Emacs 27).
- Install `ultimate-js-mode`. If you are using `straight.el` (recommended) the recipe is something
  like `(:host github :repo "guillaumebrunerie/ultimate-js-mode" :files (:defaults "libs" "queries"))`
- From the `ultimate-js-mode` directory, compile the tree-sitter grammars into
  the `libs` directory using the following commands (Linux, maybe Mac, probably
  not Windows)
  `gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-javascript/src/{,scanner.c,parser.c} -o libs/javascript.so`
  `gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-typescript/typescript/src/{,scanner.c,parser.c} -o libs/typescript.so`
  `gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-typescript/tsx/src/{,scanner.c,parser.c} -o libs/tsx.so`


Technical details
-----------------

This package uses:
- tree-sitter for syntax highlighting, the grammar was forked from upstream as
  there was a few things that needed to be fixed (need to send pull requests),
  and the highlighting queries were also tweaked from upstream
- the built-in js-mode for automatic identation of JS/JSX, with some tweaks
  based on tree-sitter to make it work for TS/TSX as well. There might be still
  some rough edges for TS/TSX, but it’s already pretty usable.

In addition, it adds the following (ideally it should be possible to disable
them, but currently it isn’t)
- electricity for JSX tags (inspired by RJSX-mode)
- syntax-aware electricity for parentheses: when placing an opening parenthesis,
  it places the closing parenthesis at the end of the subtree starting at point.
  Can be interesting sometimes, maybe should be split into a separate package as
  it is not particularly specific to Javascript/Typescript.
- (disabled for now) moving between references of a symbol in the same file
  (inspired by js2-highlight-vars)

