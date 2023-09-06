ultimate-js-mode: A major mode for JS/JSX/TS/TSX/JSON on Emacs 29
=================================================================

Introduction
------------

This package is a major mode for editing JS/JSX/TS/TSX/JSON files. After having
used `js2-mode`, `rjsx-mode` and `typescript-mode`, I consider it the final stop
in my search for an ultimate major mode that supports all of those variants of
Javascript simultaneously.

It now only supports Emacs 29, check some earlier commit if you need Emacs 28
support.

I suggest you combine it with LSP mode (or maybe eglot) for a more complete
developer experience. You may also be interested in my `init.el` and my theme,
which you can find at `github.com/guillaumebrunerie/dotfiles`.

It provides:
- customized tree-sitter highlighting queries, as I don’t really like the
  existing ones
- electricity for JSX/TSX tags (inspired by RJSX-mode)


Installation
------------

- Make sure you have at least Emacs version 29 (this package uses builtin
  support for tree-sitter).
- Install `ultimate-js-mode` and enable it for files with extension `[jt]sx?|json`.
  For instance using `straight.el` and `use-package`:
  ```
  (use-package ultimate-js-mode
    :straight (:host github :repo "guillaumebrunerie/ultimate-js-mode")
    :mode ("\\.[jt]sx?\\'" "\\.json\\'")
  ```


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

I do not use the `feature` feature of Emacs 29's implementation of tree-sitter,
so I just use a feature called `highlighting` for everything.
