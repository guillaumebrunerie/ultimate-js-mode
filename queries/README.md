Structure taken from upstream.

- highlights-js.scm is the base file, used in all modes
- highlights-js-params.scm is used in Javascript, but cannot be used in Typescript as the grammar is
  a bit different
- highlights-ts.scm is used in Typescript and TSX
- highlights-jsx.scm is used in both Javascript (which includes JSX) and TSX
