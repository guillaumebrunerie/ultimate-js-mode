#!/bin/zsh

ROOT=$PWD

echo -n "Compiling Javascript grammar… "
cd $ROOT/tree-sitter-javascript && npx tree-sitter generate
cd $ROOT && gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-javascript/src/{,scanner.c,parser.c} -o libs/javascript.so
echo "done"

echo -n "Compiling Typescript grammar… "
cd $ROOT/tree-sitter-typescript/typescript && npx tree-sitter generate
cd $ROOT && gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-typescript/typescript/src/{,scanner.c,parser.c} -o libs/typescript.so
echo "done"

echo -n "Compiling TSX grammar…        "
cd $ROOT/tree-sitter-typescript/tsx && npx tree-sitter generate
cd $ROOT && gcc -shared -fPIC -fno-exceptions -g -O2 -I tree-sitter-typescript/tsx/src/{,scanner.c,parser.c} -o libs/tsx.so
echo "done"
