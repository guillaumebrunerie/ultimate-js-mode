#!/bin/zsh

# This is a hacky script to run highlighting tests.
# Given that the `tree-sitter test` is hardcoded to look for a `package.json`
# (to get the queries) and an `src` folder, we make a temporary folder in which
# we copy the correct `src` folder, the queries, the tests, and a custom made
# `package.json` (see test-utils/)

ROOT=$PWD
JS=tree-sitter-javascript
TS=tree-sitter-typescript/typescript
TSX=tree-sitter-typescript/tsx
TEST=test/highlight
TEST_TMP=$ROOT/test_tmp
UTILS=$ROOT/test-utils
rm -rf $TEST_TMP

echo "Testing Javascript/JSX"
echo
mkdir $TEST_TMP
cd $TEST_TMP
cp -r $ROOT/$JS/src .
mkdir queries
cp -r $ROOT/queries/highlights-js* queries
mkdir -p $TEST
cp $ROOT/tests/regression.js $TEST/regression.js
cp $UTILS/package-js.json package.json
tree-sitter test || exit 1
cd $ROOT
rm -rf $TEST_TMP
echo
echo

echo "Testing Typescript"
echo
mkdir $TEST_TMP
cd $TEST_TMP
cp -r $ROOT/$TS/src .
sed -i "s#../../common/scanner.h#./scanner.h#" src/scanner.c
cp $ROOT/$TS/../common/scanner.h src
mkdir queries
cp -r $ROOT/queries/highlights-{js,ts}.scm queries
mkdir -p $TEST
cp $ROOT/tests/regression.js $TEST/regression-js.ts
cp $ROOT/tests/regression.ts $TEST/regression.ts
cp $UTILS/package-ts.json package.json
tree-sitter test || exit 1
cd $ROOT
rm -rf $TEST_TMP
echo
echo

echo "Testing TSX"
echo
mkdir $TEST_TMP
cd $TEST_TMP
cp -r $ROOT/$TSX/src .
sed -i "s#../../common/scanner.h#./scanner.h#" src/scanner.c
cp $ROOT/$TSX/../common/scanner.h src
mkdir queries
cp -r $ROOT/queries/highlights-{js,ts,jsx}.scm queries
mkdir -p $TEST
cp $ROOT/tests/regression.js $TEST/regression-js.tsx
cp $ROOT/tests/regression.ts $TEST/regression-ts.tsx
cp $ROOT/tests/regression.tsx $TEST/regression.tsx
cp $UTILS/package-tsx.json package.json
tree-sitter test || exit 1
cd $ROOT
rm -rf $TEST_TMP
