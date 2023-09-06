#!/bin/zsh

for lang in js jsx ts
do
	./convert.sh $lang < queries/highlights-$lang.scm > highlights-$lang.el
done
