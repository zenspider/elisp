#!/bin/bash

set -xve

TMP=/Users/ryan/Sites/emacs/tmp

mkdir -p $TMP
cd $TMP

cvs -z3 -d:pserver:anonymous@emacswikicode.cvs.sourceforge.net:/cvsroot/emacswikicode export -f -r HEAD emacswikicode &
svn export https://svn.rizoma.cl/svn/emacswiki &

wait

cd ..

rm -rf static elisp

mv tmp/emacswikicode elisp
mv tmp/emacswiki static

rm -rf tmp
