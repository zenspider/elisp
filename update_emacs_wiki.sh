#!/bin/bash

set -xve

DIR=/Users/ryan/Sites/emacs

cd $DIR

# cvs is pretty light, so we just do a regular checkout/update.
if [ -d elisp ]; then
    (cd elisp; cvs -q up -AdP) &
else
    cvs -z3 -d:pserver:anonymous@emacswikicode.cvs.sourceforge.net:/cvsroot/emacswikicode co -f -r HEAD -d elisp emacswikicode &
fi

(cd static; git pull) &

wait
