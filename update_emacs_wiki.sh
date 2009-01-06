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

# svn sucks because of how much disk space the .svn dir takes up, so
# we export and rsync in the differences instead. that makes it easier
# on my backups too.
TMP=newstatic.noindex
(rm -rf $TMP; svn export https://svn.rizoma.cl/svn/emacswiki $TMP; rsync -r --delete $TMP/ static; rm -rf $TMP) &

wait
