#!/bin/bash

set -xv

cd "/Users/ryan/Sites/emacs/"

STATIC="http://www.emacswiki.org/emacs/static.tar.gz"
STATIC_F="static-$(date +%Y%m%d).tar.gz"
ELISP="http://www.emacswiki.org/emacs/elisp.tar.gz"
ELISP_F="elisp-$(date +%Y%m%d).tar.gz"

wget -cnv $STATIC -O $STATIC_F &
wget -cnv $ELISP  -O $ELISP_F  &

wait

rm -rf static elisp

tar zxf $STATIC_F &
tar zxf $ELISP_F  &

wait

chmod -R a+rX static elisp

find static -name \*.html -print0 | xargs -0 perl -pi -e 's%(=")/cgi-bin/emacs/(?:download/)?%$1../elisp/%g;'
    
ls -t static-* | tail +4 | xargs rm -v
ls -t elisp-*  | tail +4 | xargs rm -v
