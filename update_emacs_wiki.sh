#!/bin/bash

EMACSWIKI_BACKUP_DIR="/Users/ryan/Sites/emacs/"

cd $EMACSWIKI_BACKUP_DIR

EMACSWIKI_URL="http://www.emacswiki.org/"
BACKUP_EXT=".tar.gz"
DEFAULT_DOWNLOAD="emacs/static emacs/elisp"
DOWNLOADS=${@:-$DEFAULT_DOWNLOAD}

for DOWNLOAD in $DOWNLOADS; do
    DIR=$(basename $DOWNLOAD)
    URL=$EMACSWIKI_URL$DOWNLOAD$BACKUP_EXT
    FILE=$DIR-$(date +%Y%m%d)$BACKUP_EXT

    if [ ! -f "$FILE" ]; then
	    ${WGET:-wget} ${WGET_OPTS:-"-nv"} "${URL}" -O  "${FILE}"
	    rm -rf $DIR
        ls -t $DIR-*$BACKUP_EXT | tail +4 | xargs rm -v
    fi
    if [ ! -d $DIR ]; then
	    tar zxf $FILE
        chmod -R a+rX $DIR
        if [ $DIR = "static" ]; then
            find static -name \*.html -print0 | xargs -0 perl -pi -e 's%(=")/cgi-bin/emacs/(?:download/)?%$1../elisp/%g;'
        fi
    fi
done

