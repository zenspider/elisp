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
    FILE=$(basename $DOWNLOAD)-$(date +%Y%m%d)$BACKUP_EXT

    if [ ! -f "$FILE" ]; then
	    ${WGET:-wget} ${WGET_OPTS:-"-nv"} "${URL}" -O  "${FILE}"
	    rm -rf $DIR
        ls -t $(basename $DOWNLOAD)-*$BACKUP_EXT | tail +4 | xargs rm -v
    fi
    if [ ! -d $DIR ]; then
	    tar zxf $FILE
        chmod -R a+rX $DIR
    fi
done
