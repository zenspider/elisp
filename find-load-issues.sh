#!/bin/bash

for ff in **/*/*.el; do
    f=$(basename $ff)
    for d in $(emacs --batch --eval "(progn (mapcar 'print load-path) (print (expand-file-name \"~/Sites/emacs/elisp\")))"|sed -n -e 's/\"//gp'); do
        if [ -f "$d/$f" ]; then
            echo "  FOUND IN $d/$f"
            diff -q $ff "$d/$f"
        fi
        if [ -f "$d/$f.gz" ]; then
            echo "  FOUND IN $d/$f.gz"
        fi
        if [ -f "$d/${f}c" ]; then
            echo "  FOUND IN $d/${f}c"
        fi
    done
done