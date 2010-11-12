#!/bin/bash

for ff in $(find . -name \*.el); do
    f=$(basename $ff)
    for d in $(emacs --batch --eval "(progn (mapcar 'print load-path) (print (expand-file-name \"~/Sites/emacs/elisp\")))"|sed -n -e 's/\"//gp') /Users/ryan/Work/svn/ruby/ruby_1_8/misc/; do
        if [ -f $d/$f -a $f != setup-keys.el ]; then
            if ! cmp -s $d/$f $ff; then
                echo "  diff -u $ff $d/$f"
            fi
        fi
        if [ -f $d/$f.gz ]; then
            echo "  FOUND IN $d/${f}c"
            zdiff -q $ff $d/$f.gz
        fi
    done
done
