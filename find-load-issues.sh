#!/bin/bash

for ff in $(find . -name \*.el); do
    f=$(basename $ff)
    for d in $(emacs --batch --eval "(progn (mapcar 'print load-path) (print (expand-file-name \"~/Sites/emacs/elisp\")))"|sed -n -e 's/\"//gp') /Users/ryan/Work/svn/ruby/ruby_1_8/misc/; do
        if [ -f $d/$f ]; then
            if ! cmp -s $d/$f $ff; then
                echo "  diff $d/$f $ff"
            fi
        fi
        if [ -f $d/$f.gz ]; then
            echo "  FOUND IN $d/${f}c"
            zdiff -q $d/$f.gz $ff
        fi
    done
done