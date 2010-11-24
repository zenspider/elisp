#!/bin/bash

cd ~/Sites/emacs

(cd static; git pull)
(rm -rf elisp; mkdir elisp; cd elisp; ln -s ../static/*.el .)

