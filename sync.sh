#!/bin/bash

set -ex

cd GITZ/elisp && git down && git p4 rebase && git onup
cd GITZ/bin   && git down && git p4 rebase && git onup
