#! /bin/bash

RESTORE_PATH=$(pwd)
date=$(date '+%Y-%m-%dT%H-%M-%S')

case $2 in
    dev)
        echo "> nodemon lib/js/src/day$1/$1.js src/day$1/example.txt"
        nodemon lib/js/src/day$1/$1.js src/day$1/example.txt
        ;;
    *)
        echo "> node lib/js/src/day$1/$1.js src/day$1/input.txt"
        node lib/js/src/day$1/$1.js src/day$1/input.txt

esac

cd $RESTORE_PATH
