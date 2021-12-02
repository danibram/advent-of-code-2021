#! /bin/bash

RESTORE_PATH=$(pwd)
date=$(date '+%Y-%m-%dT%H-%M-%S')

case $2 in
    dev)
        echo "> nodemon $1/lib/js/src/day$2/$2.js src/day$2/example.txt"
        nodemon $1/lib/js/src/day$2/$2.js src/day$2/example.txt
        ;;
    *)
        echo "> node $1/lib/js/src/day$2/$2.js src/day$2/input.txt"
        node $1/lib/js/src/day$2/$2.js src/day$2/input.txt

esac

cd $RESTORE_PATH
