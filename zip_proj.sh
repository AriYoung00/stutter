#!/bin/bash


if [[ ! -d "$1" ]]; then
    echo "Could not find directory '$1'"
    exit 1
fi

path=$1
path="${path%/}"

echo "Zipping project at path '$path'"

cd $path
echo "Cleaning project..."
cargo clean
cd -

echo "Zipping project..."
zip -r "${path}_ayoung.zip" $path/**

status=$?
if [ $status -ne 0 ]; then
    echo "Error zipping project -- code $status"
    exit 1
else
    echo "Successfully zipped project to '${path}_ayoung.zip'"
    exit 0
fi

