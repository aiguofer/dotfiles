#!/bin/sh

echo "Symlinking user files"
user_files=$(find user -type f)

for file in $user_files; do
    dest_file=${file/"user"/$HOME}
    dir=$(echo $dest_file | sed -E 's/\/[^/]*$//')
    mkdir -p $dir
    ln -sf $(pwd)/$file $dest_file
done

echo

echo "Symlinking system files"
system_files=$(find system -type f)

for file in $system_files; do
    dest_file=${file/"system"/}
    dir=$(echo $dest_file | sed -E 's/\/[^/]*$//')
    sudo mkdir -p $dir
    sudo ln -sf $(pwd)/$file $dest_file
done
