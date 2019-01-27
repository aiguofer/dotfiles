#!/bin/bash

echo "Symlinking user dirs"
# must happen before files
user_dirs=(
    user/.local/share/systemd
    user/.zsh
)

for dir in "${user_dirs[@]}"; do
    dest_dir=${dir/"user"/$HOME}
    parent_dir=$(echo $dir | sed -E 's/\/[^/]*$//')
    mkdir -p $parent_dir
    ln -sf $(pwd)/$dir $dest_dir
done

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
