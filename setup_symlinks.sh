#!/bin/bash

array_contains () {
    local e match="$1"
    shift
    for e; do [[ "$match" =~ "$e/" ]] && return 0; done
    return 1
}

echo "Symlinking user dirs"
# must happen before files
user_dirs=(
    user/.local/share/systemd
    user/.zsh
    user/.emacs.d
)

for dir in "${user_dirs[@]}"; do
    dest_dir=${dir/"user"/$HOME}
    # skip if dir is already symlinked
    if [[ -L "$dest_dir" && -d "$dest_dir" ]]; then continue; fi
    parent_dir=$(echo $dir | sed -E 's/\/[^/]*$//')
    mkdir -p $parent_dir
    ln -sf $(pwd)/$dir $dest_dir
done

echo "Symlinking user files"
user_files=$(find user \( -type f -o -type l \))

for file in $user_files; do
    # skip files in the symlinked directories
    if array_contains $file "${user_dirs[@]}"; then continue; fi
    dest_file=${file/"user"/$HOME}
    dir=$(echo $dest_file | sed -E 's/\/[^/]*$//')
    mkdir -p $dir
    ln -sf $(pwd)/$file $dest_file
done

echo

echo "Symlinking system files"
system_files=$(find system \( -type f -o -type l \))

for file in $system_files; do
    dest_file=${file/"system"/}
    dir=$(echo $dest_file | sed -E 's/\/[^/]*$//')
    sudo mkdir -p $dir
    sudo ln -sf $(pwd)/$file $dest_file
done
