#!/usr/bin/env bash
## Finds out the location of the libstdc++.so.6 library.
function find_stdcplusplus_in_nix_store() {
    local _fileName="libstdc++.so.6";

    ls /nix/store | grep "gcc" | grep "lib" \
    | awk -v file="${_fileName}" '{printf("find /nix/store/%s -name %s\n", $0, file);}' | sh \
    | awk -F'-' '{printf("%s %s\n", $3, $0);}' | sort | tail -n 1 | awk '{print $2;}'
}
## Finds out the location of the folder containing the libstdc++.so.6 library.
function find_stdcplusplus_parent_folder_in_nix_store() {
    local _folder="$(find_stdcplusplus_in_nix_store)";
    dirname "${_folder}";
}
