#!/usr/bin/env bash
. "$(dirname $(readlink -f $0))/nixos-patching-common.sh" # Import common functions

function patch_gradle_libnativeplatform() {
    local _rpath="$(find_stdcplusplus_parent_folder_in_nix_store)";
    find ~/.gradle/native -name libnative-platform.so -exec patchelf --set-rpath "${_rpath}" {} \;
}
patch_gradle_libnativeplatform
