#!/usr/bin/env bash
. "$(dirname $(readlink -f $0))/nixos-patching-common.sh" # Import common functions

function patch_gradle_libnativeplatform() {
    local _rpath="$(find_stdcplusplus_parent_folder_in_nix_store)";
    find ./ -type f -name electron -exec nix-shell -p binutils stdenv --command 'patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" --set-rpath  '$_rpath' {}' \;
}
patch_gradle_libnativeplatform
