#!/usr/bin/env bash
source ~/.bashrc
j4-dmenu-desktop --dmenu="(cat ; (stest -flx $(echo $PATH | tr : ' ') | sort -u)) | dmenu -i"
