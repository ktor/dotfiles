#!/usr/bin/env bash
rsync -av --delete --delete-excluded --exclude-from "$HOME/.local/home-backup/rsync.exclude.files.txt" $HOME "/run/media/ktor/home-backup/daily"
rsync -av --delete --delete-excluded /etc/nixos "/run/media/ktor/home-backup/nixos"
