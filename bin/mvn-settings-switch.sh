#!/usr/bin/env bash
FILE="$HOME/.m2/settings-$1.xml"
if [[ -f $FILE ]]; then
  cp $FILE ~/.m2/settings.xml
else
  echo "$FILE does not exist."
fi
