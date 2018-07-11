#!/bin/sh
noTrailingSlash="${1%/}"
scriptDir="$(dirname "$0")"
if [ -n "$1" -a -r "$noTrailingSlash" ] ; then
    # "Subtle" bug: in hierarchies of a certain depth all file with
    # names starting with 4 hex digits get deleted:
    for d in "$noTrailingSlash"/*/*/*/ ; do
        find "$1" -type f -iname '[0-9a-f][0-9a-f][0-9a-f][0-9a-f]*' -print0 | xargs -0 rm -f
    done
    # End of subtle bug
    unison -root "$noTrailingSlash/" -root /root/synctests/server/ -fat -dumbtty -silent -auto \
           -ignore 'Name *~' -ignore 'Name #*' -ignore 'Name *#' -ignore 'Name *.bak' \
           -backupcurr 'Name *' \
           -merge 'Name * -> sh "'"$scriptDir/myMerge.sh"'" "'"$noTrailingSlash"'" PATH CURRENT1 CURRENT2 NEW CURRENTARCHOPT'
else
    echo "$(basename "$0") [$(find . -maxdepth 1 -not -name . -not -name server -type d | sort | awk 'BEGIN { r = "" } { sub(/^\.\//, ""); r = (r?r " | ":"") $1 } END { print r }')]"
    exit 1
fi
