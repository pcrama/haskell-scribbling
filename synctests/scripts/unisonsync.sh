#!/bin/sh
# unisonsync.sh client-dir logfile
#
# When no changes needed to be propagated, logfile isn't updated/created
noTrailingSlash="${1%/}"
parent="$(dirname "$noTrailingSlash")"
scriptDir="$(dirname "$0")"
logfile="$2"
if test -n "$noTrailingSlash" -a -r "$noTrailingSlash" -a \
        -n "$logfile" -a \( \! -e "$logfile" -o -w "$logfile" \) ; then
    noTrailingSlash="$(cd "$noTrailingSlash" && pwd)"
    if [ -z "$noTrailingSlash" ] ; then
        echo "$(basename "$0"): unable to cd to '$1'"
        exit 2
    fi
    if [ -n "$parent" ] ; then
        parent="$(cd "$parent" && pwd)"
        if [ -z "$parent" ] ; then
            echo "$(basename "$0"): unable to cd to parent of '$1'"
            exit 3
        fi
    fi
    # "Subtle" bug: in hierarchies of a certain depth all files with
    # names starting with 4 hex digits get deleted:
    for d in "$noTrailingSlash"/*/*/*/ ; do
        find "$1" -type f -iname '[0-9a-f][0-9a-f][0-9a-f][0-9a-f]*' -print0 | xargs -0 rm -f
    done
    # End of subtle bug
    export UNISON="$parent/.unison"
    touch "$parent/.start.$(basename "$noTrailingSlash")"
    unison -root "$noTrailingSlash/" -root "$parent/server" -fat -dumbtty -silent -auto \
           -ignore 'Name *~' -ignore 'Name #*' -ignore 'Name *#' -ignore 'Name *.bak' \
           -backupcurr 'Name *' \
           -merge 'Name * -> sh "'"$scriptDir/myMerge.sh"'" "'"$noTrailingSlash"'" PATH CURRENT1 CURRENT2 NEW CURRENTARCHOPT' \
           -log -logfile "$logfile"
    touch "$parent/.stop.$(basename "$noTrailingSlash")"
else
    echo "$(basename "$0") [$(cd "$parent" ; find . -maxdepth 1 -not -name .\* -not -name server -type d | sort | awk 'BEGIN { r = "" } { sub(/^\.\//, ""); r = (r?r " | ":"") $1 } END { print r }')] logfile"
    exit 1
fi
