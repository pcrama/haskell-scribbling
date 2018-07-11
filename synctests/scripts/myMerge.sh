# myMerge ROOT PATH CURRENT1 CURRENT2 NEW CURRENTARCHOPT
_root="$1"
_path="$2"
current1="$3"
current2="$4"
_new="$5"
currentarchopt="$6"
cleanName="$_root/$_path"
if [ -n "$currentarchopt" ] ; then
    diff3 -m "$current1" "$currentarchopt" "$current2" > "_$new"
else
    if [ -f "$cleanName.server" ] ; then
        echo "Conflict in $cleanName"
        exit 1
    else
        mv "$current1" "$cleanName.$_root"
        mv "$current2" "$_new"
    fi
fi
