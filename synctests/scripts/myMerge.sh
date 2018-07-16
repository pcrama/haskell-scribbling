# myMerge ROOT PATH CURRENT1 CURRENT2 NEW CURRENTARCHOPT
_root="$1"
_path="$2"
current1="$3"
current2="$4"
_new="$5"
currentarchopt="$6"
cleanName="$_root/$_path"
tmpname="$cleanName.$(date --utc +%s).$(basename "$_root")"
if [ -f "$tmpname" ] ; then
    echo "Conflict in $cleanName"
    exit 1
else
    mv "$current1" "$tmpname"
    mv "$current2" "$_new"
fi
