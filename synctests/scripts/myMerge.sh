# myMerge ROOT PATH CURRENT1 CURRENT2 NEW CURRENTARCHOPT
_root="$1"
_path="$2"
current1="$3"
current2="$4"
_new="$5"
currentarchopt="$6"
cleanName="$_root/$_path"
# Temporary simplification: the first implementation will look at the
# synchronization of only one file and the test code looks at only one
# conflict file per client (_root).  Note that this simplification means that
# new conflicts may overwrite old conflict values.
#
# tmpname="$cleanName.$(date --utc +%s).$(basename "$_root")"
tmpname="$cleanName.$(basename "$_root")"
if [ -f "$tmpname" ] ; then
    echo "Conflict in $cleanName"
    exit 1
else
    mv "$current1" "$tmpname"
    mv "$current2" "$_new"
fi
