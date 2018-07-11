#!/bin/sh
for d in $(find . -maxdepth 1 -type d -not -name .)
do
    echo -n "$d: "
    ( cd "$d/" ; \
      find . \
           \( -type f \
              -not -name \*\~ -not -name \*.bak -not -name \#\* -not -name \*\# \
              -exec md5sum '{}' ';' \) \
           -o \( -type d -print \) ) \
    | sort \
    | md5sum
done
