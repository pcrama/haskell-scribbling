# sha256-file-compare

Compare two `sha256sum`-style file lists.

## Build

```text
cabal build
```

## Run

```text
cabal run compare-files -- input-1-old.txt input-2-new.txt
```

## Tests

```text
cabal test
```

Matching uses only the basename. Before comparison, the basename is converted to lowercase and all whitespace is removed.

Duplicate detection is performed on the old list and groups entries by hash.

Changed files have a matching normalized basename in both lists but different hashes.

Removed files are old entries whose normalized basename is neither in the new list nor in the changed-file list.

New files are new entries whose normalized basename is neither in the changed-file list nor in the removed-file list.
