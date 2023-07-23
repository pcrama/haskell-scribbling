;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

(("autoledger" . ((nil . ((compile-command . "(if [ -r ../autoledger.cabal ] ; then cd .. ; elif [ -r ./autoledger/autoledger.cabal ] ; then cd ./autoledger/ ; fi; echo 'Entering directory `'\"$PWD'\"; if command -v cabal 2> /dev/null > /dev/null ; then cabal test ; else nix-shell --run 'cabal test' ; fi)"))))))
