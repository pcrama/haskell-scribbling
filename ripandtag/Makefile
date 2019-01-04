GHC=ghc -Wincomplete-patterns -Wmissing-fields -Wdodgy-imports

@phony test: test/Spec
	test/Spec

test/Spec: src/ParseRipSpec.hs src/Shell.hs test/Spec.hs
	$(GHC) src/ParseRipSpec.hs src/Shell.hs test/Spec.hs

app/Main: src/*.hs app/*.hs
	$(GHC) src/ParseRipSpec.hs src/Shell.hs app/Main.hs
