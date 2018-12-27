@phony test: test/Spec
	test/Spec

test/Spec: src/ParseRipSpec.hs src/Shell.hs test/Spec.hs
	ghc src/ParseRipSpec.hs src/Shell.hs test/Spec.hs
