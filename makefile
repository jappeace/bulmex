OPTIMIZATION=-O0
build: update-cabal
	cabal new-build all -j --ghc-options $(OPTIMIZATION)

file-watch:
	scripts/watch.sh

update-cabal:
	hpack ./bulmex

EXTRA=""
enter:
	nix-shell --cores 0 -j 8 $(EXTRA)

run: create-db
	./dist-newstyle/build/x86_64-linux/ghc-8.4.3/backend-1.0.0.0/x/webservice/build/webservice/webservice

clean:
	rm -fR dist dist-*


