HC=ghc
BIN=semver

semver: Main.hs
	$(HC) -o $(BIN) Main.hs

clean:
	rm -f *.hi *.o $(BIN)
