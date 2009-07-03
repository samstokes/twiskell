HC = ghc
HC_OPTS =

EXE = twitsearch

$(EXE):
	$(HC) --make -o $(EXE) $(HC_OPTS) main
