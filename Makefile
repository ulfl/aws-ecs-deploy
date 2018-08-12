.PHONY: all release hoogle hoogle-server clean setup-tools

all:
	stack install --fast --ghc-options=-j --haddock-deps --local-bin-path bin

release:
	stack install --local-bin-path bin

hoogle:
	stack hoogle -- generate --local

hoogle-server:
	stack hoogle -- server --local --port=8080

clean:
	stack clean

setup-tools:
	stack build --copy-compiler-tool intero hindent hlint hoogle weeder
