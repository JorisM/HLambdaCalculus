.SILENT:clean

all: compile test clean

compile: Parsing.hs
	ghc Parsing.hs

test: tests.hs Parsing.hs
	runghc tests.hs

clean:
	if [ -a tests.o ]; then rm tests.o; fi
	if [ -a Parsing.o ]; then rm Parsing.o; fi
	if [ -a Parsing.hi ]; then rm Parsing.hi; fi
	if [ -a tests.hi ]; then rm tests.hi; fi
