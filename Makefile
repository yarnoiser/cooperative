# Mostly based off of the makefile for the chicken SDL2 project
# https://gitlab.com/chicken-sdl2/chicken-sdl2/blob/master/Makefile

.PHONY: clean

SRC=cooperative.scm cooperative.setup cooperative.meta

default: build

build: $(SRC)
	chicken-install -n

install: $(SRC)
	chicken-install

uninstall:
	chicken-uninstall cooperative

test: build tests/run.scm
	csi -I ./ -s tests/run.scm

clean:
	$(RM) *.so *.import.scm *.o *.c salmonella.log


