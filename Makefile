# Mostly based off of the makefile for the chicken SDL2 project
# https://gitlab.com/chicken-sdl2/chicken-sdl2/blob/master/Makefile

.PHONY: clean

default: build

build: cooperative.import.scm cooperative.setup
	chicken-install -n

install: cooperative.import.scm cooperative.setup
	chicken-install

uninstall:
	chicken-uninstall cooperative

test: tests/run.scm
	csi -I ./ -s tests/run.scm

clean:
	$(RM) *.so *.import.scm salmonella.log

