# Run with $ EMACS=~/opt/emacs-native/bin/emacs make foo

EMACS ?= emacs
CASK ?= cask

all: clean prepare test compile test clean

prepare:
	${CASK} install

test:
	${CASK} exec ert-runner --load *.el test/canvas-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile *.el

clean:
	${CASK} clean-elc

.PHONY: prepare test compile clean
