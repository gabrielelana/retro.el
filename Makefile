# Run with $ EMACS=~/opt/emacs-native/bin/emacs make foo

EMACS ?= emacs
CASK ?= cask

SOURCE = $(shell ls *.el | grep -v retro-pkg.el)

all: clean prepare test compile

prepare:
	${CASK} install

# `test' depends on `compile' so the suite always runs against freshly
# byte-compiled code.  Otherwise `require' loads a stale .elc (it is preferred
# over a newer .el unless `load-prefer-newer' is set) and tests would silently
# run against an old build.
test: compile
	${CASK} emacs --batch -L . -L test \
		-l test-helper \
		-l tile-test \
		-l canvas-test \
		-l sprite-test \
		-l background-test \
		-l collision-test \
		-f ert-run-tests-batch

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ${SOURCE}

# Native-compile the engine. The hot rendering/blit functions carry
# `(declare (speed 3))', so a native build is where the real performance
# lives (blit is ~2x faster native vs byte-compiled).
compile-native:
	${CASK} exec ${EMACS} -Q -batch \
		--eval "(unless (native-comp-available-p) (error \"This Emacs has no native compilation support\"))" \
		--eval "(dolist (f (split-string \"${SOURCE}\")) (native-compile f))"

clean:
	${CASK} clean-elc

.PHONY: prepare test compile compile-native clean
