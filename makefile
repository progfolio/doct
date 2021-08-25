.POSIX:
#To test with a different Emacs version run:
#$ export EMACS=<desired Emacs executable>
#$ make -e
EMACS = emacs

#Point to path of your local emacs-buttercup install
BUTTERCUP = ../emacs-buttercup/

.PHONY: all
all: clean compile check

compile: doct.elc
check: doct.elc
	$(EMACS) -Q --batch -L . -L $(BUTTERCUP) -l buttercup -f buttercup-run-discover
clean:
	rm -f doct.elc
.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
