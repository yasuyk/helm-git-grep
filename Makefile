EMACS ?= emacs
CASK ?= cask

test: test-compile unit-tests

unit-tests: elpa
	${CASK} exec ert-runner

elpa:
	mkdir -p elpa
	${CASK} install 2> elpa/install.log

clean-elpa:
	rm -rf elpa

clean-elc:
	rm -f *.elc test/*.elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}

test-compile: elpa
	$(CASK) exec $(EMACS) -batch -Q -L . -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" helm-git-grep.el

travis-ci: print-deps test

