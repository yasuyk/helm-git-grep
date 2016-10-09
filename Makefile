
EMACS ?= emacs
CASK ?= cask
SRC ?= helm-git-grep.el
FIXTURE_GIT_VERSION ?= v1.0.0
FIXTURE_GIT_WORK_DIR ?= test/fixture/git
LOADPATH = -L .

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

.PHONY : test
test: test-compile unit-tests

.PHONY : unit-tests
# `clean-elc` task needs to remove byte-compiled files to collect coverage by undercover.el.
unit-tests: clean-elc elpa
	@echo "-- Running unit-tests --"
	${CASK} exec ert-runner

.PHONY : clean-elpa
clean-elpa:
	rm -rf .cask

.PHONY : clean-elc
clean-elc:
	cask clean-elc

.PHONY : clean
clean: clean-elpa clean-elc clean-fixture

.PHONY : print-deps
print-deps:
	${EMACS} --version
	@echo CASK=${CASK}

.PHONY : test-compile
test-compile: elpa
	@echo "-- test compile --"
	$(CASK) exec $(EMACS) -batch -Q $(LOADPATH) -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $(SRC)

.PHONY : travis-ci
travis-ci: print-deps test

.PHONY : elpa
elpa: $(ELPA_DIR)

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

.PHONY : fixture
fixture: $(FIXTURE_GIT_WORK_DIR)

$(FIXTURE_GIT_WORK_DIR):
	@echo "--  clone git repository for test fixture --"
	git clone --depth 1 -b $(FIXTURE_GIT_VERSION) https://github.com/git/git.git $@

.PHONY : clean-fixture
clean-fixture:
	rm -rf $(FIXTURE_GIT_WORK_DIR)

