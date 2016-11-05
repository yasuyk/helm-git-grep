
EMACS ?= emacs
CASK ?= cask
SRC ?= helm-git-grep.el
FIXTURE_GIT_VERSION ?= v1.0.0
FIXTURE_GIT_WORK_DIR ?= test/fixture/git
TEST_CHECKDOC_EL ?=  test/test-checkdoc.el
TEST_PACKAGE_INSTALL_EL ?=  test/test-package-install.el
LOADPATH = -L .
ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

INIT_PACKAGE_EL="(progn (require 'cask) (cask-initialize \".\"))"

.PHONY : test
test: test-checkdoc package-lint test-package-install unit-tests

.PHONY : travis-ci
travis-ci: print-deps package-lint test-package-install unit-tests

.PHONY : unit-tests
# `clean-elc` task needs to remove byte-compiled files to collect coverage by undercover.el.
unit-tests: clean-elc elpa fixture
	@echo "-- Running unit-tests --"
	${CASK} exec ert-runner

.PHONY : clean-elpa
clean-elpa:
	rm -rf .cask

.PHONY : clean-elc
clean-elc:
	${CASK} clean-elc

.PHONY : clean
clean: clean-elpa clean-elc clean-fixture

.PHONY : print-deps
print-deps:
	${EMACS} --version
	@echo CASK=${CASK}

.PHONY : test-checkdoc
test-checkdoc: elpa
	@echo "-- test ckeckdoc --"
	$(CASK) exec $(EMACS) -batch -Q $(LOADPATH) -l $(TEST_CHECKDOC_EL) 2>&1 | tee /dev/tty | [ $$(wc -l) -gt 0 ] && exit 1 || exit 0


.PHONY : package-lint
package-lint: elpa
	@echo "-- package lint --"
	$(CASK) exec $(EMACS) -batch -Q --eval $(INIT_PACKAGE_EL) -l package-lint.el -f package-lint-batch-and-exit $(SRC)

.PHONY : test-package-install
test-package-install: elpa
	@echo "-- test install package --"
	$(CASK) exec $(EMACS) -batch -Q $(LOADPATH) -l $(TEST_PACKAGE_INSTALL_EL) 2>&1  | tee /dev/tty | grep Error > /dev/null && exit 1 || exit 0

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

.PHONY : check-coveralls-token
check-coveralls-token:
    ifdef COVERALLS_REPO_TOKEN
		@true
    else
		@echo COVERALLS_REPO_TOKEN is undefined
		@false
    endif

.PHONY : clean-coveralls-report
clean-coveralls-report: check-coveralls-token
	@( [ -f /tmp/undercover_coveralls_report ] && rm /tmp/undercover_coveralls_report ) || :

.PHONY : coveralls
coveralls: clean-coveralls-report unit-tests
