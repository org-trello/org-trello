VERSION=$$(grep "^;; Version: " org-trello.el | cut -f3 -d' ')
PACKAGE_FOLDER=org-trello-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
EMACS=emacs
LOG_TEST_FILE=./run-org-trello-tests.log

.PHONY: clean

pr:
	hub pull-request -b org-trello:master

deps:
	cask

build:
	cask build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	cask clean-elc

install:
	cask install

test: clean
	cask exec $(EMACS) --batch \
			-l ert \
			-l ./launch-tests.el \
			-f ert-run-tests-batch-and-exit \
	  2>&1 | tee $(LOG_TEST_FILE)

test-log:
	less $(LOG_TEST_FILE)

pkg-file:
	cask pkg-file

pkg-el: pkg-file
	cask package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	cask info

install-package-from-melpa:
	./install-package-from.sh melpa

install-file-with-deps-from-melpa: package
	./install-file-with-deps-from.sh melpa $(VERSION)

cleanup-data:
	rm -rvf ~/.emacs.d/elnode/public_html/org-trello/{1,2,3}/.scanning/* \
		~/.emacs.d/elnode/public_html/org-trello/{1,2,3}/* \
		~/.emacs.d/elnode/public_html/org-trello/*.lock

release:
	./release.sh $(VERSION)

install-cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
