PACKAGE = org-trello
VERSION = $$(grep "^;; Version: " $(PACKAGE).el | cut -f3 -d' ')
ARCHIVE = $(PACKAGE)-$(VERSION).tar
EMACS ?= emacs
CASK ?= cask
LANG=en_US.UTF-8

.PHONY: clean

pr:
	hub pull-request -b org-trello:master

deps:
	${CASK}

build:
	${CASK} build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	${CASK} clean-elc

install:
	${CASK} install

test: clean
	${CASK} exec ert-runner

pkg-file:
	${CASK} pkg-file

pkg-el: pkg-file
	${CASK} package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	${CASK} info

release:
	./release.sh $(VERSION) $(PACKAGE)

version:
	@echo "application $(PACKAGE): $(VERSION)\npackage: $(ARCHIVE)"
