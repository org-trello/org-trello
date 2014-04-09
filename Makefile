VERSION=$$(grep "^;; Version: " src/header.el | cut -f3 -d' ')
PACKAGE_FOLDER=org-trello-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
USER=ardumont
EMACS=emacs

.PHONY: clean

pr:
	hub pull-request -b org-trello:master

clean:
	rm -rf *.tar $(PACKAGE_FOLDER) dist/

install:
	cask install

test: clean
	cask exec $(EMACS) --batch \
			-l ert \
			-l ./launch-tests.el \
			-f ert-run-tests-batch-and-exit

pkg-el:
	cask package

generate:
	cask exec $(EMACS) -Q --batch -l ./build-package.el

package: clean generate pkg-el


info:
	cask info

install-package-from-marmalade:
	./install-package-from.sh marmalade

install-package-from-melpa:
	./install-package-from.sh melpa

install-file-with-deps-from-marmalade: package
	./install-file-with-deps-from.sh marmalade $(VERSION)

install-file-with-deps-from-melpa: package
	./install-file-with-deps-from.sh melpa $(VERSION)

cleanup-data:
	rm -rvf ~/.emacs.d/elnode/public_html/org-trello/{1,2,3}/.scanning/* \
		~/.emacs.d/elnode/public_html/org-trello/{1,2,3}/* \
		~/.emacs.d/elnode/public_html/org-trello/*.lock

release:
	./release.sh $(VERSION) $(USER)

install-cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
