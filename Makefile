VERSION=$$(grep "^;; Version: " org-trello.el | cut -f3 -d' ')
PACKAGE_FOLDER=org-trello-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar

test:
	cask exec emacs -Q -batch \
			-l ert \
			-l ./org-trello-tests.el \
			-f ert-run-tests-batch-and-exit

itest-clean:
	rm -f "#*org-trello-testing-buffer*#" "*org-trello-testing-buffer*" "*org-trello-testing-buffer*~"

itest: itest-clean
	cask exec ecukes --script --dbg

itest-f: itest-clean
	cask exec ecukes --script --dbg $(FEATURE)

itest-f-win: itest-clean
	cask exec ecukes --dbg $(FEATURE)

itest-win:
	cask exec ecukes features --dbg

tests:	test itest

pkg-el:
	cask package

clean:
	rm -rf *.tar $(PACKAGE_FOLDER)

prepare:
	mkdir -p $(PACKAGE_FOLDER)
	cp -r org-trello.el org-trello-pkg.el $(PACKAGE_FOLDER)

package: clean pkg-el prepare
	tar cvf $(ARCHIVE) $(PACKAGE_FOLDER)
	rm -rf $(PACKAGE_FOLDER)

info:
	cask info

install-package: package
	cask exec emacs --batch -l ./build.el -- org-trello-$(VERSION).tar

install-package-and-tests: install-package
	cask exec emacs -Q --batch -l ./launch-tests.el

tangle:
	time cask exec emacs -Q -batch \
	    		     -l org \
			     ./org-trello.org \
			     -e org-babel-tangle

ttest: tangle test
