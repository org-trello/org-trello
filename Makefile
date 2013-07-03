VERSION=$$(date "+%Y%m%d.%H%M")
FILE=org-trello-$(VERSION)

test:
	emacs --no-init-file -batch \
		-l ert \
		-l ./org-trello-tests.el \
		-f ert-run-tests-batch-and-exit

tar:
	mkdir -p $(FILE)
	sed "s/###VERSION###/\"$(VERSION)\"/" org-trello-pkg-template.el > $(FILE)/org-trello-pkg.el
	cp -r utils/ org-trello.el README.md COPYING $(FILE)
	tar cvf $(FILE).tar $(FILE)

clean:
	rm -f *.tar
