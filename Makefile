VERSION=$$(date "+%Y%m%d.%H%M")
FILE=orgtrello-$(VERSION)

test:
	emacs --no-init-file -batch \
		-l ert \
		-l ./orgtrello-tests.el \
		-f ert-run-tests-batch-and-exit

tar:
	mkdir -p $(FILE)
	sed "s/###VERSION###/\"$(VERSION)\"/" orgtrello-pkg-template.el > $(FILE)/orgtrello-pkg.el
	cp -r utils/ orgtrello.el orgtrello-pkg.el README.md COPYING $(FILE)
	tar cvf $(FILE).tar $(FILE)

clean:
	rm -f *.tar
