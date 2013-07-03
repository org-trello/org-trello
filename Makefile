VERSION=$$(date "+%Y%m%d.%H%M")

test:
	emacs --no-init-file -batch \
		-l ert \
		-l ./orgtrello-tests.el \
		-f ert-run-tests-batch-and-exit

tar:
	sed "s/###VERSION###/\"$(VERSION)\"/" orgtrello-pkg-template.el > orgtrello-pkg.el
	tar acvf orgtrello-$(VERSION).tar.gz utils/ orgtrello.el orgtrello-pkg.el README.md COPYING

clean:
	rm -f *.tar.gz
