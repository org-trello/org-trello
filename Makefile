test:
	emacs --no-init-file -batch \
		-l ert \
		-l ./orgtrello-tests.el \
		-f ert-run-tests-batch-and-exit
