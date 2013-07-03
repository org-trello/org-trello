test:
	emacs --no-init-file -batch \
		-l ert \
		-l ./org-trello-tests.el \
		-f ert-run-tests-batch-and-exit
