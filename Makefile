test:
	# carton setups the elpa env for me
	carton exec emacs --no-init-file -batch \
			-l ert \
			-l ./org-trello-tests.el \
			-f ert-run-tests-batch-and-exit

package:
	carton package

info:
	carton info

# test-package:
# 	bash ~/bin/emacs/emacs-install-clean.el ~/repo/perso/melpa/packages/org-trello-20130705.1722.tar testing-clean-install/

# build-package:
# 	emacs --batch -l ./build.el -- org-trello.el
