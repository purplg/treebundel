test:
	cask emacs -Q --batch -L ./ -l treebund-tests.el --eval="(ert-run-tests-batch-and-exit)"
