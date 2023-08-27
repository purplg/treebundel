CASK_DIR := `cask package-directory`

cask:
	cask install
	@touch {{ CASK_DIR }}

test-cmd := "cask emacs -Q --batch -L ./ -l tests/treebundel-tests.el"

_test-all:
	{{ test-cmd }} -f ert-run-tests-batch-and-exit

_test-select selector:
	{{ test-cmd }} --eval='(ert-run-tests-batch-and-exit "{{ selector }}")'

test selector="":
	[ -z "{{ selector }}" ] && \
	  just _test-all || \
	  just _test-select {{ selector }}
