.PHONY: test elpa build

build:
	cask build

lint:
	cask emacs -Q --batch -l package-lint.el --eval "(setq package-lint-main-file \"bazel-transient.el\")" -f package-lint-batch-and-exit  *.el

test:
	cask exec buttercup -L .
