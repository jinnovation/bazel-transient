lint:
	cask emacs -Q --batch -l package-lint.el --eval "(setq package-lint-main-file \"bazel-transient.el\")" -f package-lint-batch-and-exit  *.el