;;; test-helper.el --- Helpers for bazel-transient-test.el

(require 'buttercup)
(require 'f)

(buttercup-define-matcher-for-binary-function :to-equal-ht ht-equal-p)

(add-to-list 'load-path (f-parent (f-parent (f-this-file))))

(require 'undercover)
(setq undercover-force-coverage t)

(undercover
 "*.el"
 (:report-file "coverage.json")
 (:send-report nil))

(require 'bazel-transient)

;;; test-helper.el ends here
