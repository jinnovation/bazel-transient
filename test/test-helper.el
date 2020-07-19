;;; test-helper.el --- Helpers for bazel-transient-test.el

(require 'f)

(add-to-list 'load-path (f-parent (f-parent (f-this-file))))

(require 'undercover)
(setq undercover-force-coverage t)

(undercover
 "*.el"
 (:report-file "coverage")
 (:report-format 'text)
 (:send-report nil))

(require 'bazel-transient)

;;; test-helper.el ends here
