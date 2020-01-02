;;; bazel-transient --- Transient command execution for Bazel

;;; Code:

(require 'dash)
(require 's)
(require 'transient)

(define-infix-command bazel-test-test-output ()
  :class 'transient-switches
  :description "Set test output"
  :key "-t"
  :argument-format "--test_output=%s"
  :argument-regexp "\\(--test_output=\\(summary\|errors\|all\|streamed\\)\\)"
  :choices '("summary" "errors" "all" "streamed"))

(define-infix-command bazel-test-test-filter ()
  :description "Test filter"
  :class 'transient-option
  :argument "--test_filter="
  :key "-f"
  :reader 'completing-read)

(define-infix-command bazel-test-test-summary ()
  :description "Set summary display"
  :class 'transient-switches
  :key "-s"
  :argument-format "--test_summary=%s"
  :choices '("short" "terse" "detailed" "none")
  :argument-regexp "\\(--test_summary=\\(short\|terse\|detailed\|none\\)\\)")

(defun bazel-test-target (target args)
  (interactive
   (list
    (read-from-minibuffer "Test target: ")
    (transient-args 'bazel-test)))
  (let ((cmd (s-join " " (-flatten `("bazel" "test" ,target ,args)))))
    (compile cmd)))

(define-transient-command bazel-test ()
  "Test a target."
  ["Variables"
   (bazel-test-test-output)
   (bazel-test-test-filter)
   (bazel-test-test-summary)]
  [[
    ("t" "target" bazel-test-target)
    ]])

(provide 'bazel-transient)
