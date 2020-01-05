;;; bazel-transient --- Transient command execution for Bazel -*- lexical-binding: t -*-

;; Author: Jonathan Jin <jjin082693@gmail.com>
;; Maintainer: Jonathan Jin <jjin082693@gmail.com>

;;; Commentary:

;; Provides a transient command for testing of Bazel-based projects.

;;; Code:

(require 'transient)
(require 's)
(require 'dash)

(defun bazel-transient/bazel-do (cmd args &optional target do-fn)
  "Execute a Bazel command CMD with ARGS and optional TARGET.

TARGET is provided primarily for semantic convenience.  Passing
the corresponding value in as the last value of the ARGS list
results in equivalent behavior.

DO-FN is used to change exactly how the overall Bazel command is
carried out.  By default, this is `compile', but can for instance
be changed to `shell-command-to-string' if you intend to consume
the command's results."
  (let ((total-cmd (s-join " " (-flatten `("bazel" ,(symbol-name cmd) ,args ,target)))))
    (funcall (or do-fn 'compile) total-cmd)))

(define-infix-command bazel-test-test-output ()
  :class 'transient-switches
  :description "Test output style"
  :key "-o"
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
  :description "Test summary style"
  :class 'transient-switches
  :key "-s"
  :argument-format "--test_summary=%s"
  :choices '("short" "terse" "detailed" "none")
  :argument-regexp "\\(--test_summary=\\(short\|terse\|detailed\|none\\)\\)")

(define-infix-command bazel-test-test-timeout ()
  :description "Timeout"
  :class 'transient-option
  :argument "--test_timeout="
  :key "-t"
  :reader 'transient-read-number-N+)

(defun bazel-transient/get-all-workspace-targets-of-kind (kind)
  (let ((args `("--noshow_progress"
                ,(s-lex-format "\"kind(${kind}, //... - //third_party/...)\""))))
    (s-lines (bazel-transient/bazel-do 'query args nil 'shell-command-to-string))))

;; FIXME: Documentation
(defun bazel-test-target (target args)
  (interactive
   (list
    (ivy-read
     "Test target: "
     (bazel-transient/get-all-workspace-targets-of-kind 'test))
    (transient-args 'bazel-test)))
  (bazel-transient/bazel-do 'test args target))

(defun bazel-transient/get-buffer-pkg-label (&optional buffer)
  ;; FIXME: Interactively select buffer from those available
  (let* ((b (or buffer (current-buffer)))
         (buffer-relpath (s-concat "./"
                                   (url-file-nondirectory (buffer-file-name b))))
         (buffer-label (bazel-transient/bazel-do
                        'query
                        '("--noshow_progress" "--output label")
                        buffer-relpath
                        'shell-command-to-string)))
    (car (s-split ":" buffer-label))))

(defun bazel-transient/test-all-in-current-package (args)
  "Execute all test targets in the current package.

ARGS is forwarded to Bazel as test command flags."
  (interactive (list (transient-args 'bazel-test)))
  (bazel-transient/bazel-do 'test args (s-append ":all" (bazel-transient/get-buffer-pkg-label))))

(define-transient-command bazel-test ()
  "Test a target."
  ["Variables"
   (bazel-test-test-output)
   (bazel-test-test-filter)
   (bazel-test-test-summary)
   (bazel-test-test-timeout)]
  [[
    "Test"
    ("t" "target" bazel-test-target)
    ("c" "all in current pkg" bazel-transient/test-all-in-current-package)
    ]])

(provide 'bazel-transient)
;;; bazel-transient.el ends here
