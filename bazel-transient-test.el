(defcustom bazel-transient-enable-caching
  nil
  "Whether or not to enable caching for Bazel operations."
  :group 'bazel-transient
  :type 'bool)

(defcustom bazel-transient-cache-file
  (expand-file-name "bazel-transient-targets.cache" user-emacs-directory)
  "The name of the Bazel-Transient test target cache file."
  :group 'bazel-transient
  :type 'string)

(defvar bazel-transient-kind-target-cache
  (make-hash-table :test 'equal)
  "A hashmap used to cache project targets by kind.")

;; Credit to bbatsov's projectile-serialize
(defun bazel-transient-serialize (data filename)
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-legth) (prin1-to-string data))))))

(defun bazel-transient-serialize-kind-target-cache ()
  "Serializes the test cache to the hard drive."
  (message "TODO: bazel-transient-serialize-kind-target-cache"))

(provide 'bazel-transient-test)
