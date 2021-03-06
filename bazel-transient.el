;;; bazel-transient.el --- Transient command dispatch for Bazel projects -*- lexical-binding: t -*-

;; Copyright © 2020 Jonathan Jin <me@jonathanj.in>

;; Author: Jonathan Jin <me@jonathanj.in>
;; URL: https://github.com/jinnovation/bazel-transient
;; Keywords: project, convenience, build
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (pkg-info "0.6") (ht "2.2") (transient "0.2.0") (s "1.12.0") (dash "2.16.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Provides a transient command for testing of Bazel-based projects.
;;
;;; Code:

(require 'dash)
(require 'ht)
(require 'transient)
(require 's)
(require 'subr-x)

(defcustom bazel-transient-bazel-cmd
  "bazel"
  "Command to run Bazel with."
  :group 'bazel-transient
  :type 'string)

(defcustom bazel-transient-completion-system
  'default
  "Completion system to use."
  :group 'bazel-transient
  :options '(default
              ivy)
  :type 'symbol)

(defcustom bazel-transient-enable-caching
  nil
  "Whether or not to enable caching for Bazel operations."
  :group 'bazel-transient
  :type 'bool)

(defcustom bazel-transient-cache-file
  (expand-file-name "bazel-transient-targets.cache" user-emacs-directory)
  "The name of the Bazel-Transient test target cache file."
  :group 'bazel-transient
  :type 'file)

(defvar bazel-transient-kind-target-cache
  (ht)
  "A hashmap used to cache project targets by kind.")

(defun bazel-transient-kind-target-cache-lookup (kind &optional workspace)
  "Look up the cached targets of KIND stored for WORKSPACE.

Returns nil if either WORKSPACE is not present, or KIND is not cached within the
  WORKSPACE."
  (when-let ((workspace (or workspace (bazel-transient-workspace-root)))
             (workspace-cache (ht-get bazel-transient-kind-target-cache
                                      workspace)))
    (ht-get workspace-cache kind)))

(defconst bazel-transient-workspace-file-name "WORKSPACE"
  "The file name used to identify the root of a Bazel project.")

;; FIXME: Copied wholesale from magit-utils.el. Upstream a PR to
;; transient that decouples this from magit.
(defmacro bazel-transient-read-char-case (prompt verbose &rest clauses)
  "TODO: Documentation.  PROMPT VERBOSE CLAUSES."
  (declare (indent 2)
           (debug (form form &rest (characterp form body))))
  `(prog1 (pcase (read-char-choice
                  (concat ,prompt
                          ,(concat (mapconcat 'cadr clauses ", ")
                                   (and verbose ", or [C-g] to abort") " "))
                  ',(mapcar 'car clauses))
            ,@(--map `(,(car it) ,@(cddr it)) clauses))
     (message "")))

(defun bazel-transient-shell-command-to-string-maybe (&rest args)
  "Return output of command comprised of ARGS on successful execution."
  (let* ((exit-code -1)
         (cmd (message (s-join " " (-flatten args))))
         (output (s-trim-right (with-output-to-string
                                 (with-current-buffer standard-output
                                   (setq exit-code (call-process-shell-command cmd nil t nil)))))))
    (when (zerop exit-code) output)))


(defun bazel-transient-bazel-command-to-string-maybe (cmd args &optional target)
  "Return the result of Bazel command CMD with ARGS and TARGET if successful.

TARGET is provided primarily for semantic convenience.  Passing
the corresponding value in as the last value of the ARGS list
results in equivalent behavior.

If `bazel-transient-bazel-cmd' is not an actual executable, error
out."
  (bazel-transient-shell-command-to-string-maybe
   bazel-transient-bazel-cmd
   (symbol-name cmd)
   args
   target
   ;; FIXME: Really need to have a package-internal .bazelrc to use across all
   ;; cmd invocations to avoid users' configs getting in the way like this
   "--color=no"
   "--noshow_progress"
   "--show_timestamps=no"
   ;; FIXME: The | cat is a horrid hack to get around dazel
   ;; (https://github.com/nadirizr/dazel) detecting sub-processes calling out to
   ;; dazel from within Emacs as running inside a tty, leading to "not a tty"
   ;; errors. This might only be an issue w/ NVIDIA's internal Dazel, but who
   ;; knows.
   "| cat"))

(defun bazel-transient-bazel-do (cmd args &optional target)
  "Execute a Bazel command CMD with ARGS and optional TARGET.

TARGET is provided primarily for semantic convenience.  Passing
the corresponding value in as the last value of the ARGS list
results in equivalent behavior.

If `bazel-transient-bazel-cmd' is not an actual executable, error
out."
  (if-let* ((bazel-cmd
            ;; FIXME: Needs to handle TRAMP. Namely, bazel-cmd that exists
            ;; remotely, that we intend to run remotely, will get checked
            ;; against local PATH. This is no bueno.
            ;;
            ;;Take inspiration from projectile-file-exists-p
             bazel-transient-bazel-cmd)

             ;; (or (executable-find bazel-transient-bazel-cmd) (f-exists-p
             ;;               (f-full bazel-transient-bazel-cmd))))
            (total-cmd (s-join " "
                               (-flatten `(,bazel-cmd
                                           ,(symbol-name cmd)
                                           ,args
                                           ,target)))))
      (compile total-cmd)
    (error "`%s' not found" bazel-transient-bazel-cmd)))

(transient-define-infix bazel-test-test-output ()
  :class 'transient-option
  :description "Test output style"
  :key "-o"
  :argument "--test_output="
  :reader (lambda (&rest _ignore)
            (bazel-transient-read-char-case nil t
              (?u "s[u]mmary" "summary")
              (?e "[e]rrors" "errors")
              (?a "[a]ll" "all")
              (?t "s[t]reamed" "streamed"))))

(transient-define-infix bazel-test-test-filter ()
  :description "Test filter"
  :class 'transient-option
  :argument "--test_filter="
  :key "-f"
  :reader 'completing-read)

(transient-define-infix bazel-test-test-summary ()
  :description "Test summary style"
  :class 'transient-option
  :key "-s"
  :argument "--test_summary="
  :reader (lambda (&rest _ignore)
            (bazel-transient-read-char-case nil t
              (?s "[s]hort" "short")
              (?t "[t]erse" "terse")
              (?d "[d]etailed" "detailed")
              (?n "[n]one" "none"))))

(transient-define-infix bazel-test-cache-test-results ()
  :description "Cache test results"
  :class 'transient-option
  :argument "--cache_test_results="
  :key "-c"
  :reader (lambda (&rest _ignore)
            (bazel-transient-read-char-case nil t
              (?y "[y]es" "yes")
              (?n "[n]o" "no")
              (?a "[a]uto" "auto"))))

(transient-define-infix bazel-test-test-timeout ()
  :description "Timeout"
  :class 'transient-option
  :argument "--test_timeout="
  :key "-t"
  :reader 'transient-read-number-N+)

(defun bazel-transient-get-all-workspace-targets-of-kind (kind)
  "Get all targets in the current workspace of KIND."
  (if-let ((caching-p bazel-transient-enable-caching)
           (cached-targets (gethash kind bazel-transient-kind-target-cache)))
      cached-targets
    (if-let* ((output (bazel-transient-bazel-command-to-string-maybe
                       'query
                       (s-lex-format "\"kind(${kind}, //...)\""))))
        (bazel-transient-cache-targets-maybe kind (s-lines output))
      (error "Get workspace targets failed"))))

(defun bazel-transient-cache-targets-maybe (kind results)
  "Conditionally cache RESULTS under the kind KIND.

If `bazel-test-cache-test-results' is nil, simply return
RESULTS.  Otherwise, cache and return RESULTS."
  (if (not bazel-transient-enable-caching)
      results
    ;; FIXME: Note that this won't work across multiple projects. Need a way to
    ;; define a project ROOT, e.g. optionally using Projectile.
    (puthash kind results bazel-transient-kind-target-cache)
    (bazel-transient-serialize-kind-target-cache)
    results))

(defun bazel-transient-invalidate-cache-maybe ()
  "Invalidate the cache if `bazel-transient-enable-caching' is non-nil."
  (interactive)
  (when bazel-transient-enable-caching
    (setq bazel-transient-kind-target-cache (ht))
    (if bazel-transient-enable-caching (bazel-transient-serialize-kind-target-cache))))

(defun bazel-transient-completing-read (prompt choices)
  "Present PROMPT with CHOICES based on `bazel-transient-completion-system'."
  (cond
   ((eq bazel-transient-completion-system 'default)
    (completing-read prompt choices))
   ((eq bazel-transient-completion-system 'ivy)
    (if (fboundp 'ivy-read)
        (ivy-read prompt choices)
      (user-error "Ivy selected, but not installed.  Please install")))
   (t (funcall bazel-transient-completion-system prompt choices))))

;; FIXME: Documentation
(defun bazel-test-target (target args)
  "Test the argument TARGET using `bazel-transient-bazel-cmd'.  ARGS are forwarded."
  (interactive
   (list
    (bazel-transient-completing-read
     "Test target: "
     (bazel-transient-get-all-workspace-targets-of-kind 'test))
    (transient-args 'bazel-test)))
  (bazel-transient-bazel-do 'test args target))

(defun bazel-transient-get-buffer-pkg-label (&optional buffer)
  "Gets the label of the package that the file BUFFER is visiting belongs to."
  ;; FIXME: Interactively select buffer from those available
  (if-let* ((b (or buffer (current-buffer)))
            (buffer-relpath (s-concat "./"
                                      (url-file-nondirectory (buffer-file-name b))))
            (buffer-label (bazel-transient-bazel-command-to-string-maybe
                           'query
                           '("--output label")
                           buffer-relpath)))
      (car (s-split ":" buffer-label))
    (error "Failed to get buffer's package label")))

(defun bazel-transient-test-all-in-current-package (args)
  "Execute all test targets in the current package.

ARGS is forwarded to Bazel as test command flags."
  (interactive (list (transient-args 'bazel-test)))
  (bazel-transient-bazel-do 'test args (s-append ":all" (bazel-transient-get-buffer-pkg-label))))

(transient-define-prefix bazel-test ()
  "Test a target."
  ["Variables"
   (bazel-test-cache-test-results)
   (bazel-test-test-output)
   (bazel-test-test-filter)
   (bazel-test-test-summary)
   (bazel-test-test-timeout)]
  [[
    "Test"
    ("t" "target" bazel-test-target)
    ("c" "all in current pkg" bazel-transient-test-all-in-current-package)
    ]])

;; Credit to bbatsov's projectile-serialize
(defun bazel-transient-serialize (data filename)
  "Serialize DATA to FILENAME."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

;; Credit to bbatsov's projectile-unserialize
(defun bazel-transient-unserialize (filename)
  "Read data serialized by `bazel-transient-serialize' in FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))

(defun bazel-transient-serialize-kind-target-cache ()
  "Serializes the test cache to the hard drive."
  (bazel-transient-serialize bazel-transient-kind-target-cache bazel-transient-cache-file))

(defun bazel-transient-workspace-root (&optional filename)
  "Return the root of the workspace FILENAME belongs to.

If FILENAME is not provided, use the file of the current buffer."
  ;; TODO: Cache this value
  (f-traverse-upwards
   (lambda (path) (let ((workspace-file (f-join path bazel-transient-workspace-file-name)))
                    (and (file-exists-p workspace-file)
                              (not (f-directory-p workspace-file)))))
   (or filename (buffer-file-name (current-buffer)))))

(define-minor-mode bazel-transient-mode
  "Minor mode to enable transient command dispatch for Bazel projects."
  :group 'bazel-transient
  (unless bazel-transient-kind-target-cache
    (setq bazel-transient-kind-target-cache
          (or (bazel-transient-unserialize bazel-transient-cache-file)
              (ht)))))

(provide 'bazel-transient)

;;; bazel-transient.el ends here

