;;; bazel-transient.el --- Transient command dispatch for Bazel projects -*- lexical-binding: t -*-

;; Copyright © 2020 Jonathan Jin <me@jonathanj.in>

;; Author: Jonathan Jin <me@jonathanj.in>
;; URL: https://github.com/jinnovation/bazel-transient
;; Keywords: project, convenience, build
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (pkg-info "0.6") (transient "0.2.0") (s "1.12.0") (dash "2.16.0"))

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
  (make-hash-table :test 'equal)
  "A hashmap used to cache project targets by kind.")

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

(defun bazel-transient-bazel-do (cmd args &optional target do-fn)
  "Execute a Bazel command CMD with ARGS and optional TARGET.

TARGET is provided primarily for semantic convenience.  Passing
the corresponding value in as the last value of the ARGS list
results in equivalent behavior.

DO-FN is used to change exactly how the overall Bazel command is
carried out.  By default, this is `compile', but can for instance
be changed to `shell-command-to-string' if you intend to consume
the command's results."
  ;; FIXME: The | cat is a horrid hack to get around dazel
  ;; (https://github.com/nadirizr/dazel) detecting sub-processes calling out to
  ;; dazel from within Emacs as running inside a tty, leading to "not a tty"
  ;; errors. This might only be an issue w/ NVIDIA's internal Dazel, but who
  ;; knows.
  (let ((total-cmd (s-join " " (-flatten `(,bazel-transient-bazel-cmd ,(symbol-name cmd) ,args
                                                      ,target "| cat")))))
    (message total-cmd)
    (funcall (or do-fn 'compile) total-cmd)))

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
  (if-let ((cached-targets (gethash kind bazel-transient-kind-target-cache)))
      cached-targets
    (let* ((args `("--noshow_progress"
                   ,(s-lex-format "\"kind(${kind}, //...)\"")))
           (output (bazel-transient-bazel-do 'query args nil
                                             'shell-command-to-string))
           (results (s-lines output)))
      (bazel-transient-cache-targets-maybe kind results))))

(defun bazel-transient-cache-targets-maybe (kind results)
  "Conditionally cache RESULTS under the kind KIND.

If `bazel-test-cache-test-results' is nil, simply return
RESULTS.  Otherwise, cache and return RESULTS."
  (if (not bazel-transient-enable-caching)
      results
    ;; FIXME: Note that this won't work across multiple projects. Need a way to
    ;; define a project ROOT, e.g. optionally using Projectile.
    (puthash kind results bazel-transient-kind-target-cache)
    (bazel-transient-serialize-kind-target-cache)))

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
  (let* ((b (or buffer (current-buffer)))
         (buffer-relpath (s-concat "./"
                                   (url-file-nondirectory (buffer-file-name b))))
         (buffer-label (bazel-transient-bazel-do
                        'query
                        ;; FIXME: need the following as defaults
                        ;; maybe consider creating a bazel profile
                        ;; --noshow_loading_progress
                        ;; --nohome_rc by default
                        '("--noshow_progress" "--output label")
                        buffer-relpath
                        'shell-command-to-string)))
    (car (s-split ":" buffer-label))))

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

(define-minor-mode bazel-transient-mode
  "Minor mode to enable transient command dispatch for Bazel projects."
  :group 'bazel-transient
  (unless bazel-transient-kind-target-cache
    (setq bazel-transient-kind-target-cache
          (or (bazel-transient-unserialize bazel-transient-cache-file)
              (make-hash-table :test 'equal)))))

(provide 'bazel-transient)

;;; bazel-transient.el ends here
