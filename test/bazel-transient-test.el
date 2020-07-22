;;; bazel-transient-test.el --- Tests for bazel-transient -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Code:

(require 'buttercup)
(require 'dash)
(require 'ht)

(describe
 "bazel-transient-bazel-do"
 (before-each
  (spy-on 'compile))
 (describe
  "when `bazel-transient-bazel-cmd' does not exist"
  (before-each
   (setq bazel-transient-bazel-cmd "complete nonsense"))
  (it "fails"
      (expect (bazel-transient-bazel-do 'test '("foo")) :to-throw)
      (expect 'compile :not :to-have-been-called)))
 (describe
  "when `bazel-transient-bazel-cmd' does exist"
  (before-each
   (spy-on 'executable-find :and-return-value "pwd"))
  (it "runs successfully"
      (bazel-transient-bazel-do 'bar '("foo"))
      (expect 'compile :to-have-been-called-with "pwd bar foo"))))

(describe
 "bazel-transient-cache-targets-maybe"
 (it "caches RESULTS under KIND if caching enabled"
     (let ((bazel-transient-enable-caching t))
       (spy-on 'bazel-transient-serialize-kind-target-cache :and-return-value nil)
       (let ((res (bazel-transient-cache-targets-maybe 'kind '("foo" "bar"))))
         (expect 'bazel-transient-serialize-kind-target-cache :to-have-been-called))))
 (it "returns RESULTS regardless of cache enabled"
     (-each '(nil t)
       (lambda (cache-enabled)
         (let ((bazel-transient-enable-caching cache-enabled))
           (expect (bazel-transient-cache-targets-maybe 'kind '("foo")) :to-equal '("foo")))))))

(describe
 "bazel-transient-invalidate-cache-maybe"

 (describe
  "when `bazel-transient-enable-caching' is nil"
  (before-each
   (setq bazel-transient-enable-caching nil)
   (setq bazel-transient-kind-target-cache
         (ht ('test '("foo" "bar")))))
  (it "does nothing"
      (spy-on 'bazel-transient-serialize-kind-target-cache)
      (let ((orig-cache-value bazel-transient-kind-target-cache))
        (bazel-transient-invalidate-cache-maybe)
        (expect bazel-transient-kind-target-cache :to-equal orig-cache-value))
      (expect 'bazel-transient-serialize-kind-target-cache :not :to-have-been-called)))

 (describe
  "when `bazel-transient-enable-caching' is non-nil"
  (before-each
   (setq bazel-transient-enable-caching t))
  (it "invalidates the in-memory cache")
  (it "deletes the serialized file cache")))

(describe
 "unserialize"
 (it "can round-trip serialize"
     (let* ((filename "/tmp/foo")
            (data (ht ('test '("foo" "bar")))))
       (bazel-transient-serialize data filename)
       (expect (bazel-transient-unserialize filename) :to-equal-ht data))))

(describe
 "bazel-transient-get-all-workspace-targets-of-kind"
 (before-each
  (setq bazel-transient-kind-target-cache (ht ('test '("foo"))))
  (spy-on 'bazel-transient-bazel-command-to-string-maybe :and-return-value "foo\nbar\nbaz"))

 (describe
  "when `bazel-transient-enable-caching' is non-nil"
  (before-each
   (setq bazel-transient-enable-caching t))

  (describe
   "when cache contains the kind"
   (it "returns the cached value"
       (expect (bazel-transient-get-all-workspace-targets-of-kind 'test)
               :to-equal
               '("foo")))
   (it "makes no call to Bazel"
       (expect 'bazel-transient-bazel-command-to-string-maybe :not :to-have-been-called)))

  (describe
   "when cache does not contain the kind"
   (it "returns the Bazel query result"
       (let ((res (bazel-transient-get-all-workspace-targets-of-kind 'other-kind)))
         (expect res :to-equal '("foo" "bar" "baz"))
         (expect 'bazel-transient-bazel-command-to-string-maybe :to-have-been-called)))))

 (describe
  "when `bazel-transient-enable-caching' is nil"
  (before-each
   (setq bazel-transient-enable-caching nil)
   (setq bazel-transient-kind-target-cache (ht ('test '("foo")))))

  (it "returns the Bazel query result"
      (let ((res (bazel-transient-get-all-workspace-targets-of-kind 'test)))
        (expect 'bazel-transient-bazel-command-to-string-maybe :to-have-been-called)
        (expect res :to-equal '("foo" "bar" "baz"))))))

(describe
 "bazel-transient-shell-command-to-string-maybe"
 (describe
  "when command fails"
  (it "returns nil"))
 (describe
  "when command succeeds"
  (it "returns the command output")))

;;; bazel-transient-test.el ends here
