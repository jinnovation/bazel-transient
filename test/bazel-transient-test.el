;;; bazel-transient-test.el --- Tests for bazel-transient

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

;;; bazel-transient-test.el ends here
