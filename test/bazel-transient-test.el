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

;;; bazel-transient-test.el ends here
