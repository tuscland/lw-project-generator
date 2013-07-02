;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

;;; LispWorks Project Generator.
;;; Copyright (c) 2013, Camille Troillard. All rights reserved.

;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an "AS
;;; IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
;;; express or implied.  See the License for the specific language
;;; governing permissions and limitations under the License.

;;; Utility functions.


(in-package "COM.WILDORA.PROJECT-GENERATOR")

(defun pathname-as-relative (parent subpath
                             &optional (defaults *default-pathname-defaults*))
  "Given PARENT and a SUBPATH contained in it, return a relative path
leading from PARENT to SUBPATH.  In other words, (merge-pathnames
(pathname-as-relative parent subpath) parent) == subpath.

Both PARENT and SUBPATH might be either relative or absolute. If only
one is relative, an absolute DEFAULTS pathname must be specified to
allow anchoring it."
  (let* ((parent (system:directory-pathname parent))
         (defaults (and defaults (system:directory-pathname defaults)))
         (parent (if defaults (merge-pathnames parent defaults) parent))
         (subpath (if defaults (merge-pathnames subpath defaults) subpath)))
    (unless (eq (first (pathname-directory parent))
                (first (pathname-directory subpath)))
      (error "DEFAULTS must be specified when mixing relative and absolute input pathnames"))
    (labels ((nil-as-unspecific (value)
               (or value
                   :unspecific))
             (incompatible-pathnames (lhs rhs key)
               (let ((lhs (nil-as-unspecific (funcall key lhs)))
                     (rhs (nil-as-unspecific (funcall key rhs))))
                 (and (or lhs rhs)
                      (not (equal lhs rhs))))))
      (when (incompatible-pathnames parent subpath #'pathname-host)
        (error "Pathnames '~A' and '~A' are located on different ~
                hosts and cannot be expressed relative to each other"
               parent subpath))
      (when (incompatible-pathnames parent subpath #'pathname-device)
        (error "Pathnames '~A' and '~A' are located on different ~
                devices and cannot be expressed relative to each other"
               parent subpath)))
    (loop :for (pdir . prest) :on (cdr (pathname-directory parent))
          :for (subdir . subrest) :on (cdr (pathname-directory subpath))
          :for up := prest ;; cannot use :previous, since that won't run in the 1-past-last iteration
          :for down := subrest
          :while (equal pdir subdir)
          :finally (return (make-pathname :directory `(:relative ,@(mapcar (constantly :up) up) ,@down)
                                          :defaults subpath)))))

(defun last-dotted-component (string)
  (car (last (split-sequence '(#\.) string))))

(defun string-suffix-p (string suffix)
  (let ((start-index (- (length string)
                        (length suffix))))
    (when (>= start-index 0)
      (string= string suffix :start1 start-index))))

(defun read-string-up-to (endchar &optional (stream *standard-input*))
  (loop :with result := nil
        :for char := (read-char stream)
        :while (not (eql char endchar))
        :do (push char result)
        :finally (return (coerce (nreverse result)
                                 'string))))

(defun keyword-symbol (string)
  (intern (string-upcase string)
          (find-package "KEYWORD")))
