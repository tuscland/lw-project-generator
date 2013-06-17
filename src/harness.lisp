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

;;; Implementation of project template variables.


(in-package "COM.WILDORA.PROJECT-GENERATOR")

(defparameter *template-variables-package-name*
  "COM.WILDORA.PROJECT-GENERATOR-VARIABLES")

(defparameter *template-variables-package*
  (or (find-package *template-variables-package-name*)
      (make-package *template-variables-package-name* :use nil))
  "The package in which template variables are defined.")

;; For the moment, make at least this function available in the
;; variables package.
(import '(string) *template-variables-package*)


(defparameter *template-variables* ())

(defun clear-template-variables ()
  (mapc #'makunbound *template-variables*)
  (setf *template-variables* ()))

(defmacro define-template-variable (name documentation)
  (let ((symbol (intern (string name) *template-variables-package*)))
    `(progn
       (defvar ,symbol nil
         ,documentation)
       (pushnew ',symbol *template-variables*))))

(editor:setup-indent 'define-template-variable 1)

(defmacro template-variable (name)
  "Accesses the value stored in a template variable, SETF-able."
  `(symbol-value
    ',(find-symbol (string name)
                   *template-variables-package*)))

(defun template-read-eval (stream)
  (eval
   (let ((*package* *template-variables-package*))
     (read stream))))

(defun |#.-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (template-read-eval stream))

(defun |#+-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (unless (template-read-eval stream)
    (read stream))
  (values))

(defun install-template-reader-macros ()
  (set-dispatch-macro-character #\# #\. #'|#.-reader|)
  (set-dispatch-macro-character #\# #\+ #'|#+-reader|))

(defmacro with-template-reader-macros (&body body)
  `(let ((*readtable* (copy-readtable nil)))
     (install-template-reader-macros)
     ,@body))
