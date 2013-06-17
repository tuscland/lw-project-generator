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

;;; Definition of project template variables.


(in-package "COM.WILDORA.PROJECT-GENERATOR")

(clear-template-variables)

(define-template-variable product-name
  "[string] User defined name for the executable product.")

(define-template-variable system-name
  "[string] User defined name given to DEFSYSTEM.")

(define-template-variable package-name
  "[string] Name of the package for this project, by default is the system-name.")

(define-template-variable package-nickname
  "[string] Nickname of the package, by default is the last dotted component of the system-name.")

(define-template-variable main-function-symbol
  "[symbol] Symbol representing the main entry point in the executable, MAIN by default.")

(define-template-variable main-interface-class-symbol
  "[symbol] Symbol representing the main interface class, by default is the concatenation of the system name and INTERFACE.")

(define-template-variable osx-bundle-identifier
  "[string] Reverse-DNS style name that appears in the Info.plist file, by default is the system-name.")

(define-template-variable lispworks-executable-path
  "[string] Path to the LispWorks executable used to build the delivered product, by default use the current image path.")

(defparameter *project-package* nil)
(defparameter *main-function-default-name* "MAIN")

(defun set-project-package (system-name)
  (setf (template-variable package-name) system-name
        (template-variable package-nickname) (when (find #\. system-name)
                                               (last-dotted-component system-name))
        *project-package* (or (find-package (template-variable package-name))
                              (make-package (template-variable package-name)))))

(defun project-symbol (name)
  (intern name *project-package*))

(defun set-main-symbols (system-name)
  (let ((short-system-name (last-dotted-component system-name)))
    (setf (template-variable main-function-symbol)
          (project-symbol *main-function-default-name*)
          
          (template-variable main-interface-class-symbol)
          (project-symbol (format nil "~A-INTERFACE" short-system-name)))
    ;; TODO: Since we export the symbol here, package.lisp-template
    ;; should in fact list the exported symbols.
    (export (template-variable main-function-symbol) *project-package*)
    (export (template-variable main-interface-class-symbol) *project-package*)))

(defun setup-template-variables (system-name product-name)
  (assert (and product-name
               system-name))
  (set-project-package system-name)
  (set-main-symbols system-name)
  (setf (template-variable product-name) product-name
        (template-variable system-name) system-name
        (template-variable osx-bundle-identifier) (string-downcase system-name)
        (template-variable lispworks-executable-path) (lisp-image-name)))

(defmacro with-template-variables ((system-name product-name) &body body)
  `(let ,*template-variables*
     (setup-template-variables ,system-name ,product-name)
     ,@body))
