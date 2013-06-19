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

;;; High-level template generation.


(in-package "COM.WILDORA.PROJECT-GENERATOR")

(defvar *debug-log-p* nil
  "When true, some additional information is logged to *DEBUG-IO*.")
(defvar *dry-run-p* nil
  "When true, no actual processing occurs (e.g. no file is written to disk).")

(defparameter *git* "/usr/bin/git")
(defparameter *dot-git-ignore*
  ".DS_Store
*~
*.xfasl
*.64xfasl
*.ofasl
*.64ofasl
*.ufasl
build
")

(defparameter *default-project-template* "capi-application")
(defparameter *project-templates-directory*
  (merge-pathnames #P"../project-templates/"
                   (pathname-location *load-truename*)))

(defun debug-log (format &rest args)
  (when *debug-log-p*
    (apply #'format *debug-io*
           format args)
    (terpri *debug-io*)))

(defmacro with-safe-output ((stream pathname) &body body)
  "Macro used for every output stream based operations, ensuring that
we do not overwrite an existing file by error."
  `(with-open-file (,stream ,pathname
                            :direction :output
                            :if-exists :error)
     ,@body))

(defun template-p (path)
  (let ((type (pathname-type path)))
    (unless (eq type :unspecific)
      (string-suffix-p (pathname-type path)
                       "-template"))))

(defun do-tags (fn string)
  (let ((start (search "#{" string)))
    (if start
        (let ((end (search "}" string :start2 start)))
          (unless end
            (error "Unmatched closing brace."))
          (funcall fn
                   (subseq string 0 start)
                   (subseq string (+ start 2) end)
                   (subseq string (1+ end))))
      (funcall fn string nil nil))))

(defun do-substitutions (string)
  "In STRING, substitute all pieces of code that are between #{ and
} with the result of their evaluation."
  (do-tags (lambda (string tag rest)
             (if tag
                 (string-append
                  string
                  (with-input-from-string (stream tag)
                    (template-read-eval stream))
                  (do-substitutions rest))
               string))
           string))

(defun lisp-template-p (path)
  (and (template-p path)
       (string-equal (pathname-type path)
                     "lisp-template")))

(defmacro with-template-io-syntax (&body body)
  `(with-standard-io-syntax
     (with-template-reader-macros
       (let ((*print-case* :downcase)
             (*print-pretty* t))
         ,@body))))

(defun find-package-in-form (form)
  (and (eq (first form) 'in-package)
       (find-package (second form))))

(defun read-lisp-template (pathname)
  (with-open-file (stream pathname
                          :direction :input)
    (with-template-io-syntax
      (loop :for form := (read stream nil)
            :while form
            :do (when-let (package (find-package-in-form form))
                  (setf *package* package))
            :collect form))))

(defun write-lisp-template (to forms)
  (with-safe-output (stream to)
    (with-template-io-syntax
      (mapcar (lambda (form)
                (when-let (package (find-package-in-form form))
                  (setf *package* package))
                (pprint form stream)
                (terpri stream))
              forms))))


(defun template-file-target-type (pathname)
  (let* ((type (pathname-type pathname))
         (end (- (length type)
                 (length "-template"))))
    (subseq type 0 end)))

(defun target-file-type (pathname)
  (if (template-p pathname)
      (template-file-target-type pathname)
    (pathname-type pathname)))

(defun target-pathname (pathname destination)
  (make-pathname :type (target-file-type pathname)
                 :defaults (merge-pathnames pathname destination)))

(defun process-simple-file (from to)
  (debug-log "; copy file ~A to ~A ~%" from to)
  (unless *dry-run-p*
    (with-simple-restart (continue "Supersede ~A." to)
      (when (probe-file to)
        (error "File ~A already exists." to)))
    (copy-file from to)))

(defun process-lisp-template (from to)
  (debug-log "; process lisp template ~A to ~A~%" from to)
  (let ((forms (read-lisp-template from)))
    (unless *dry-run-p*
      (write-lisp-template to forms))))

(defun process-generic-template (from to)
  (debug-log "; process text template ~A to ~A~%" from to)
  (let ((string (file-string from)))
    (unless *dry-run-p*    
      (with-safe-output (stream to)
        (write-string (do-substitutions string)
                      stream)))))

(defun process-a-file (source destination pathname)
  "Process a template file, that is, a file whose extension ends with -template."
  (let ((source (merge-pathnames pathname source))
        (destination (target-pathname pathname destination)))
    (unless *dry-run-p*
      (ensure-directories-exist destination))
    (cond
     ((lisp-template-p pathname) (process-lisp-template source destination))
     ((template-p pathname) (process-generic-template source destination))
     (t (process-simple-file source destination)))))

(defun list-template-directory (source relative-pathname)
  "Return the list of pathnames relative to SOURCE and found
in RELATIVE-PATHNAME."
  (let* ((defaults (merge-pathnames relative-pathname source))
         (pathname-pattern (make-pathname :name :wild :type :wild :defaults defaults))
         (pathnames (directory pathname-pattern)))
    (mapcar (lambda (pathname)
              (pathname-as-relative source pathname))
            pathnames)))



;; TODO: See how we can automate git repository creation on Windows.
(defun initialize-git-in-directory (directory)
  (sys:call-system-showing-output
   (list *git* "init" (namestring directory))
   :show-cmd nil)
  (with-safe-output (stream (merge-pathnames ".gitignore" directory))
    (write-string *dot-git-ignore* stream))
  (values))

(defun process-project-template (source destination &optional (pathname #P""))
  "Recursively process files in PATHNAME relatively to SOURCE, and put
the processed files in PATHNAME, this time relatively to DESTINATION."
  (dolist (pathname (list-template-directory source pathname))
    (if (file-directory-p
         (merge-pathnames pathname source))
        (process-project-template source destination pathname)
      (process-a-file source destination pathname))))

(defun run (system-name product-name
            &key (project-template-name *default-project-template*)
                 (destination (get-working-directory))
                 #-WINDOWS
                 (git-init-p t))
  (let ((template-directory (truename
                             (sys:directory-pathname
                              (merge-pathnames project-template-name
                                               *project-templates-directory*))))
        (project-directory (sys:directory-pathname
                            (merge-pathnames (last-dotted-component
                                              (string-downcase system-name))
                                             destination))))
    (with-template-variables (system-name product-name)
      (process-project-template template-directory project-directory))
    #-WINDOWS
    (when git-init-p
      (initialize-git-in-directory project-directory))
    project-directory))

(defun find-project-templates ()
  (mapcar (lambda (pathname)
            (car (last
             (pathname-directory pathname))))
          (directory *project-templates-directory*
                     :test #'file-directory-p)))
