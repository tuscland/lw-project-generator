(in-package "APP-GENERATOR")

(defparameter *default-template* "app-template")
(defparameter *default-version* "1.0")
(defvar *dry-run*)

(defparameter *templates-directory*
  (merge-pathnames #P"../"
                   (pathname-location *load-truename*)))

(defmacro with-pretty-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*print-case* :downcase)
           (*print-pretty* t))
       ,@body)))

(defun read-lisp-template (pathname)
  (with-open-file (stream pathname
                          :direction :input)
    (with-standard-io-syntax
      (with-template-reader-macros
        (loop :for form := (read stream nil)
              :while form
              :do (when-let (package (find-package-in-form form))
                    (format t "~&Reading package to ~A in ~A~%" package stream)
                    (setf *package* package))
            :collect form)))))

(defun find-package-in-form (form)
  (and (eq (first form) 'in-package)
       (find-package (second form))))

(defun print-forms (forms &optional (stream *standard-output*))
  (when-let (package (find-package-in-form (first forms)))
    (format t "~&Writing package to ~A in ~A~%" package stream)
    (setf *package* package))
  (mapcar (lambda (form)
            (pprint form stream)
            (terpri stream))
          forms))

(defun write-lisp-template (forms pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :error)
    (with-pretty-io-syntax
      (print-forms forms stream))))

(defun target-pathname (target-directory pathname &key (type (pathname-type pathname)))
  (make-pathname :type type
                 :defaults (merge-pathnames pathname
                                            target-directory)))

(defun process-simple-file (base-directory target-directory pathname)
  (let ((source-pathname (merge-pathnames pathname base-directory))
        (target-pathname (target-pathname target-directory pathname)))
    (if *dry-run*
        (format t "; copy file ~A to ~A ~%" pathname target-pathname)
      (progn
        (ensure-directories-exist target-pathname)
        (copy-file source-pathname target-pathname)))))

(defun process-lisp-template-file (base-directory target-directory pathname)
  (let* ((source-pathname (merge-pathnames pathname base-directory))
         (target-pathname (target-pathname target-directory pathname :type "lisp"))
         (forms (read-lisp-template source-pathname)))
    (if *dry-run*
      (format t "; process lisp template ~A to ~A~%" pathname target-pathname)
      (progn
        (ensure-directories-exist target-pathname)
        (write-lisp-template forms target-pathname)))))

(defun lisp-template-file-p (path)
  (string-equal (pathname-type path)
                "lisp-template"))

(defun list-template-directory (base-directory relative-pathname)
  (let* ((defaults (merge-pathnames relative-pathname base-directory))
         (pathnames (directory (make-pathname :name :wild
                                              :type :wild
                                              :defaults defaults))))
    (mapcar (lambda (pathname)
              (pathname-as-relative base-directory pathname))
            pathnames)))

(defun process-template-directory (base-directory target-directory &optional (pathname #P""))
  (dolist (relative-pathname (list-template-directory base-directory pathname))
    (let* ((pathname (merge-pathnames relative-pathname base-directory))
           (action (cond
                    ((file-directory-p pathname) #'process-template-directory)
                    ((lisp-template-file-p pathname) #'process-lisp-template-file)
                    (t #'process-simple-file))))
      (funcall action base-directory target-directory relative-pathname))))

(defun invoke-template (&key system-name app-name (version *default-version*)
                             dry-run
                             (template-name *default-template*)
                             (target-directory (get-working-directory)))
  (let ((*dry-run* dry-run)
        (template-directory (truename
                             (sys:directory-pathname
                              (merge-pathnames template-name
                                               *templates-directory*))))
        (app-directory (sys:directory-pathname
                        (merge-pathnames (string-downcase system-name)
                                         target-directory))))
    (with-bound-variables (app-name system-name version)
      (process-template-directory template-directory app-directory))))
