(in-package "APP-GENERATOR")

(defparameter *variable-substitution-package*
  (or (find-package "APP-TEMPLATE")
      (make-package "APP-TEMPLATE")))

(defparameter *variables* ())

(defun clear-variables ()
  (dolist (var *variables*)
    (makunbound var))
  (setf *variables* ()))

(defmacro define-variable (name documentation)
  (let ((symbol (intern (string name) *variable-substitution-package*)))
    `(eval-when (:load-toplevel :compile-toplevel :execute)
       (defvar ,symbol nil
         ,documentation)
       (export ',symbol *variable-substitution-package*)
       (push ',symbol *variables*))))

(defun substitution-variable-p (object)
  (and (symbolp object)
       (eq (symbol-package object)
           *variable-substitution-package*)))

(defun read-string-up-to (endchar &optional (stream *standard-input*))
  (loop :with result = nil
        :for char := (read-char stream)
        :while (not (eql char endchar))
        :do (push char result)
        :finally (return
                  (coerce (nreverse result)
                         'string))))

(defun |#{-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((*package* *variable-substitution-package*)
         (form (read-from-string
                (read-string-up-to #\} stream))))
    (eval form)))

(defun install-template-reader-macros ()
  (set-dispatch-macro-character #\# #\{ #'|#{-reader|))

(defmacro with-bound-variables ((app-name system-name app-version) &body body)
  `(let ,*variables*
     (setup-variables ,app-name ,system-name ,app-version)
     ,@body))

(defmacro with-template-reader-macros (&body body)
  `(let ((*readtable* (copy-readtable nil)))
     (install-template-reader-macros)
     ,@body))

(editor:setup-indent 'define-variable 1)
