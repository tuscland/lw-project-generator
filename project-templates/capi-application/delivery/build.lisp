;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP -*-

(in-package "CL-USER")

(load (current-pathname "utilities"))
(load (current-pathname "shared-settings") :print t)

(defparameter *delivery-script*
  (namestring
   (current-pathname "deliver")))

(defparameter *target-directory*
  (namestring
   (target-directory)))

(defun call-system (&rest args)
  (let ((result (apply #'sys:call-system-showing-output args)))
    (unless (zerop result)
      (error "~&Calling ~A resulted in status code ~A~%"
             args
             result))))

(defun run-deliver ()
  (format t "~&; *** Delivery~%")
  (call-system
   (list (lisp-image-name) "-build" *delivery-script*)))

(defun run-clean ()
  (format t "~&; *** Cleaning~%")
  (ecase (platform)
    (:macosx
     (call-system
      (list "/bin/rm" "-rf" *target-directory*)))
    (:mswindows
      (when (probe-file *target-directory*)
        (call-system
         (string-append "RMDIR /S /Q \"" *target-directory* "\""))))))

(defun run-codesign ()
  (case (platform)
    (:macosx
     (format t "~&; *** Codesigning~%")
     (call-system
      (list "/usr/bin/codesign" "-s"
            *codesign-identity*
            (namestring *product*))))))

(run-clean)
(run-deliver)
(run-codesign)
(quit)
