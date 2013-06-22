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
      (quit :status result :ignore-errors-p t))))

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
     (call-system
      (string-append "RMDIR " *target-directory* "/S" "/Q")))))

(defun run-codesign ()
  (ecase (platform)
    (:macosx
     (format t "~&; *** Codesigning~%")
     (call-system
      (list "/usr/bin/codesign" "-s"
            *codesign-identity*
            (namestring *product*))))
    (otherwise
     (do-nothing))))

(run-clean)
(run-deliver)
(run-codesign)
(quit)
