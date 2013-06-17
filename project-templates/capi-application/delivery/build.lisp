(in-package "CL-USER")

(load (current-pathname "utilities"))
(load (current-pathname "shared-settings") :print t)

(defparameter *delivery-script*
  (current-pathname "deliver"))

(defparameter *rm*
  ;; TODO: adapt this script to Windows and Linux.
  "/bin/rm")

(defun normalize-command-argument (object)
  (cond
   ((pathnamep object)
    (namestring object))
   (t object)))

(defun call-system (&rest command)
  (sys:call-system-showing-output
   (mapcar #'normalize-command-argument command)))

(defun run-deliver ()
  (format t "~&; *** Delivery~%")
  (call-system (lisp-image-name) "-build" *delivery-script*))

(defun run-clean ()
  (format t "~&; *** Cleaning~%")
  (call-system *rm* "-rf" *product*))

(defun run-codesign ()
  (format t "~&; *** Codesigning~%")
  (call-system "/usr/bin/codesign" "-s" *codesign-identity* *product*))

(run-clean)
(run-deliver)
#+MACOSX (run-codesign)
(quit)
