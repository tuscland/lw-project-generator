;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP -*-

(in-package "CL-USER")

(defun has-feature (feature)
  (member feature *features*))

(defun architecture ()
  (cond
   ((has-feature :LISPWORKS-32BIT) "i386")
   ((has-feature :LISPWORKS-64BIT) "x64")
   (t "unknown")))

(defun app-build-architecture ()
  (format nil "~A-~A" (software-type) (architecture)))

(defun target-directory ()
  (make-pathname :directory (append
                             (butlast (pathname-directory
                                       (current-pathname)))
                             (list "build"
                                   (app-build-architecture)))))

(defun target-executable-path (product-name &optional product-bundle-extension)
  (make-pathname :name product-name
                 :type product-bundle-extension
                 :defaults (target-directory)))

(defun platform ()
  #+macosx :macosx
  #+mswindows :mswindows
  #+linux :linux
  #-(or macosx mswindows linux) :unknown)

(defun build-date ()
  (multiple-value-bind (seconds minutes hours day month year &rest)
      (decode-universal-time (get-universal-time))
    (format nil "~A-~2,'0D-~2,'0D"
            year month day)))
