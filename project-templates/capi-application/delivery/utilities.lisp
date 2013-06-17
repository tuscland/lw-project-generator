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

(defun target-executable-path (product-name &optional product-bundle-extension)
  (current-pathname (make-pathname :name product-name
                                   :directory (list :relative :up "build" (app-build-architecture)))
                    product-bundle-extension))
