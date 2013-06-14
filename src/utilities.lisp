(in-package "APP-GENERATOR")

(defun pathname-as-relative (parent subpath &optional (defaults *default-pathname-defaults*))
  "Given PARENT and a SUBPATH contained in it, return a relative path
leading from PARENT to SUBPATH.  In other words, (merge-pathnames
(pathname-as-relative parent subpath) parent) == subpath.

Both PARENT and SUBPATH might be either relative or absolute. If only
one is relative, an absolute DEFAULTS pathname must be specified to
allow anchoring it."
  (let* ((parent (system:directory-pathname parent))
         (defaults (and defaults (system:directory-pathname defaults)))
         (parent (if defaults (merge-pathnames parent defaults) parent))
         (subpath (if defaults (merge-pathnames subpath defaults) subpath)))
    (unless (eq (first (pathname-directory parent))
                (first (pathname-directory subpath)))
      (error "DEFAULTS must be specified when mixing relative and absolute input pathnames"))
    (labels ((nil-as-unspecific (value)
               (or value
                   :unspecific))
             (incompatible-pathnames (lhs rhs key)
               (let ((lhs (nil-as-unspecific (funcall key lhs)))
                     (rhs (nil-as-unspecific (funcall key rhs))))
                 (and (or lhs rhs)
                      (not (equal lhs rhs))))))
      (when (incompatible-pathnames parent subpath #'pathname-host)
        (error "Pathnames '~A' and '~A' are located on different ~
                hosts and cannot be expressed relative to each other"
               parent subpath))
      (when (incompatible-pathnames parent subpath #'pathname-device)
        (error "Pathnames '~A' and '~A' are located on different ~
                devices and cannot be expressed relative to each other"
               parent subpath)))    
    (loop :for (pdir . prest) :on (cdr (pathname-directory parent))
          :for (subdir . subrest) :on (cdr (pathname-directory subpath))
          :for up := prest ;; cannot use :previous, since that won't run in the 1-past-last iteration
          :for down := subrest
          :while (equal pdir subdir)
          :finally (return (make-pathname :directory `(:relative ,@(mapcar (constantly :up) up) ,@down)
                                          :defaults subpath)))))
