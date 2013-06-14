(in-package "APP-GENERATOR")

(clear-variables)

(define-variable app-name
  "[string] The application name as it is displayed to the user.")

(define-variable app-version
  "[string] The version of the application, typically 1.0.")

(define-variable system-name
  "[string] Name given to defsystem to build the application.")

(define-variable app-package-name
  "[string] Name of the package for the application")

(define-variable main-function-symbol
  "[symbol] Symbol representing the main entry point in the application.")

(define-variable main-function-name
  "[string] String form of the main-function variable.")

(define-variable main-interface-class-symbol
  "[symbol] Symbol representing the main interface class.")

(define-variable osx-bundle-identifier
  "[string] Reverse-DNS style name that appears in the Info.plist file.")

(defparameter *app-package* nil)

(defparameter *main-function-default-name* "MAIN")

(defun set-app-package (system-name)
  (setf app-template:app-package-name system-name
        *app-package* (or (find-package app-template:app-package-name)
                          (make-package app-template:app-package-name
                                        :use '("CL" "CL-USER")))))

(defun app-symbol (name)
  (intern name *app-package*))

(defun set-main-symbols (system-name)
  (setf app-template:main-function-symbol (app-symbol *main-function-default-name*)
        app-template:main-function-name (string app-template:main-function-symbol)
        app-template:main-interface-class-symbol (app-symbol (format nil "~A-INTERFACE" system-name)))
  (export app-template:main-function-symbol *app-package*)
  (export app-template:main-interface-class-symbol *app-package*))

(defun setup-variables (app-name system-name app-version)
  (assert (and app-name
               system-name
               app-version))
  (setf system-name
        (string-upcase system-name))
  (set-app-package system-name)
  (set-main-symbols system-name)
  (setf app-template:app-name app-name
        app-template:app-version app-version
        app-template:system-name system-name
        app-template:osx-bundle-identifier (string-downcase system-name)))
