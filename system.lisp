(in-package "CL-USER")

(defsystem "APP-GENERATOR"
  (:default-pathname "src")
  :members ("package"
            "utilities"
            "variables-package"
            "app-generator"
            "variables")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load "package")
            (:load "variables-package")))
          (:in-order-to :compile "app-generator"
           (:requires
            (:load "utilities")))
          (:in-order-to :compile "variables"
           (:requires
            (:load "app-generator")))))

