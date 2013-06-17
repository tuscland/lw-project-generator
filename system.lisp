(in-package "CL-USER")

(defsystem "COM.WILDORA.PROJECT-GENERATOR"
  (:default-pathname "src")
  :members ("package"
            "utilities"
            "harness"
            "template-variables"
            "project-generator")
  :rules ((:in-order-to :compile :all
           (:requires
            (:load :previous)))))
