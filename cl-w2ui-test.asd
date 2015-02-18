#|
  This file is a part of cl-w2ui project.
|#

(in-package :cl-user)
(defpackage cl-w2ui-test-asd
  (:use :cl :asdf))
(in-package :cl-w2ui-test-asd)

(defsystem cl-w2ui-test
  :author ""
  :license ""
  :depends-on (:cl-w2ui
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-w2ui"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
