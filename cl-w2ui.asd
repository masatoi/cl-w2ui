#|
  This file is a part of cl-w2ui project.
  Author: Satoshi Imai (satoshi.imai@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-w2ui-asd
  (:use :cl :asdf))
(in-package :cl-w2ui-asd)

(defsystem cl-w2ui
  :version "0.1"
  :author "Satoshi Imai"
  :license "LLGPL"
  :depends-on (:clack
	       :ningle
               :cl-ppcre
	       :cl-who
	       :parenscript
	       :alexandria
	       :anaphora)
  :components ((:module "src"
                :components
                ((:file "app")
		 (:file "utils")
		 (:file "w2ui" :depends-on ("utils"))
		 (:file "example" :depends-on ("utils" "w2ui"))
		 )))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-w2ui-test))))
