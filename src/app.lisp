;;; -*- Coding:utf-8; Mode:Lisp; -*-

(in-package :cl-user)

(defpackage cl-w2ui.app
  (:nicknames :cl-w2ui :app)
  (:use :cl :anaphora
	:clack
	:clack.request
	:clack.middleware.static
	:clack.middleware.session
	:clack.middleware.accesslog)
  (:shadow :stop)
  (:export :start
	   :stop
	   :defroute
	   :store-to-session
	   :get-from-session
	   :remove-from-session))

(in-package :cl-w2ui.app)

;;; Application handler
(defvar *handler*)

;; Setting for Static
(defparameter *static-middleware*
  (let* ((app-root-directory (asdf:system-source-directory :cl-w2ui))
	 (static-directory   (merge-pathnames #P"static/" app-root-directory)))
    (make-instance '<clack-middleware-static>
       :path (lambda (path)
	       (if (ppcre:scan "^(?:/images/|/css/|/js/|/fonts/|/robot\\.txt$|/favicon.ico$)" path)
		 path))
       :root static-directory)))

(defparameter *app* (make-instance 'ningle:<app>))

;; start/stop function (contstruct clack application)
(defun start (&key (port 5000) (server :hunchentoot))
  (setf *handler*
	(clackup
	 (wrap
	  *static-middleware*
	  (wrap
	   (make-instance '<clack-middleware-session>)
		 *app*))
	 :port port :server server)))

(defun stop () (clack.handler:stop *handler*))

(defmacro defroute (name (params &rest route-args) &body body)
  "Example:
 (defroute \"/\" (params)
   (html (:p \"Hello,world!\")))

 (defroute \"/login\" (params :method :POST)
   (if (authorize (assoc \"username\" params :test #'string=)
 		 (assoc \"password\" params :test #'string=))
     \"Authorized!\"
     \"Failed...Try again.\"))"
  `(setf (ningle:route *app* ,name ,@route-args)
	 (lambda (,params) ,@body)))

;;; session operators
(defun store-to-session (key val request)
  (let ((env (lack.request:request-env request)))
    (setf (gethash key (getf env :lack.session)) val)))

(defun get-from-session (key request)
  (let ((env (lack.request:request-env request)))
    (gethash key (getf env :lack.session))))

(defun remove-from-session (key request)
  (let ((env (lack.request:request-env request)))
    (remhash key (getf env :lack.session))))
