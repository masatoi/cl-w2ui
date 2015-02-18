;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cl-user)

(defpackage cl-w2ui.utils
  (:nicknames :utils)
  (:use :cl :cl-who)
  (:export :cat
	   :html
	   :with-head
	   :render-button))
	   
(in-package :cl-w2ui.utils)

;;; Utilities

;; concatenate strings
(defun cat (&rest strings)
  (let ((strings (if (listp (car strings)) (car strings) strings)))
    (reduce (lambda (s1 s2)
	      (concatenate 'string (format nil "~A" s1) (format nil "~A" s2)))
	    strings)))

;; cl-who wrapper
(defmacro html (&body body)
  (let ((s (gensym)))
    `(with-html-output-to-string (,s)
       ,@body)))

(defmacro with-head ((&key title css script (content-type "text/html") (encoding "UTF-8")) &body body)
  "Example:
 (with-head (:title \"Title\" :css \"foo.css\")
   (html (:p \"Hello,World!\")))

 (with-head (:title \"Title\" :css (\"foo.css\" \"bar.css\"))
   (html (:p \"Hello,World!\")))"
  `(html (:head ,@(if title `((:title ,title)))
		,@(if (consp css)
		    (mapcar (lambda (css)
			      `(:link :rel "stylesheet" :type "text/css" :href ,css :media "all"))
			    css)
		    `((:link :rel "stylesheet" :type "text/css" :href ,css :media "all")))
		,@(if (consp script)
		    (mapcar (lambda (script) `(:script :src ,script)) script)
		    `((:script :src ,script)))
		(:meta :http-equiv "Content-Type"
		       :content ,(format nil "~A; charset=~A" content-type encoding)))
	 (str ,@body)))

(defun render-button (label &key class id name value on-click)
  (assert (or (stringp on-click) (listp on-click)))
  (let* ((on-click-str (if (and on-click (listp on-click)) (ps:ps (ps:lisp on-click)) on-click))
	 (on-click-str-quoted (if on-click-str
				(cl-ppcre:regex-replace-all "\"" on-click-str "&quot;")
				on-click-str)))
    (format nil "~A"
	    (cat (remove-if #'null
			    (list
			     "<button "
			     (if class (format nil "class=\"~A\" " class))
			     (if id (format nil "id=\"~A\" " id))
			     (if name (format nil "name=\"~A\" " name))
			     (if on-click-str-quoted (format nil "onclick=\"~A\"" on-click-str-quoted))
			     (if value (format nil "value=\"~A\" " value))
			     ">"
			     label
			     "</button>"))))))
