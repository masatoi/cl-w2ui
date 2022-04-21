;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cl-user)

(defpackage cl-w2ui.utils
  (:nicknames :utils)
  (:use :cl :cl-who)
  (:import-from :alexandria)
  (:export #:define-class
           #:cat
	   #:html
	   #:with-head
	   #:render-button))

(in-package :cl-w2ui.utils)

;;; Utilities

(defmacro define-class (class-name superclass-list &body body)
  "Simplified definition of classes which similar to definition of structure.
 [Example]
  (define-class agent (superclass1 superclass2)
    currency
    position-list
    (position-upper-bound :initform 1 :type single-float)
    log
    money-management-rule)
=> #<STANDARD-CLASS AGENT>"
  (alexandria:with-gensyms (class initargs)
    `(prog1
         (defclass ,class-name (,@superclass-list)
           ,(mapcar (lambda (slot)
                      (let* ((slot-symbol (if (listp slot) (car slot) slot))
                             (slot-name (symbol-name slot-symbol))
                             (slot-initval (if (listp slot)
                                               (getf (cdr slot) :initform)
                                               nil))
                             (slot-type (if (listp slot)
                                            (getf (cdr slot) :type)
                                            t)))
                        (list slot-symbol
                              :accessor (intern slot-name)
                              :initarg (intern slot-name :keyword)
                              :initform slot-initval
                              :type slot-type)))
             body))

       (defmethod initialize-instance :before ((,class ,class-name)
                                               &rest ,initargs
                                               &key ,@(mapcar (lambda (slot)
                                                                (etypecase slot
                                                                  (list (if (getf (cdr slot) :initform)
                                                                            (list (car slot)
                                                                                  (getf (cdr slot) :initform))
                                                                            (car slot)))
                                                                  (symbol slot)))
                                                              body)
                                               &allow-other-keys)
         (declare (ignorable ,initargs
                             ,@(mapcar (lambda (slot)
                                         (etypecase slot
                                           (list (car slot))
                                           (symbol slot)))
                                       body)))
         ,@(remove nil
                   (mapcar (lambda (slot)
                             (when (and (listp slot) (getf (cdr slot) :type))
                               `(check-type ,(car slot) ,(getf (cdr slot) :type))))
                           body))))))

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

(defmacro with-head ((&key title css script favicon (content-type "text/html") (encoding "UTF-8")) &body body)
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
		,@(if favicon `((:link :rel "shortcut icon" :type "image/vnd.microsoft.icon" :href ,favicon)
				(:link :rel "icon" :type "image/vnd.microsoft.icon" :href ,favicon)))
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
