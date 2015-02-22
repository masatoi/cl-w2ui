;;; -*- Coding:utf-8; Mode:Lisp; -*-

(in-package :cl-user)

(defpackage cl-w2ui.example
  (:nicknames :example)
  (:use :cl :cl-w2ui.utils :cl-w2ui.app :cl-w2ui.w2ui :cl-who)
  (:import-from :parenscript :ps :chain :create :lisp))

(in-package :cl-w2ui.example)

(defparameter layout
  (layout "my-layout"
	  (list (panel 'main :content "<h2>Main panel</h2>")
		(panel 'top :size 100 :content "<h2>Top panel</h2>")
		(panel 'left :size 200 :resizable-p t
		       :on-resizing '(lambda (event) (console.log "left panel resized")))
		(panel 'right :size 100 :resizable-p t :content "<h2>Right panel</h2>" :hidden-p t)
		(panel 'preview :size 100 :resizable-p t :content "<h2>Preview panel</h2>" :hidden-p t)
		(panel 'bottom :size 100 :resizable-p t :content "<h2>Bottom panel</h2>" :hidden-p t))))

(defparameter tabs
  (tabs "my-tabs"
	(list (tab "tab1" "Tab1" :on-click `(lambda () ((chain ($ "#tab-body") html) ,(html (:p "This is Tab1.")))))
	      (tab "tab2" "Tab2" :on-click `(lambda (event)					      
					      (console.log event)
					      ((chain ($ "#tab-body") html) ,(html (:p "This is Tab2.")))))
	      (tab "tab3" "Tab3" :on-click (tab-set "tab-body" (html (:p "This is Tab3.")))))))

(defparameter grid
  (grid "my-grid"
	(list (column "fname" "First name"    :size "30%")
	      (column "lname" "Last name"     :size "30%")
	      (column "email" "Email Address" :size "40%"))
	:records '(("John"    "Smith"  "john.smith@foo.com")
		   ("Taro"    "Yamada" "yamada@bar.com")
		   ("Satoshi" "Imai"   "satoshi.imai@gmail.com"))))

;; type: button, check, radio, drop, menu, break, spacer, html
(defparameter toolbar
  (toolbar "my-toolbar"
	   (list (item "item1"
		       :text "item1"
		       :icon "fa fa-exclamation-circle"
		       :hint "Hint text"
		       :on-click '(lambda () (alert "Toolbar item1: alert")))
		 (item "item2" :text "item2" :icon "fa fa-times-circle")
		 (item "break" :text "break" :type "break") ; insert bar
		 (item "check" :text "check" :type "check" :icon "fa fa-star")
		 (item "radio1" :text "radio1" :type "radio" :icon "fa fa-check")
		 (item "radio2" :text "radio2" :type "radio" :icon "fa fa-flag")
		 (item "break"  :type "break")
		 (item "html" :html (html (:strong "HTML")) :type "html")
		 (item "spacer" :type "spacer") ; insert space
		 (item "break"  :type "break")
		 (item "drop" :text "drop" :type "drop" :html "<p>This is drop</p>")
		 (item "menu" :text "menu" :type "menu"
		       :sub-items (list (sub-item "sub-item1" :text "sub-item1"
						  :on-click '(lambda () (alert "This is sub-item1")))
					(sub-item "sub-item2" :text "sub-item2"
						  :on-click '(lambda (event) (console.log event))))))))

(defparameter form
  (form "my-form"
	;; fields
	(list (field "First-name" "text" :required t)
	      (field "Last-name" "text"  :required t)
	      (field "Number" "int" :options (make-form-field-options :min 0 :max 100)))
	;; actions
	(list (action "Reset" '(lambda () ((chain this clear))))
	      (action "Error" '(lambda () ((chain this error) "This is Error.")))
	      (action "Save"  '(lambda () ((chain this save)))))
	:post-url "/post"))

(defparameter popup
  (popup :title "Popup title"
	 :body (html (:h2 "Popup sample"))
	 :show-close 'true
	 :show-max 'false
	 ;; html which describes buttons area
	 :buttons (cat (render-button "OK" :on-click (popup-close) :id "popup-ok")
		       (render-button "Cancel" :on-click (popup-close)))
	 :on-keydown '(lambda (event) ; describe keycode
		       (console.log (+ "Pressed keycode: " (chain event original-event key-code))))))

(defparameter sidebar
  (sidebar
   "my-sidebar"
   (list (node "layout-node" "Layout"   :icon "fa fa-th-large"        :on-click (layout-load layout 'main "/layout-operations"))
	 (node "tabs-node"    "Tabs"    :icon "fa fa-list-alt"        :on-click (layout-load layout 'main "/tabs"))
	 (node "grid-node"    "Grid"    :icon "fa fa-table"           :on-click (layout-load layout 'main "/grid"))
	 (node "toolbar-node" "Toolbar" :icon "fa fa-ellipsis-h"      :on-click (layout-load layout 'main "/toolbar"))
	 (node "form-node"    "Form"    :icon "fa fa-pencil-square-o" :on-click (layout-load layout 'main "/form"))
	 (node "popup-node"   "Popup"   :icon "fa fa-external-link"   :on-click (layout-load layout 'main "/popup"))
	 (node "expandable-node" "Expandable node" :icon "fa fa-folder" :expanded-p t
	       :nodes (list (node "sub-node1" "Sub node1")
			    (node "sub-node2" "Sub node2"))))
   :enable-keyboard-p 'true))

(defroute "/" (params)
  (declare (ignore params))
  (with-head (:title "w2ui demo"
	      :css ("/css/w2ui-1.4.2.css" "/css/font-awesome.css" "/css/example.css")
	      :script ("/js/jquery-2.1.1.js" "/js/w2ui-1.4.2.js"))
    (html
      (:div :id "my-layout" :style "width: 100%; height: 100%;")
      (:script
       (str
	(cat
	 ;; define Javascript w2ui objects
	 (define-w2ui-objects layout grid sidebar toolbar form tabs)
	 ;; set sidebar to left panel of layout
	 (ps (funcall (lisp (layout-set layout 'left sidebar))))))))))

(defroute "/layout-operations" (params)
  (declare (ignore params))
  (html (:h2 "Layout operations")
	(str (render-button
	      "Show all panel"
	      :on-click `(progn
			   (funcall ,(layout-show layout 'right))
			   (funcall ,(layout-show layout 'preview))
			   (funcall ,(layout-show layout 'bottom)))))
	(str (render-button
	      "Hide"
	      :on-click `(progn
			   (funcall ,(layout-hide layout 'right))
			   (funcall ,(layout-hide layout 'preview))
			   (funcall ,(layout-hide layout 'bottom)))))))

(defroute "/tabs" (params)
  (declare (ignore params))
  (html
    (:h2 "Tabs sample")
    (:div :id "my-tabs" :style "width: 100%;")
    (:div :id "tab-body" :style "width: 100%;")
    (:script (str (render-w2ui-objects tabs)))))

(defroute "/grid" (params)
  (declare (ignore params))
  (html (:h2 "Grid sample")
	(:div :id "my-grid" :style "width: 100%; height: 400px;")
	(:script (str (render-w2ui-objects grid)))))

(defroute "/toolbar" (params)
  (declare (ignore params))
  (html (:h2 "Toolbar sample")
	(:div :id "my-toolbar" :style "width: 100%;")
	(:script (str (render-w2ui-objects toolbar)))))

(defroute "/form" (params)
  (declare (ignore params))
  (html
    (:h2 "Form sample")
    (:div :id "my-form" :style "width: 100%;")
    (:script (str (render-w2ui-objects form)))))

;; return JSON object
(defroute "/post" (params :method :post)
  (print params)
  ;; "{ \"status\": \"success\" }"
  "{ \"status\": \"error\", \"message\": \"Server Error.\" }"
  )

(defroute "/popup" (params)
  (declare (ignore params))
  (html (:h2 "Popup sample")
	(str (render-button "Display popup" :on-click (popup-open popup)))))
