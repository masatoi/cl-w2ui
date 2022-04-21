;;; -*- Coding:utf-8; Mode:Lisp; -*-

(in-package :cl-user)

(defpackage :cl-w2ui.w2ui
  (:nicknames :w2ui)
  (:use :cl :cl-w2ui.utils)
  (:import-from :alexandria :flatten)
  (:import-from :parenscript :ps :chain :create :lisp)
  (:export :ps-expression-of-w2ui-object
	   :define-w2ui-objects
	   :render-w2ui-objects
	   ;; layout
	   :layout :panel :main :top :bottom :left :right :preview
	   :layout-set :layout-load :layout-show :layout-hide
           :layout-element-id
           :layout-panels
           :layout-padding
           :layout-resizer-size
           :layout-on-destroy
           :layout-on-refresh
           :layout-on-render
           :layout-on-resize
           :layout-on-resizer-click
	   ;; sidebar
	   :sidebar :node
	   ;; grid
	   :grid :column :show
	   ;; toolbar
	   :toolbar :item :sub-item
	   ;; form
	   :form :field :action :make-form-field-options
	   ;; popup
	   :popup :popup-open :popup-close
	   ;; tabs
	   :tabs :tab :tab-set))

(in-package :w2ui)

(defun ps-expression-of-w2ui-object (obj)
  "return Parenscript's input which define obj."
  (typecase obj
    (layout (layout-spec obj))
    (grid (grid-spec obj))
    (sidebar (sidebar-spec obj))
    (toolbar (toolbar-spec obj))
    (form (form-spec obj))
    (tabs (tabs-spec obj))
    (t (error "obj is not cl-w2ui object."))))

(defun define-w2ui-objects (&rest objs)
  "return Javascript string which define objects."
  (ps (lisp (cons 'progn (mapcar #'ps-expression-of-w2ui-object objs)))))

(defun render-w2ui-objects (&rest objs)
  "return a Javascript string which render object."
  (cat (mapcar (lambda (obj)
		 (let ((id (typecase obj
			     (layout (layout-element-id obj))
			     (grid (grid-element-id obj))
			     (sidebar (sidebar-element-id obj))
			     (toolbar (toolbar-element-id obj))
			     (form (form-element-id obj))
			     (tabs (tabs-element-id obj))
			     (t (error "obj is not cl-w2ui object.")))))
		   (typecase obj
		     (form (format nil "$('#~A').w2render('~A');" id id))
		     (tabs (format nil "w2ui['~A'].render('#~A');w2ui['~A'].click('~A');" id id id
				   (if (tabs-active obj) (tabs-active obj) (tab-id (car (tabs-tab-list obj))))))
		     (t    (format nil "w2ui['~A'].render('#~A');" id id)))))
	       objs)))

;;; Layout
(defstruct panel
  :type         ; type of the panel can be: left, right, top, bottom, preview
  :title        ; title for the panel
  (:size nil :type (or null string integer)) ; width or height of the panel depending on panel type
  :min-size     ; minimum size of the panel in px when it is resized
  :max-size     ; if a number, then it defined maximum size of the panel
  :hidden-p     ; indicates if panel is hidden
  :resizable-p  ; indicates if panel is resizable
  :overflow     ; overflow property of the panel, can have same values as similar CSS property
  :style        ; additional css styles for the panel
  :content      ; content of the pane, can be a string or an object with .render(box) method
  :width        ; width of the panel, read only
  :height       ; height of the panel, read only
  :tabs         ; w2tabs object for the panel
  :toolbar      ; w2toolbar object for the panel
  ;; Events
  :on-refresh   ; refresh event for the panel
  :on-resizing  ; resizing evnet for the panel
  :on-show      ; show event for the panel
  :on-hide      ; hide event for the panel
  )

(defun panel (type &key title size min-size max-size hidden-p resizable-p
		        overflow style content width height tabs toolbar
		        on-resizing on-resizer-click on-show on-hide
		        on-refresh on-destroy on-render on-resize)
  (assert (member type '(main top bottom left right preview)))
  (make-panel :type type :title title
	      :size (typecase size (integer (format nil "~Apx" size)) (t size))
	      :min-size min-size :max-size max-size
	      :hidden-p hidden-p :resizable-p resizable-p
	      :overflow overflow :style style :content content
	      :width width :height height :tabs tabs :toolbar toolbar
	      :on-refresh on-refresh :on-resizing on-resizing :on-show on-show :on-hide on-hide
	      ))

(defun panel-spec (panel)
  `(create ,@(if (panel-type panel) `(type ,(string-downcase (symbol-name (panel-type panel)))))
	   ,@(if (panel-title panel) `(title ,(panel-title panel)))
	   ,@(if (panel-size panel) `(size ,(panel-size panel)))
	   ,@(if (panel-min-size panel) `(min-size ,(panel-min-size panel)))
	   ,@(if (panel-max-size panel) `(max-size ,(panel-max-size panel)))
	   ,@(if (panel-hidden-p panel) `(hidden ,(panel-hidden-p panel)))
	   ,@(if (panel-resizable-p panel) `(resizable ,(panel-resizable-p panel)))
	   ,@(if (panel-overflow panel) `(overflow ,(panel-overflow panel)))
	   ,@(if (panel-style panel) `(style ,(panel-style panel)))
	   ,@(if (panel-content panel) `(content ,(panel-content panel)))
	   ,@(if (panel-width panel) `(width ,(panel-width panel)))
	   ,@(if (panel-height panel) `(height ,(panel-height panel)))
	   ,@(if (panel-tabs panel) `(tabs ,(panel-tabs panel)))
	   ,@(if (panel-toolbar panel) `(toolbar ,(panel-toolbar panel)))
	   ,@(if (panel-on-refresh panel) `(on-refresh ,(panel-on-refresh panel)))
	   ,@(if (panel-on-resizing panel) `(on-resizing ,(panel-on-resizing panel)))
	   ,@(if (panel-on-show panel) `(on-show ,(panel-on-show panel)))
	   ,@(if (panel-on-hide panel) `(on-hide ,(panel-on-hide panel)))
	   ))

(defstruct layout
  (:element-id nil :type string)
  (:panels nil :type list) ; list of panels
  (:padding 1 :type integer)
  (:resizer-size 4 :type integer)
  
  ;; Events
  :on-destroy ; Called when object is destroyed.
  :on-refresh ; Called when object is refreshed.
  :on-render  ; Called when object is rendered.
  :on-resize  ; Called when object is resized.
  :on-resizer-click
  )

(defun layout (element-id panels &key (padding 1) (resizer-size 4) on-destroy on-refresh on-render on-resize on-resizer-click)
  (make-layout :element-id element-id :panels panels
	       :padding padding :resizer-size resizer-size
	       :on-destroy on-destroy :on-refresh on-refresh :on-render on-render
	       :on-resize on-resize :on-resizer-click on-resizer-click))

(defun layout-spec (layout)
  `((chain ($ ,(cat "#" (layout-element-id layout))) w2layout)
    (create name   ,(layout-element-id layout)
	    panels ,(cons 'list (mapcar #'panel-spec (layout-panels layout)))
	    padding ,(layout-padding layout)
	    resizer ,(layout-resizer-size layout)
	    ,@(if (layout-on-destroy layout) `(on-destroy ,(layout-on-destroy layout)))
	    ,@(if (layout-on-refresh layout) `(on-refresh ,(layout-on-refresh layout)))
	    ,@(if (layout-on-render layout) `(on-render ,(layout-on-render layout)))
	    ,@(if (layout-on-resize layout) `(on-resize ,(layout-on-resize layout)))
	    ,@(if (layout-on-resizer-click layout) `(on-resizer-click ,(layout-on-resizer-click layout)))
	    )))

;; layout utilities

(defun layout-set (layout panel-type w2ui-obj-or-string)
  `(lambda ()
     ((chain (aref w2ui ,(layout-element-id layout)) content)
      ,(string-downcase (symbol-name panel-type))
      ,(typecase w2ui-obj-or-string
	 (grid `(aref w2ui ,(grid-element-id w2ui-obj-or-string)))
	 (sidebar `(aref w2ui ,(sidebar-element-id w2ui-obj-or-string)))
	 (toolbar `(aref w2ui ,(toolbar-element-id w2ui-obj-or-string)))
	 (form `(aref w2ui ,(form-element-id w2ui-obj-or-string)))
	 (tabs `(aref w2ui ,(tabs-element-id w2ui-obj-or-string)))
	 (string w2ui-obj-or-string)
	 (t (error "w2ui-obj-or-string is neither cl-w2ui object nor string."))))))

;;; For AJAX by using jQuery
;; Send POST/GET request using jQuery.post/get, and set content its result.
;; post-data/get-data is alist, which map key to value.
;; data-type: The type of data expected from the server.
(defun layout-post-set-content (url post-data layout panel-type &key (data-type "html"))
  `(lambda ()
     ((chain $ post) ,url ,(if post-data (cons 'create (flatten post-data)))
      (lambda (data) ; callback
	((chain (aref w2ui ,(layout-element-id layout)) content)
	 ,(string-downcase (symbol-name panel-type))
	 data))
      ,data-type
      )))

(defun layout-get-set-content (url get-data layout panel-type)
  `(lambda ()
     ((chain $ get) ,url ,(if get-data (cons 'create (flatten get-data)))
      (lambda (data) ; callback
	((chain (aref w2ui ,(layout-element-id layout)) content)
	 ,(string-downcase (symbol-name panel-type))
	 data))
      )))

(defun layout-load (layout panel-type path)
  `(lambda ()
     ((chain (aref w2ui ,(layout-element-id layout)) load)
      ,(string-downcase (symbol-name panel-type)) ,path)))

(defun layout-show (layout panel-type)
  `(lambda ()
     ((chain (aref w2ui ,(layout-element-id layout)) show)
      ,(string-downcase (symbol-name panel-type)))))

(defun layout-hide (layout panel-type)
  `(lambda ()
     ((chain (aref w2ui ,(layout-element-id layout)) hide)
      ,(string-downcase (symbol-name panel-type)))))

;;; Sidebar

;; To using custom icon, add this to stylesheet.
;; .my-icon {
;; background-image: url(link/to/image) no-repeat center;
;; }
;; and indicate :img "icon-xxx" (ex. "icon-folder")

;; To using font-awesome, indicate
;; :icon "fa fa-xxx" . See font-awesome http://fortawesome.github.io/Font-Awesome/icons/
(defstruct node
  :id                ; id of the node, must be unique for the whole sidebar
  :text              ; text of the node
  :count             ; text of the badge that will appear on the right
  :img               ; css class of the image that will appear on the left
  :icon              ; css class of the font icon that will appear on the left
  :nodes             ; array of sub node objects
  :style             ; additional style for the node
  :route             ; route for the node item
  :selected-p        ; indicates if node is selected
  :expanded-p        ; indicates if node is expanded
  :hidden-p          ; indicates if node is hidden
  :disabled-p        ; indicates is node is disabled
  :group-p           ; indicates if node is a group
  :plus-p            ; if true, then + will be shown even if there is no sub nodes
  ;; Events
  :on-click          ; mouse click event handler
  :on-dbl-click      ; mouse double click event handler
  :on-context-menu   ; mouse right click event handler
  :on-open           ; mouse open node event handler
  :on-close          ; mouse close node event handler
  )

(defun node (id text &key count img icon nodes style route
		       selected-p expanded-p hidden-p disabled-p group-p plus-p
		       on-click on-dbl-click on-context-menu on-open on-close)
  (make-node :id id :text text :count count :img img :icon icon
	     :nodes nodes :style style :route route
	     :selected-p selected-p :expanded-p expanded-p :hidden-p hidden-p
	     :disabled-p disabled-p :group-p group-p :plus-p plus-p
	     :on-click on-click :on-dbl-click on-dbl-click
	     :on-context-menu on-context-menu :on-open on-open :on-close on-close))

(defun node-spec (node)
  `(create ,@(if (node-id node) `(id ,(node-id node)))
	   ,@(if (node-text node) `(text ,(node-text node)))
	   ,@(if (node-count node) `(count ,(node-count node)))
	   ,@(if (node-img node) `(img ,(node-img node)))
	   ,@(if (node-icon node) `(icon ,(node-icon node)))
	   ,@(if (node-nodes node) `(nodes ,(cons 'list (mapcar #'node-spec (node-nodes node)))))
	   ,@(if (node-style node) `(style ,(node-style node)))
	   ,@(if (node-route node) `(route ,(node-route node)))
	   ,@(if (node-selected-p node) `(selected ,(node-selected-p node)))
	   ,@(if (node-expanded-p node) `(expanded ,(node-expanded-p node)))
	   ,@(if (node-hidden-p node) `(hidden ,(node-hidden-p node)))
	   ,@(if (node-disabled-p node) `(disabled ,(node-disabled-p node)))
	   ,@(if (node-group-p node) `(group ,(node-group-p node)))
	   ,@(if (node-plus-p node) `(plus ,(node-plus-p node)))
	   ,@(if (node-on-click node) `(on-click ,(node-on-click node)))
	   ,@(if (node-on-dbl-click node) `(on-dbl-click ,(node-on-dbl-click node)))
	   ,@(if (node-on-context-menu node) `(on-context-menu ,(node-on-context-menu node)))
	   ,@(if (node-on-open node) `(on-open ,(node-on-open node)))
	   ,@(if (node-on-close node) `(on-close ,(node-on-close node)))))

;; Full spec list: http://w2ui.com/web/docs/w2sidebar.nodes
(defstruct sidebar
  (:element-id nil :type string)
  (:nodes nil :type list)
  :enable-keyboard-p        ; Indicates if sidebar should listen to keyboard.
  :menu            ; Array, default = []. Context menu for the sidebar.
  ;; Events  
  :on-click        ; Called when user clicks the node.
  :on-collapse     ; Called when user collapses sub-nodes of the node.
  :on-context-menu ; Called when user clicks right mouse button on the node.
  :on-dbl-click    ; Called when user double clicks the node.
  :on-expand       ; Called when user expands sub-nodes of the node.
  :on-keydown      ; Called when user clicks a keyboard key and sidebar is active.
  :on-menu-click   ; Called when user selects and item from the context menu.
  :on-destroy      ; Called when object is destroyed.
  :on-refresh      ; Called when object is refreshed.
  :on-render       ; Called when object is rendered.
  :on-resize       ; Called when object is resized.
  )

(defun sidebar (element-id nodes &key enable-keyboard-p menu
				   on-click on-collapse on-context-menu on-dbl-click on-expand on-keydown
				   on-menu-click on-destroy on-refresh on-render on-resize)
  (make-sidebar :element-id element-id :nodes nodes :enable-keyboard-p enable-keyboard-p :menu menu
		:on-click on-click :on-collapse on-collapse :on-context-menu on-context-menu :on-dbl-click on-dbl-click
		:on-expand on-expand :on-keydown on-keydown :on-menu-click on-menu-click :on-destroy on-destroy :on-refresh on-refresh
		:on-render on-render :on-resize on-resize))

(defun sidebar-spec (sidebar)
  `((chain ($ ,(cat "#" (sidebar-element-id sidebar))) w2sidebar)
    (create name  ,(sidebar-element-id sidebar)
	    img   nil
	    nodes ,(cons 'list (mapcar #'node-spec (sidebar-nodes sidebar)))
	    ,@(if (sidebar-enable-keyboard-p sidebar) `(keyboard ,(sidebar-enable-keyboard-p sidebar)))
	    ,@(if (sidebar-menu sidebar) `(menu ,(sidebar-menu sidebar)))
	    ,@(if (sidebar-on-click sidebar) `(on-click ,(sidebar-on-click sidebar)))
	    ,@(if (sidebar-on-collapse sidebar) `(on-collapse ,(sidebar-on-collapse sidebar)))
	    ,@(if (sidebar-on-context-menu sidebar) `(on-context-menu ,(sidebar-on-context-menu sidebar)))
	    ,@(if (sidebar-on-dbl-click sidebar) `(on-dbl-click ,(sidebar-on-dbl-click sidebar)))
	    ,@(if (sidebar-on-expand sidebar) `(on-expand ,(sidebar-on-expand sidebar)))
	    ,@(if (sidebar-on-keydown sidebar) `(on-keydown ,(sidebar-on-keydown sidebar)))
	    ,@(if (sidebar-on-menu-click sidebar) `(on-menu-click ,(sidebar-on-menu-click sidebar)))
	    ,@(if (sidebar-on-destroy sidebar) `(on-destroy ,(sidebar-on-destroy sidebar)))
	    ,@(if (sidebar-on-refresh sidebar) `(on-refresh ,(sidebar-on-refresh sidebar)))
	    ,@(if (sidebar-on-render sidebar) `(on-render ,(sidebar-on-render sidebar)))
	    ,@(if (sidebar-on-resize sidebar) `(on-resize ,(sidebar-on-resize sidebar)))
	    )))

;;; Grid

(defstruct column
  (:field nil :type (or null string))    ; field name to map column to a record
  (:caption nil :type (or null string))  ; column caption 
  (:size nil :type (or null string integer)) ; size of column in px or %
  (:min 15 :type  (or null integer))   ; minimum width of column in px
  (:max nil :type (or null integer))  ; maximum width of column in px
  :grid-min-width  ; minimum width of the grid when column is visible
  :size-corrected  ; read only, corrected size (see explanation below)
  :size-calculated ; read only, size in px (see explanation below)
  :hidden-p        ; indicates if column is hidden
  :sortable-p      ; indicates if column is sortable
  :searchable-p    ; indicates if column is searchable, bool/string: int,float,date,...
  :resizable-p     ; indicates if column is resiable
  :hideable-p      ; indicates if column can be hidden
  :attr            ; string that will be inside the <td ... attr> tag
  :style           ; additional style for the td tag
  :render          ; string or render function
  :title           ; string or function for the title property for the column cells
  :editable        ; editable object if column fields are editable
  )

(defun column (field caption
	       &key size min max grid-min-width size-corrected size-calculated
		 hidden-p sortable-p searchable-p (resizable-p t) (hideable-p t)
		 attr style render title editable)
  (make-column :field field :caption caption
	       :size (typecase size (integer (format nil "~Apx" size)) (t size))
	       :min min :max max
	       :grid-min-width grid-min-width :size-corrected size-corrected
	       :size-calculated size-calculated :hidden-p hidden-p :sortable-p sortable-p
	       :searchable-p searchable-p :resizable-p resizable-p :hideable-p hideable-p
	       :attr attr :style style :render render :title title :editable editable))

(defun column-spec (column)
  `(create
    ,@(if (column-field column) `(field ,(column-field column)))
    ,@(if (column-caption column) `(caption ,(column-caption column)))
    ,@(if (column-size column) `(size ,(column-size column)))
    ,@(if (column-min column) `(min ,(column-min column)))
    ,@(if (column-max column) `(max ,(column-max column)))
    ,@(if (column-grid-min-width column) `(grid-min-width ,(column-grid-min-width column)))
    ,@(if (column-size-corrected column) `(size-corrected ,(column-size-corrected column)))
    ,@(if (column-size-calculated column) `(size-calculated ,(column-size-calculated column)))
    ,@(if (column-hidden-p column) `(hidden ,(column-hidden-p column)))
    ,@(if (column-sortable-p column) `(sortable ,(column-sortable-p column)))
    ,@(if (column-searchable-p column) `(searchable ,(column-searchable-p column)))
    ,@(if (column-resizable-p column) `(resizable ,(column-resizable-p column)))
    ,@(if (column-hideable-p column) `(hideable ,(column-hideable-p column)))
    ,@(if (column-attr column) `(attr ,(column-attr column)))
    ,@(if (column-style column) `(style ,(column-style column)))
    ,@(if (column-render column) `(render ,(column-render column)))
    ,@(if (column-title column) `(title ,(column-title column)))
    ,@(if (column-editable column) `(editable ,(column-editable column)))))

(defstruct show
  (:header-p nil)         ; indicates if header is visible
  (:toolbar-p nil)        ; indicates if toolbar is visible
  (:footer-p nil)         ; indicates if footer is visible
  (:column-headers-p t)   ; indicates if columns is visible
  (:line-numbers-p nil)   ; indicates if line numbers column is visible
  (:expand-column-p nil)  ; indicates if expand column is visible
  (:select-column-p nil)  ; indicates if select column is visible
  (:empty-records-p t)    ; indicates if empty records are visible
  (:toolbar-reload-p t)   ; indicates if toolbar reload button is visible
  (:toolbar-columns-p t)  ; indicates if toolbar columns button is visible
  (:toolbar-search-p t)   ; indicates if toolbar search controls are visible
  (:toolbar-add-p t)      ; indicates if toolbar add new button is visible
  (:toolbar-edit-p t)     ; indicates if toolbar edit button is visible
  (:toolbar-delete-p t)   ; indicates if toolbar delete button is visible
  (:toolbar-save-p t)     ; indicates if toolbar save button is visible
  (:selection-border-p t) ; display border arround selection (for selectType = 'cell')
  (:record-titles-p t)    ; indicates if to define titles for records
  (:skip-records-p t)     ; indicates if skip records should be visible
  )

(defun show (&key header-p toolbar-p footer-p (column-headers-p t) line-numbers-p expand-column-p
	       select-column-p (empty-records-p t) (toolbar-reload-p t) (toolbar-columns-p t) (toolbar-search-p t)
	       (toolbar-add-p t) (toolbar-edit-p t) (toolbar-delete-p t) (toolbar-save-p t) (selection-border-p t)
	       (record-titles-p t) (skip-records-p t))
  (make-show :header-p header-p :toolbar-p toolbar-p :footer-p footer-p
	     :column-headers-p column-headers-p :line-numbers-p line-numbers-p
	     :expand-column-p expand-column-p :select-column-p select-column-p
	     :empty-records-p empty-records-p :toolbar-reload-p toolbar-reload-p
	     :toolbar-columns-p toolbar-columns-p :toolbar-search-p toolbar-search-p
	     :toolbar-add-p toolbar-add-p :toolbar-edit-p toolbar-edit-p
	     :toolbar-delete-p toolbar-delete-p :toolbar-save-p toolbar-save-p
	     :selection-border-p selection-border-p
	     :record-titles-p record-titles-p :skip-records-p skip-records-p))

(defun show-spec (show)
  `(create
    ,@(if (show-header-p show) `(header ,(show-header-p show)))
    ,@(if (show-toolbar-p show) `(toolbar ,(show-toolbar-p show)))
    ,@(if (show-footer-p show) `(footer ,(show-footer-p show)))
    ,@(if (not (show-column-headers-p show)) `(column-headers ,(show-column-headers-p show)))
    ,@(if (show-line-numbers-p show) `(line-numbers ,(show-line-numbers-p show)))
    ,@(if (show-expand-column-p show) `(expand-column ,(show-expand-column-p show)))
    ,@(if (show-select-column-p show) `(select-column ,(show-select-column-p show)))
    ,@(if (not (show-empty-records-p show)) `(empty-records ,(show-empty-records-p show)))
    ,@(if (not (show-toolbar-reload-p show)) `(toolbar-reload ,(show-toolbar-reload-p show)))
    ,@(if (not (show-toolbar-columns-p show)) `(toolbar-columns ,(show-toolbar-columns-p show)))
    ,@(if (not (show-toolbar-search-p show)) `(toolbar-search ,(show-toolbar-search-p show)))
    ,@(if (not (show-toolbar-add-p show)) `(toolbar-add ,(show-toolbar-add-p show)))
    ,@(if (not (show-toolbar-edit-p show)) `(toolbar-edit ,(show-toolbar-edit-p show)))
    ,@(if (not (show-toolbar-delete-p show)) `(toolbar-delete ,(show-toolbar-delete-p show)))
    ,@(if (not (show-toolbar-save-p show)) `(toolbar-save ,(show-toolbar-save-p show)))
    ,@(if (not (show-selection-border-p show)) `(selection-border ,(show-selection-border-p show)))
    ,@(if (not (show-record-titles-p show)) `(record-titles ,(show-record-titles-p show)))
    ,@(if (not (show-skip-records-p show)) `(skip-records ,(show-skip-records-p show)))))

(defun record-spec (columns record i)
  (let ((fields (mapcar #'column-field columns)))
    (reduce #'append (cons `(create recid ,i) (mapcar #'list fields record)))))

(defun records-spec (grid)
  (let ((records (grid-records grid))
	(columns (grid-columns grid)))
    (cons 'list
	  (loop for i from 1 to (length records)
		for r in records
		collect (record-spec columns r i)))))

(defstruct grid
  (:element-id nil :type string)
  (:columns nil :type list)  ; List of column objects.
  (:header nil :type (or null string)) ; The header of the grid.
  (:url nil :type (or null string))       ; URL to the remote data source.
  (:method nil :type (or null string))    ; Overwrites method for ajax requests.
  :toolbar         ; Toolbar for the grid.
  :searches
  :sort-data       ; Array of sort objects (submitted to data source for record sorting).
  :records
  :enable-keyboard-p ; Indicates if grid should listen to keyboard.
  :show
  :multi-search
  :multi-select
  :multi-sort

  ;; Events
  :on-click
  :on-dbl-click
  :on-select
  :on-unselect
  :on-expand
  :on-add
  :on-delete
  )

;; autoLoad       ; Indicates if the records should be loaded from the server automatically as user scrolls.
;; buttons        ; Object that contains default toolbar items
;; column-groups  ; Array of column group objects.
;; context-menu   ; Displays context menu under specified record.
;; fixed-body     ; Indicates if the body of the grid is of fixed height.
;; get-cell-value ; Returns parse value for the cell.
;; last           ; Internal grid's vairables.
;; limit          ; Number of records to return from remote data source per attempt.
;; mark-search    ; Indicates if result of the search should be highlighted.
;; menu           ; Array of object for context menu.
;; msgAJAXerror   ; Error message when server returns undefined error.
;; msgDelete      ; Confirmation message when user clicks the delete button.
;; msgNotJSON     ; Error message when server does not return JSON structure.
;; msgRefresh     ; Message that appears when grid refreshes.
;; multiSearch    ; Indicates if multi field search is allowed.
;; multiSelect    ; Indicates if record multi select is allowed.
;; multiSort      ; Indicates if column multi sort is allowed.
;; offset         ; Number of records to skip when retriving records from remote source.
;; parser         ; Function to parse server response.
;; postData       ; Map of additional parameter to submit to remove data source.
;; ranges ; Array of all ranges defined for the grid.
;; recid ; Name for the recid field in the records array.
;; recordHeight ; Height of the record.
;; records ; Array of record objects.
;; reorderColumns ; Indicates if reordering of columns is allowed.
;; reorderRows ; Indicates if reordering of rows is allowed.
;; resizeBoxes ; Called to resize grid's elements.
;; resizeRecords ; Called to resize grid's records.
;; routeData ; Object with data for the route.
;; searchData ; Array of search objects (submitted to data source for record filtering).
;; searches ; Array of search objects.
;; selectType ; Defines selection type.
;; show ; Map of indicators which elements of the grid are visible.
;; sortData ; Array of sort objects (submitted to data source for record sorting).
;; summary ; Summary records that displayed on the bottom
;; total ; Total number of records.

(defun grid (element-id columns &key header url method searches sort-data records enable-keyboard-p multi-search multi-select multi-sort
                                  show on-click on-dbl-click on-select on-unselect on-expand on-add on-delete)
  (make-grid :element-id element-id
	     :columns columns :header header :url url :method method
	     :searches searches :sort-data sort-data :records records :enable-keyboard-p enable-keyboard-p
             :multi-search multi-search :multi-select multi-select :multi-sort multi-sort
	     :show show
	     :on-click on-click :on-dbl-click on-dbl-click :on-select on-select :on-unselect on-unselect
             :on-expand on-expand :on-add on-add :on-delete on-delete))

(defun grid-spec (grid)
  `((chain ($ ,(cat "#" (grid-element-id grid))) w2grid)
    (create
     name ,(grid-element-id grid)
     ,@(if (grid-columns grid) `(columns (list ,@(mapcar #'column-spec (grid-columns grid)))))
     ,@(if (grid-header grid) `(header ,(grid-header grid)))
     ,@(if (grid-url grid) `(url ,(grid-url grid)))
     ,@(if (grid-method grid) `(method ,(grid-method grid)))
     ,@(if (grid-searches grid) `(searches ,(grid-searches grid)))
     ,@(if (grid-sort-data grid) `(sort-data ,(grid-sort-data grid)))
     ,@(if (grid-records grid) `(records ,(records-spec grid)))
     ,@(if (grid-enable-keyboard-p grid) `(keyboard ,(grid-enable-keyboard-p grid)))
     ,@(if (grid-multi-search grid) `(multi-search ,(grid-multi-search grid)))
     ,@(if (grid-multi-select grid) `(multi-select ,(grid-multi-select grid)))
     ,@(if (grid-multi-sort grid) `(multi-sort ,(grid-multi-sort grid)))
     ,@(if (grid-records grid) `(records ,(records-spec grid)))
     ,@(if (grid-records grid) `(records ,(records-spec grid)))
     ,@(if (grid-show grid) `(show ,(show-spec (grid-show grid))))
     ,@(if (grid-on-click grid) `(on-click ,(grid-on-click grid)))
     ,@(if (grid-on-dbl-click grid) `(on-dbl-click ,(grid-on-dbl-click grid)))
     ,@(if (grid-on-select grid) `(on-select ,(grid-on-select grid)))
     ,@(if (grid-on-unselect grid) `(on-unselect ,(grid-on-unselect grid)))
     ,@(if (grid-on-expand grid) `(on-expand ,(grid-on-expand grid)))
     ,@(if (grid-on-add grid) `(on-add ,(grid-on-add grid)))
     ,@(if (grid-on-delete grid) `(on-delete ,(grid-on-delete grid)))
     )))

;;; Toolbar

(defstruct toolbar-item
  :id        ; id of the item
  :type      ; type of the item (button, check, radio, drop, menu, break, spacer, html)
  :text      ; caption of the item
  :route     ; route to follow, can have dynamic parts as /item/:id/details (see routeData)
  :html      ; html text for the item (only if type = html)
  :img       ; css class of the image for the item
  :icon      ; css class of the icon font for the item
  :hidden    ; indicates if item is hidden
  :disabled  ; indicates if item is disabled
  :checked   ; indicates if item is checked
  :arrow     ; down arrow for drop/menu types
  :hint      ; hint text for the item
  :group     ; group name for the item, used for radio buttons, can be integer or string
  :sub-items ; for type=menu it is an array of items in the menu
  :overlay   ; for drop down menus additional params for overlay, see overlay
  :on-click  ; mouse click event handler
  :count     ; count of the item (number which will be side of the item)
  )

(defun item (id &key type text route html img icon hidden disabled checked arrow hint group sub-items overlay on-click count)
  (make-toolbar-item :id id :type type :text text :route route :html html :img img :icon icon :hidden hidden :disabled disabled :checked checked :arrow arrow :hint hint :group group :sub-items sub-items :overlay overlay :on-click on-click :count count))

(defun toolbar-item-spec (item)
  `(create ,@(if (toolbar-item-id item) `(id ,(toolbar-item-id item)))
	   ,@(if (toolbar-item-type item) `(type ,(toolbar-item-type item)))
	   ,@(if (toolbar-item-text item) `(text ,(toolbar-item-text item)))
	   ,@(if (toolbar-item-route item) `(route ,(toolbar-item-route item)))
	   ,@(if (toolbar-item-html item) `(html ,(toolbar-item-html item)))
	   ,@(if (toolbar-item-img item) `(img ,(toolbar-item-img item)))
	   ,@(if (toolbar-item-icon item) `(icon ,(toolbar-item-icon item)))
           ,@(if (toolbar-item-hidden item) `(hidden ,(toolbar-item-hidden item)))
	   ,@(if (toolbar-item-disabled item) `(disabled ,(toolbar-item-disabled item)))
	   ,@(if (toolbar-item-checked item) `(checked ,(toolbar-item-checked item)))
	   ,@(if (toolbar-item-arrow item) `(arrow ,(toolbar-item-arrow item)))
	   ,@(if (toolbar-item-hint item) `(hint ,(toolbar-item-hint item)))
	   ,@(if (toolbar-item-group item) `(group ,(toolbar-item-group item)))
	   ,@(if (toolbar-item-sub-items item) `(items ,(cons 'list (mapcar #'toolbar-sub-item-spec (toolbar-item-sub-items item)))))
	   ,@(if (toolbar-item-overlay item) `(overlay ,(toolbar-item-overlay item)))
	   ,@(if (toolbar-item-on-click item) `(on-click ,(toolbar-item-on-click item)))
	   ,@(if (toolbar-item-count item) `(count ,(toolbar-item-count item)))))

(defstruct toolbar-sub-item
  :id        ; id of the item
  :type      ; type of the item (button, check, radio, drop, menu, break, spacer, html)
  :text      ; caption of the item
  :img       ; css class of the image for the item
  :icon      ; css class of the icon font for the item
  :disabled  ; indicates if item is disabled
  :on-click  ; mouse click event handler
  )

(defun sub-item (id &key type text img icon disabled on-click)
  (make-toolbar-sub-item :id id :type type :text text :img img :icon icon :disabled disabled :on-click on-click))

(defun toolbar-sub-item-spec (item)
  `(create ,@(if (toolbar-sub-item-id item) `(id ,(toolbar-sub-item-id item)))
	   ,@(if (toolbar-sub-item-type item) `(type ,(toolbar-sub-item-type item)))
	   ,@(if (toolbar-sub-item-text item) `(text ,(toolbar-sub-item-text item)))
	   ,@(if (toolbar-sub-item-img item) `(img ,(toolbar-sub-item-img item)))
	   ,@(if (toolbar-sub-item-icon item) `(icon ,(toolbar-sub-item-icon item)))
	   ,@(if (toolbar-sub-item-disabled item) `(disabled ,(toolbar-sub-item-disabled item)))))

(defstruct toolbar
  (:element-id nil :type string)
  (:items nil :type list))

;; (defstruct toolbar
;;   :items     ; Array,  deault = [],   Array of the item objects.
;;   :right     ; String, default = '',  Defines HTML in the right corner of the toolbar.
;;   :routeData ; String, default = '',  Object with data for the route.
;;   ;; Common Properties
;;   :box       ; DOM Element, default = null, The DOM element where to render the object.
;;   :handlers  ; Array, default = [], Array of event handlers.
;;   :name      ; String, default = '', Unique name for the object.
;;   :style     ; String, default = '', Additional style for the .box where the object is rendered.
;;   )

(defun toolbar (element-id items)
  (make-toolbar :element-id element-id :items items))

(defun element-id-list-of-sub-items-which-have-on-click (toolbar)
  (let ((items-have-sub-items (remove-if-not (lambda (item) (toolbar-item-sub-items item))
					     (toolbar-items toolbar))))
    (flatten
     (mapcar (lambda (item)
	       (if (toolbar-item-sub-items item)
		 (mapcar (lambda (sub-item)
			   (if (toolbar-sub-item-on-click sub-item)
			     (cat (toolbar-item-id item) ":" (toolbar-sub-item-id sub-item))))
			 (toolbar-item-sub-items item))))
	     items-have-sub-items))))

(defun make-event-listener-from-toolbar-sub-items (toolbar)
  (let ((items-have-sub-items (remove-if-not (lambda (item) (toolbar-item-sub-items item))
					     (toolbar-items toolbar))))
    (if items-have-sub-items
      (let ((on-click-lambda-forms
	     (mapcar #'toolbar-sub-item-on-click
		     (remove-if-not (lambda (sub-item)
				      (toolbar-sub-item-on-click sub-item))
				    (apply #'append (mapcar #'toolbar-item-sub-items
							    items-have-sub-items))))))
	(let* ((funcall-lambda-form-list
		(mapcar (lambda (lambda-form)
			  (cond ((null (cadr lambda-form)) 
				 `(funcall ,lambda-form))
				((= (length (cadr lambda-form)) 1)
				 `(funcall ,lambda-form event))))
			on-click-lambda-forms))
	       (element-id-list (element-id-list-of-sub-items-which-have-on-click toolbar))
	       (cond-clause-list
		(mapcar (lambda (funcall-lambda-form element-id)
			  `((= (chain event target) ,element-id)  ,funcall-lambda-form))
			funcall-lambda-form-list element-id-list)))
	  `(lambda (event) (cond ,@cond-clause-list)))))))

(defun toolbar-spec (toolbar)
  `(progn
     ;; create toolbar
     ((chain ($ ,(cat "#" (toolbar-element-id toolbar))) w2toolbar)
      (create name  ,(toolbar-element-id toolbar)
	      items ,(cons 'list (mapcar #'toolbar-item-spec (toolbar-items toolbar)))))
     ;; define event listener for sub-items
     ,@(if (make-event-listener-from-toolbar-sub-items toolbar)
	 `(((chain (aref w2ui ,(toolbar-element-id toolbar)) on) "click"
	    ,(make-event-listener-from-toolbar-sub-items toolbar))))))

;;; Form

(defstruct form-html
  :caption  ; caption for the field
  :attr     ; attributes inside input/select control
  :text     ; text to display on the right from the control
  :span     ; number of span, will be added to class="w2ui-span{span}"
  :page     ; page number if there are tabs in the form
  )

(defun form-html-spec (form-html)
  `(create
    ,@(if (form-html-caption form-html) `(caption ,(form-html-caption form-html)))
    ,@(if (form-html-attr form-html) `(attr ,(form-html-attr form-html)))
    ,@(if (form-html-text form-html) `(text ,(form-html-text form-html)))
    ,@(if (form-html-span form-html) `(span ,(form-html-span form-html)))
    ,@(if (form-html-page form-html) `(page ,(form-html-page form-html)))))

(defstruct form-field-options
  :min                ; min value
  :max                ; max value
  :step               ; step to icrement with keyboard
  :placeholder        ; placeholder if empty
  :auto-format        ; indicates if to auto format the number
  :currency-prefix    ; symbol for currency prefix
  :currency-suffix    ; symbol for currency suffix
  :currency-precision ; defines precision, default = 2  
  :group-symbol       ; symbol for number grouping
  :arrows             ; indicates if to display arrows on the right side
  :enable-keyboard-p  ; indicates if keyboard should be supported
  :precision          ; defines precision for auto format
  :silent             ; indicate if to correct silently or display error tag
  :prefix             ; control prefix
  :suffix             ; control suffix
  )

(defun form-field-options-spec (form-field-options)
  `(create
    ,@(if (form-field-options-min form-field-options) `(min ,(form-field-options-min form-field-options)))
    ,@(if (form-field-options-max form-field-options) `(max ,(form-field-options-max form-field-options)))
    ,@(if (form-field-options-step form-field-options) `(step ,(form-field-options-step form-field-options)))
    ,@(if (form-field-options-placeholder form-field-options) `(placeholder ,(form-field-options-placeholder form-field-options)))
    ,@(if (form-field-options-auto-format form-field-options) `(auto-format ,(form-field-options-auto-format form-field-options)))
    ,@(if (form-field-options-currency-prefix form-field-options) `(currency-prefix ,(form-field-options-currency-prefix form-field-options)))
    ,@(if (form-field-options-currency-suffix form-field-options) `(currency-suffix ,(form-field-options-currency-suffix form-field-options)))
    ,@(if (form-field-options-currency-precision form-field-options) `(currency-precision ,(form-field-options-currency-precision form-field-options)))
    ,@(if (form-field-options-group-symbol form-field-options) `(group-symbol ,(form-field-options-group-symbol form-field-options)))
    ,@(if (form-field-options-arrows form-field-options) `(arrows ,(form-field-options-arrows form-field-options)))
    ,@(if (form-field-options-enable-keyboard-p form-field-options) `(keyboard ,(form-field-options-enable-keyboard-p form-field-options)))
    ,@(if (form-field-options-precision form-field-options) `(precision ,(form-field-options-precision form-field-options)))
    ,@(if (form-field-options-silent form-field-options) `(silent ,(form-field-options-silent form-field-options)))
    ,@(if (form-field-options-prefix form-field-options) `(prefix ,(form-field-options-prefix form-field-options)))
    ,@(if (form-field-options-suffix form-field-options) `(suffix ,(form-field-options-suffix form-field-options)))))

(defstruct form-field
  :name     ; name of field, must match name of input or select 
  :type     ; type of field (same types as for w2field object)
  :options  ; additional options for the field
  :required ; required or not
  :html     ; info for auto generation (see generateHTML)
  )

(defun form-field-spec (form-field)
  `(create
    ,@(if (form-field-name form-field) `(name ,(form-field-name form-field)))
    ,@(if (form-field-type form-field) `(type ,(form-field-type form-field)))
    ,@(if (form-field-options form-field) `(options ,(form-field-options-spec (form-field-options form-field))))
    ,@(if (form-field-required form-field) `(required ,(form-field-required form-field)))
    ,@(if (form-field-html form-field) `(html ,(form-field-spec (form-field-html form-field))))))

;; Supported types
;;   text
;;   textarea
;;   email
;;   pass
;;   password
;;   int
;;   float
;;   money
;;   currency (alias for money)
;;   percent
;;   hex
;;   alphanumeric
;;   color
;;   date
;;   time
;;   list
;;   combo
;;   enum
;;   file
;;   select
;;   radio
;;   checkbox
;;   toggle

(defun field (name type &key options required html)
  (make-form-field :name name :type type :options options :required required :html html))

(defstruct form-action :name :action)

(defun form-action-spec (form-action)
  `(,(form-action-name form-action) ,(form-action-action form-action)))

(defun action (name action) (make-form-action :name name :action action))

(defstruct form
  (:element-id nil :type string)
  (:fields nil :type list)
  (:actions nil :type list)
  :post-url)

(defun form (element-id fields actions &key post-url)
  (make-form :element-id element-id :fields fields :actions actions :post-url post-url))

(defun form-spec (form)
  `((chain ($ ,(cat "#" (form-element-id form))) w2form)
    (create name    ,(form-element-id form)
	    url     ,(form-post-url form)
	    fields  ,(cons 'list (mapcar #'form-field-spec (form-fields form)))
	    actions (create ,@(apply #'append (mapcar #'form-action-spec (form-actions form)))))))

;;; Popup

(defstruct popup
  :title       ; html for title - empty by default
  :body        ; html for body - empty by default
  :buttons     ; html for buttons - empty by default
  :width       ; width of the popup
  :height      ; height of the popup
  :style       ; additional styles
  :color       ; color of the screen lock
  :opacity     ; opacity of the screen lock
  :speed       ; speed popup appears
  :modal       ; if modal, it cannot be closed by clicking on the screen lock
  :maximized   ; by default it is not maximized
  :enable-keyboard-p ; will close popup on esc if not modal
  :show-close  ; show closed button by default
  :show-max    ; do not show max button by default
  :transition  ; no content transition by default
  ;; Events
  :on-close    ; called when popup is closed
  :on-keydown  ; called when user presses a keyboard key and popup is open
  :on-max      ; called when popup is maximized
  :on-min      ; called when popup is minimized
  :on-open     ; called when popup is opened or transitions to the new content
  :on-toggle   ; called when popup is maximized or minimized
  )

(defun popup-spec (popup)
  `(create
    ,@(if (popup-title popup) `(title ,(popup-title popup)))
    ,@(if (popup-body popup) `(body ,(popup-body popup)))
    ,@(if (popup-buttons popup) `(buttons ,(popup-buttons popup)))
    ,@(if (popup-width popup) `(width ,(popup-width popup)))
    ,@(if (popup-height popup) `(height ,(popup-height popup)))
    ,@(if (popup-style popup) `(style ,(popup-style popup)))
    ,@(if (popup-color popup) `(color ,(popup-color popup)))
    ,@(if (popup-opacity popup) `(opacity ,(popup-opacity popup)))
    ,@(if (popup-speed popup) `(speed ,(popup-speed popup)))
    ,@(if (popup-modal popup) `(modal ,(popup-modal popup)))
    ,@(if (popup-maximized popup) `(maximized ,(popup-maximized popup)))
    ,@(if (popup-enable-keyboard-p popup) `(keyboard ,(popup-enable-keyboard-p popup)))
    ,@(if (popup-show-close popup) `(show-close ,(popup-show-close popup)))
    ,@(if (popup-show-max popup) `(show-max ,(popup-show-max popup)))
    ,@(if (popup-transition popup) `(transition ,(popup-transition popup)))    
    ,@(if (popup-on-close popup) `(on-close ,(popup-on-close popup)))
    ,@(if (popup-on-keydown popup) `(on-keydown ,(popup-on-keydown popup)))
    ,@(if (popup-on-max popup) `(on-max ,(popup-on-max popup)))
    ,@(if (popup-on-min popup) `(on-min ,(popup-on-min popup)))
    ,@(if (popup-on-open popup) `(on-open ,(popup-on-open popup)))
    ,@(if (popup-on-toggle popup) `(on-toggle ,(popup-on-toggle popup)))
    ))

(defun popup (&key title body buttons width height style color opacity speed modal maximized enable-keyboard-p show-close show-max transition on-close on-keydown on-max on-min on-open on-toggle)
  (make-popup :title title :body body :buttons buttons :width width :height height :style style :color color :opacity opacity :speed speed :modal modal :maximized maximized :enable-keyboard-p enable-keyboard-p :show-close show-close :show-max show-max :transition transition :on-close on-close :on-keydown on-keydown :on-max on-max :on-min on-min :on-open on-open :on-toggle on-toggle))

(defun popup-open (popup)
  `((chain w2popup open) ,(popup-spec popup)))

(defun popup-close ()
  `((chain w2popup close)))

;;; Tabs

(defstruct tab
  :id          ; command to be sent to all event handlers
  :text        ; tab caption
  :hidden      ; defines if tab is hidden
  :disabled    ; defines if tab is disabled
  :closable    ; defines if tab is closable
  :hint        ; mouse hint for the tab
  :on-click    ; click event handler (only this tab)
  :on-refresh  ; refresh event handler (only this tab)
  :on-close    ; close event handler (only this tab)
  )

(defun tab (id text &key hidden disabled closable hint on-click on-refresh on-close)
  (make-tab :id id :text text :hidden hidden :disabled disabled :closable closable
	    :hint hint :on-click on-click :on-refresh on-refresh :on-close on-close))

(defun tab-spec (tab)
  `(create
    ,@(if (tab-id tab) `(id ,(tab-id tab)))
    ,@(if (tab-text tab) `(text ,(tab-text tab)))
    ,@(if (tab-hidden tab) `(hidden ,(tab-hidden tab)))
    ,@(if (tab-disabled tab) `(disabled ,(tab-disabled tab)))
    ,@(if (tab-closable tab) `(closable ,(tab-closable tab)))
    ,@(if (tab-hint tab) `(hint ,(tab-hint tab)))
    ,@(if (tab-on-click tab) `(on-click ,(tab-on-click tab)))
    ,@(if (tab-on-refresh tab) `(on-refresh ,(tab-on-refresh tab)))
    ,@(if (tab-on-close tab) `(on-close ,(tab-on-close tab)))))

(defstruct tabs
  (:element-id nil :type string)
  :active     ; String, default = ''  Id of the active tab.
  :tab-list   ; List of tab lisp object
  )

(defun tabs (element-id tab-list &key active)
  (assert (>= (length tab-list) 1))
  (assert (null (member-if (lambda (x) (not (eq (type-of x) 'tab))) tab-list)))
  (make-tabs :element-id element-id :tab-list tab-list :active active))

(defun tabs-spec (tabs)
  `((chain ($ ,(cat "#" (tabs-element-id tabs))) w2tabs)
    (create name ,(tabs-element-id tabs)
	    ,@(if (tabs-active tabs) `(active ,(tabs-active tabs)))
	    tabs ,(cons 'list (mapcar #'tab-spec (tabs-tab-list tabs)))
	    )))

(defun tab-set (tab-body-element-id body)
  `(lambda () ((chain ($ ,(cat "#" tab-body-element-id)) html) ,body)))
