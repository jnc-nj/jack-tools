(in-package #:jack.tools.bootstraps)

(defmacro bs-btn ((&key (type "default") (size nil)) &body body)
  `(with-html-output (*standard-output*)
     (:button :type "button" :class
	      (concatenate 'string "btn"
			   (if ,type (format nil " btn-~a" ,type))
			   (if ,size (format nil " btn-~a" ,size)))
              ,@body)))

(defmacro bs-btn-lg ((&key (type "default")) &body body)
  `(with-html-output (*standard-output*)
     (:button :type "button" :class (format nil "btn btn-lg btn-~a" ,type)
              ,@body)))

(defmacro bs-btn-sm ((&key (type "default")) &body body)
  `(with-html-output (*standard-output*)
     (:button :type "button" :class (format nil "btn btn-sm btn-~a" ,type)
              ,@body)))

(defmacro bs-btn-xs ((&key (type "default")) &body body)
  `(with-html-output (*standard-output*)
     (:button :type "button" :class (format nil "btn btn-xs btn-~a" ,type)
              ,@body)))


(defmacro bs-btn-default (&body body)
  `(bs-btn () ,@body))

(defmacro bs-btn-primary (&body body)
  `(bs-btn (:type "primary") ,@body))

(defmacro bs-btn-success (&body body)
  `(bs-btn (:type "success") ,@body))

(defmacro bs-btn-info (&body body)
  `(bs-btn (:type "info") ,@body))

(defmacro bs-btn-warning (&body body)
  `(bs-btn (:type "warning") ,@body))

(defmacro bs-btn-danger (&body body)
  `(bs-btn (:type "danger") ,@body))

(defmacro bs-link-btn ((&key (type "default") (size nil) (href "#")) &body body)
  `(with-html-output (*standard-output*)
     (:a :role "button"
	 :class (concatenate 'string "btn"
			     (if ,type (format nil " btn-~a" ,type))
			     (if ,size (format nil " btn-~a" ,size)))
	 :href ,href
	 ,@body)))

(defmacro bs-btn-dropdown ((&key (title "")) &body body)
  "Turn a button into a dropdown toggle with some basic markup changes."
  `(with-html-output (*standard-output*)
     (:div :class "btn-group"
           (:button :type "button"
                    :class "btn btn-default dropdown-toggle"
                    :data-toggle "dropdown"
                    :aria-haspopup "true"
                    :aria-expanded "false"
                    ,title
                    "&nbsp;" (:span :class "caret"))
           (:ul :class "dropdown-menu" ,@body))))

(defmacro bs-form-text ((&key (label "Text: ") (id nil) (name nil)))
  `(bs-form-input "text" (:label ,label :id ,id :name ,name)))

(defmacro bs-form-embd-static ((&key label id name placeholder value))
  (let ((g-id (gensym))
	(g-name (gensym))
	(g-placeholder (gensym)))
    `(let ((,g-id ,id)
	   (,g-name ,name)
	   (,g-placeholder ,placeholder))
       (list :div :class "form-group"
	     (list :label :for ,g-id ,label)
	     (list :input :type "text" :readonly t :class "form-control-plaintext"
		   :id ,g-id :value ,value
		   ,@(when g-name `(:name ,g-name))
		   ,@(when g-placeholder `(:placeholder ,g-placeholder)))))))

(defmacro bs-form-embd-checkbox (&body body)
  `(list :div :class "checkbox"
	 (list :label (list :input :type "checkbox" ,@body))))

(defmacro bs-embd-modal ((&key (title "")) &body body)
  `(list :div :class "modal fade" :id "myModal" :tabindex "-1" :role "dialog"
         (list :div :class "modal-dialog" :role "document"
               (list :div :class "modal-content"
		     (list :div :class "modal-header"
                           (list :button :type "button" :class "close"
				 :data-dismiss "modal" :aria-label "Close"
				 (list :span :aria-hidden "true" "&times;"))
                           (list :h4 :class "modal-title" ,title))
		     (list :div :class "modal-body" ,@body)
		     (list :div :class "modal-footer"
                           (list :button :type "button" :class "btn btn-default"
				 :data-dismiss "modal" "Close")
			   (list :button :type "button" :class "btn btn-primary" "Save"))))))

(defmacro bs-embd-table (&body body)
  `(list :table :class "table",@body))
