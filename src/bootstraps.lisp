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
