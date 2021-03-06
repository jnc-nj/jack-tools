(in-package #:jack.tools.threads)

(defun count-threads (key)
  "Count threads matching key."
  (let ((count 0))
    (dolist (thread (bt:all-threads))
      (when (search key (bt:thread-name thread))
	(incf count)))
    count))

(defun connect-client (client-name)
  (handler-case (join-thread client-name)
    (sb-sys:interactive-interrupt () (sb-ext:exit))
    (error () nil)))

(defun join-thread (thread-name)
  (bt:join-thread
   (find-thread thread-name)))

(defun destroy-thread (&rest thread-names)
  (dolist (thread-name thread-names)
    (let ((thread (find-thread thread-name)))
      (when thread (bt:destroy-thread thread)))))

(defun find-thread (thread-name)
  (find-if #'(lambda (thread) (search thread-name (bt:thread-name thread)))
           (bt:all-threads)))

(defun all-thread-names ()
  (mapcar #'bt:thread-name (bt:all-threads)))
