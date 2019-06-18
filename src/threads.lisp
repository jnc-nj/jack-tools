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
    (sb-sys:interactive-interrupt ()
      (sb-ext:exit))))

(defun join-thread (thread-name)
  (bt:join-thread
   (find-thread thread-name)))

(defun find-thread (thread-name)
  (find-if #'(lambda (thread) (search thread-name (bt:thread-name thread)))
           (bt:all-threads)))
