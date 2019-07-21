(in-package #:jack.tools.filesystems)

(defun create-directory (name)
  (ensure-directories-exist (pathname name))
  (truename name))

(defun write-file (object target)
  (with-open-file (file (pathname target)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (if (stringp object)
        (format file "~d" object)
        (format file "~d" (cl-json:encode-json-to-string object)))))

(defun open-file (target)
  (with-open-file (file (pathname target))
    (let ((contents (make-string (file-length file))))
      (read-sequence contents file)
      contents)))

(defun cmd-read-path (path)
  (format nil "$(<~d)" (pathname path)))

(defun *probe-file (path)
  (when path (probe-file path)))

(defun truenamep (path)
  (handler-case (pathname (truename path))
    (error () nil)))
