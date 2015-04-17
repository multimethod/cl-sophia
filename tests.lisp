(in-package #:cl-user)

(defpackage #:cl-sophia-test
  (:use #:cl
        #:lisp-unit
        #:cl-sophia)
  (:import-from #:alexandria
                #:with-unique-names
                #:once-only)

  (:import-from #:cl-utilities
                #:collecting
                #:collect))

(in-package #:cl-sophia-test)

(defparameter *prefix* (user-homedir-pathname))

(defun create-temp-directory (&optional (prefix *prefix*))
  (let ((random-string (cl-fad::generate-random-string)))
    (let ((temp-path (merge-pathnames (concatenate 'string random-string "/") prefix)))
      (when (cl-fad:directory-exists-p temp-path)
        (error "Directory ~a already exists" temp-path))
      (ensure-directories-exist temp-path))))

(defmacro with-temp-directory ((name) &body body)
  `(let* ((,name (create-temp-directory))
          (*prefix* ,name))
     (unwind-protect
          (progn
            ,@body)
       (cl-fad:delete-directory-and-files ,name))))

(defmacro with-temp-sophia-directory (() &body body)
  (with-unique-names (temp)
    `(with-temp-directory (,temp)
       (let ((sophia:*path* (namestring ,temp)))
         ,@body))))


(define-test set-get
  (with-temp-sophia-directory ()
    (with-database ("test")
      (assert-equal "bar" (setf ($ "foo") "bar"))
      (assert-equal "bar" ($ "foo")))))

(define-test delete
  (with-temp-sophia-directory ()
    (with-database ("test")
      (setf ($ "x") "a"
            ($ "y") "b")
      (assert-equal "a" ($ "x"))
      (assert-equal "b" ($ "y"))
      (assert-false (setf ($ "x") nil))
      (assert-false (setf ($ "y") nil))
      (assert-false ($ "x"))
      (assert-false ($ "y")))))



(defun run ()
  (let ((*print-summary* t))
    (with-temp-directory (temp)
      (run-tests :all :cl-sophia-test))))
