(in-package #:cl-user)

(defpackage #:cl-sophia-test
  (:use #:cl
        #:lisp-unit
        #:cl-sophia)
  (:import-from #:alexandria
                #:with-unique-names
                #:once-only
                #:iota
                #:curry)

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
      (assert-false ($ "foo"))
      (assert-equal "bar" (setf ($ "foo") "bar"))
      (assert-equal "bar" ($ "foo")))))

(define-test delete
  (with-temp-sophia-directory ()
    (with-database ("test")
      (assert-false ($ "x"))
      (assert-equal "a" (setf ($ "x") "a"))
      (assert-equal "a" ($ "x"))
      (assert-false (setf ($ "x") nil))
      (assert-false ($ "x")))))

(define-test multiple-databases
  (with-temp-sophia-directory ()
    (with-named-databases ((db-a "db-a")
                           (db-b "db-b"))
      (setf ($ "x" db-a) "foo"
            ($ "x" db-b) "bar")
      (assert-equal "foo" ($ "x" db-a))
      (assert-equal "bar" ($ "x" db-b))

      (setf ($ "y" db-a) "Y"
            ($ "z" db-b) "Z")
      
      (assert-false ($ "y" db-b))
      (assert-false ($ "z" db-a)))

    (with-database ("db-a")
      (assert-equal "foo" ($ "x"))
      (assert-false ($ "z")))
    
    (with-database ("db-b")
      (assert-equal "bar" ($ "x"))
      (assert-false ($ "y")))))

(define-test iterators
  (with-temp-sophia-directory ()
    (with-database ("test" :cmp :u32)
      (let* ((keys (iota 1000))
             (vals (mapcar (curry #'format nil "~r") keys)))
        (mapc (lambda (k v)
                (setf ($ k) v))
              keys
              vals)

        (dolist (*order* '(:>= :>))
          (let (dbkeys
                dbvals)
            (map-object (lambda (key val)
                          (push key dbkeys)
                          (push val dbvals)))
            (let ((dbkeys (nreverse dbkeys))
                  (dbvals (nreverse dbvals)))
              (assert-equal keys dbkeys)
              (assert-equal vals dbvals))))

        (dolist (*order* '(:<= :<))
          (let (dbkeys
                dbvals)
            (map-object (lambda (key val)
                          (push key dbkeys)
                          (push val dbvals)))
            (assert-equal keys dbkeys)
            (assert-equal vals dbvals)))))))

(defun run ()
  (let ((*print-summary* t))
    (with-temp-directory (temp)
      (run-tests :all :cl-sophia-test))))
