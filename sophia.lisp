;;;
;;; Copyright (c) 2015 Andrey V. Tikhonov <multimethod@yandex.ru>
;;;
;;; This program is free software. It comes without any warranty, to
;;; the extent permitted by applicable law. You can redistribute it
;;; and/or modify it under the terms of the Do What The Fuck You Want
;;; To Public License, Version 2, as published by Sam Hocevar. See
;;; http://www.wtfpl.net/ for more details.
;;;

(in-package #:cl-sophia)

(define-condition internal-error (simple-error)
  ((retcode :initarg :retcode :initform nil)))

(defun check-retcode (retcode &rest arguments)
  (unless (= 0 retcode)
    (error 'internal-error
           :retcode retcode
           :format-control (car arguments)
           :format-arguments (cdr arguments))))

(defun check-pointer (pointer &rest arguments)
  (when (null-pointer-p pointer)
    (error 'internal-error
           :format-control (car arguments)
           :format-arguments (cdr arguments)))
  (values pointer))

(defgeneric capture-field (foreign-type object key value)
  )

(defgeneric release-field (foreign-type field)
  )

(defmethod capture-field (foreign-type object key value)
  (let ((fvalue (foreign-alloc foreign-type :initial-element value)))
    (check-retcode (sp-set object :string key :pointer fvalue :int (foreign-type-size foreign-type)))
    (values fvalue)))

(defmethod release-field (foreign-type field)
  (declare (ignore foreign-type))
  (foreign-free field)
  (values))

(defmethod capture-field ((foreign-type (eql :string)) object key value)
  (multiple-value-bind (fvalue fvalue-size) (foreign-string-alloc value)
    (check-retcode (sp-set object :string key :pointer fvalue :int fvalue-size))
    (values fvalue)))

(defmethod release-field ((foreign-type (eql :string)) field)
  (foreign-string-free field)
  (values))

(defun call-with-field (function foreign-type object key value)
  (let ((field (capture-field foreign-type object key value)))
    (unwind-protect
         (funcall function)
      (release-field foreign-type field))))

(defmacro with-field ((foreign-type object key value) &body body)
  `(call-with-field (lambda () ,@body)
                    ,foreign-type
                    ,object
                    ,key
                    ,value))

(defgeneric capture-key-field-by-comparator (comparator object value)
  )

(defmethod capture-key-field-by-comparator ((comparator (eql :string)) object value)
  (check-type value string)
  (values :string
          (capture-field :string object "key" value)))

(defmethod capture-key-field-by-comparator ((comparator (eql :u32)) object value)
  (check-type value (unsigned-byte 32))
  (values :uint32
          (capture-field :uint32 object "key" value)))

(defmethod capture-key-field-by-comparator ((comparator (eql :u64)) object value)
  (check-type value (unsigned-byte 64))
  (values :uint64
          (capture-field :uint64 object "key" value)))

(defun call-with-key-field-by-comparator (function comparator object value)
  (multiple-value-bind (foreign-type field)
      (capture-key-field-by-comparator comparator object value)
    (unwind-protect
         (funcall function)
      (release-field foreign-type field))))

(defmacro with-key-field-by-comparator ((comparator object value) &body body)
  `(call-with-key-field-by-comparator (lambda () ,@body)
                                      ,comparator
                                      ,object
                                      ,value))

(defgeneric get-field (foreign-type object key)
  )

(defmethod get-field (foreign-type object key)
  (check-type key string)
  (mem-ref (check-pointer (sp-get object :string key :pointer (null-pointer)))
           foreign-type))

(defmethod get-field ((foreign-type (eql :string)) object key)
  (check-type key string)
  (nth-value 0 (foreign-string-to-lisp (check-pointer (sp-get object :string key :pointer (null-pointer))))))

(defgeneric get-key-field (comparator object)
  )

(defmethod get-key-field ((comparator (eql :string)) object)
  (get-field :string object "key"))

(defmethod get-key-field ((comparator (eql :u32)) object)
  (get-field :uint32 object "key"))

(defmethod get-key-field ((comparator (eql :u64)) object)
  (get-field :uint64 object "key"))


(defclass db ()
  ((dbh :initarg :dbh :initform nil)
   (cmp :initarg :cmp :initform :string)))

(defparameter *env* nil)
(defparameter *ctl* nil)
(defparameter *ctx* nil)

(defparameter *db* nil)
(defparameter *dbnames* nil)

(defparameter *path* (namestring (merge-pathnames ".cl-sophia-storage/" (user-homedir-pathname))))

(defun init-env ()
  (check-pointer (sp-env)))

(defun open-env ()
  (check-retcode (sp-open *env*)))

(defun free-env ()
  (check-retcode (sp-destroy *env*)))

(defun init-ctl ()
  (check-pointer (sp-ctl *env*)))

(defun cfg (key)
  (let ((obj (sp-get *ctl* :string key)))
    (unless (null-pointer-p obj)
      (foreign-string-to-lisp (check-pointer (sp-get obj :string "value" :pointer (null-pointer)))))))

(defun (setf cfg) (value key)
  (check-retcode (sp-set *ctl* :string key :string value))
  value)

(defun get-dbnames ()
  (let ((dbnames-path (ensure-directories-exist (merge-pathnames ".dbnames" *path*))))
    (with-open-file (s dbnames-path :if-does-not-exist :create)
      (read s nil))))

(defun add-dbname (dbname)
  (let ((dbnames-path (ensure-directories-exist (merge-pathnames ".dbnames" *path*))))
    (let ((dbnames (with-open-file (s dbnames-path :if-does-not-exist :create)
                     (read s nil))))
      (unless (find dbname dbnames :test #'string=)
        (with-open-file (s dbnames-path :direction :output :if-exists :supersede)
          (write (append dbnames (list dbname)) :stream s))))))

(defun hack-init-dbs ()
  (let ((dbnames (get-dbnames)))
    (dolist (dbname dbnames dbnames)
      (setf (cfg "db") dbname))))

(deftype comparator ()
  `(member :string :u32 :u64))

(defun init-db (dbname &key (cmp :string))
  (check-type dbname string)
  (check-type cmp comparator)

  (unless (find dbname *dbnames* :test #'string=)
    (setf (cfg "db") dbname))

  (check-retcode (sp-set *ctl*
                         :string (format nil "db.~a.index.cmp" dbname)
                         :string (string-downcase (string cmp))
                         :pointer (null-pointer)))
  (prog1
      (make-instance 'db
                     :dbh (check-pointer (sp-get *ctl* :string (format nil "db.~a" dbname)))
                     :cmp cmp)
    (add-dbname dbname)))

(defun $ (key &optional (db *db*))
  (check-type db db)
  (with-slots (dbh cmp) db
    (let ((ctx (or *ctx* (slot-value db 'dbh))))
      (let ((object (sp-object dbh)))
        (with-key-field-by-comparator (cmp object key)
          (let ((result (sp-get ctx :pointer object)))
            (unless (null-pointer-p result)
              (unwind-protect
                   (get-field :string result "value")
                (check-retcode (sp-destroy result))))))))))

(defun (setf $) (value key &optional (db *db*))
  (check-type db db)
  (with-slots (dbh cmp) db
    (let ((ctx (or *ctx* (slot-value db 'dbh))))
      (let ((object (sp-object dbh)))
        (with-key-field-by-comparator (cmp object key)
          (if value
              (with-field (:string object "value" value)
                (check-retcode (sp-set ctx :pointer object)))
              (check-retcode (sp-delete ctx object)))))))
  (values value))

(defmacro with-env (() &body body)
  `(let ((*env* (init-env)))
     (unwind-protect
          (let ((*ctl* (init-ctl)))
            (setf (cfg "sophia.path") *path*)
            (let ((*dbnames* (hack-init-dbs)))
              (progn
                ,@body)))
       (free-env))))

(defmacro with-db ((name dbname &rest settings) &body body)
  `(let ((,name (init-db ,dbname ,@settings)))
     ,@body))

(defmacro with-dbs ((clause &rest clauses) &body body)
  `(with-db ,clause
     ,(if clauses
          `(with-dbs ,clauses
             ,@body)
          `(progn
             ,@body))))

(defmacro with-database ((dbname &rest settings) &body body)
  `(with-env ()
     (with-db (*db* ,dbname ,@settings)
       (open-env)
       ,@body)))

(defmacro with-named-databases (((&whole clause name dbname &rest settings) &rest clauses) &body body)
  (declare (ignore name dbname settings))
  `(with-env ()
     (with-dbs ,(cons clause clauses)
       (open-env)
       ,@body)))

(defun init-transaction ()
  (check-pointer (sp-begin *env*)))

(defun free-transaction (transaction)
  (check-retcode (sp-destroy transaction)))

(define-condition transaction-locked (error)
  ())

(defun recommit (c)
  (declare (ignore c))
  (invoke-restart 'recommit))

(defun rollback (c)
  (declare (ignore c))
  (invoke-restart 'rollback))

(defun commit-transction (transaction)
  (let ((retcode (sp-commit transaction)))
    (unless (= 0 retcode)
      (if (= 2 retcode)
          (restart-case
              (error 'transaction-locked)
            (recommit ()
              (commit-transction transaction))
            (rollback ()
              (free-transaction transaction)))
          (error 'internal-error :retcode retcode)))))

(defmacro with-transaction (() &body body)
  (with-unique-names (commited)
    `(let ((*ctx* (init-transaction)) ,commited)
       (unwind-protect
            (multiple-value-prog1
                (progn
                  ,@body)
              (commit-transction *ctx*)
              (setf ,commited t))
         (unless ,commited
           (free-transaction *ctx*))))))

(deftype order ()
  `(member :< :<= :> :>=))

(declaim (type order *order*))
(defparameter *order* :>=)

(defun set-order (obj order)
  (check-type order order)
  (check-retcode (sp-set obj :string "order" :string (string order))))

(defun init-db-iterator (db)
  (with-slots (dbh cmp) db
    (let ((ctx (or *ctx* dbh)))
      (let ((object (sp-object dbh)))
        (set-order object *order*)
        (let* ((iterator (check-pointer (sp-cursor ctx :pointer object)))
               (iterate-object (sp-get iterator :pointer object)))
          (values iterator
                  (lambda ()
                    (unless (null-pointer-p iterate-object)
                      (multiple-value-prog1
                          (values t
                                  (get-key-field cmp iterate-object)
                                  (get-field :string iterate-object "value"))
                        (setf iterate-object (sp-get iterator :pointer iterate-object)))))))))))

(defun free-db-iterator (iterator)
  (check-retcode (sp-destroy iterator)))

(defmacro with-database-iterator ((name &optional (db '*db*)) &body body)
  (with-unique-names (iterator fn)
    `(multiple-value-bind (,iterator ,fn) (init-db-iterator ,db)
       (flet ((,name () (funcall ,fn)))
         (unwind-protect
              (progn
                ,@body)
           (free-db-iterator ,iterator))))))

(defun map-object (function &optional (db *db*))
  (with-database-iterator (fn db)
    (loop (multiple-value-bind (exists key value) (fn)
            (unless exists
              (return))
            (funcall function key value)))))
