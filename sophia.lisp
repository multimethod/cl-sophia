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

(defmacro with-object-field ((object key value) &body body)
  (with-unique-names (fvalue fvalue-size)
    (once-only (object key value)
      `(with-foreign-string ((,fvalue ,fvalue-size) ,value)
         (check-retcode (sp-set ,object :string ,key :pointer ,fvalue :int ,fvalue-size))
         ,@body))))

(defmacro with-object-foreign-type-field ((foreign-type object key value) &body body)
  (with-unique-names (fvalue)
    (once-only (foreign-type object key value)
      `(with-foreign-object (,fvalue ,foreign-type)
         (setf (mem-ref ,fvalue ,foreign-type) ,value)
         (check-retcode (sp-set ,object :string ,key :pointer ,fvalue :int (foreign-type-size ,foreign-type)))
         ,@body))))

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

(defun get-object-field (obj key)
  (foreign-string-to-lisp (check-pointer (sp-get obj :string key :pointer (null-pointer)))))

(defun get-object-foreign-type-field (foreign-type obj key)
  (mem-ref (check-pointer (sp-get obj :string key :pointer (null-pointer)))
           foreign-type))

(defun get-object (db ctx key)
  (with-slots (dbh cmp) db
    (flet ((get% (object)
             (let ((result (sp-get ctx :pointer object)))
               (unless (null-pointer-p result)
                 (unwind-protect
                      (get-object-field result "value")
                   (sp-destroy result))))))
      (let ((object (sp-object dbh)))
        (ecase cmp
          (:string
           (check-type key string)
           (with-object-field (object "key" key)
             (get% object)))
          (:u32
           (check-type key (unsigned-byte 32))
           (with-object-foreign-type-field (:uint32 object "key" key)
             (get% object))))))))

(defun $ (key &optional (db *db*))
  (check-type db db)
  (let ((ctx (or *ctx* (slot-value db 'dbh))))
    (get-object db ctx key)))


(defun set-object (db ctx key value)
  (check-type value (or null string))
  (with-slots (dbh cmp) db
    (flet ((set-or-delete% (object)
             (if value
                 (with-object-field (object "value" value)
                   (check-retcode (sp-set ctx :pointer object)))
                 (check-retcode (sp-delete ctx object)))))
      (let ((object (sp-object dbh)))
        (ecase cmp
          (:string
           (check-type key string)
           (with-object-field (object "key" key)
             (set-or-delete% object)))
          (:u32
           (check-type key (unsigned-byte 32))
           (with-object-foreign-type-field (:uint32 object "key" key)
             (set-or-delete% object))))))
    (values)))

(defun (setf $) (value key &optional (db *db*))
  (check-type db db)
  (let ((ctx (or *ctx* (slot-value db 'dbh))))
    (set-object db ctx key value))
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

(defmacro with-named-database ((&whole clause name dbname &rest settings) &body body)
  (declare (ignore name dbname settings))
  `(with-env ()
     (with-db ,clause
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

(define-condition commit-error (error)
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
              (error 'commit-error :format-control "Transaction locked")
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
                                  (case cmp
                                    (:string
                                     (get-object-field iterate-object "key"))
                                    (:u32
                                     (get-object-foreign-type-field :uint32 iterate-object "key")))
                                  (get-object-field iterate-object "value"))
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
