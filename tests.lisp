;;;
;;; Copyright (c) 2015 Andrey V. Tikhonov <multimethod@yandex.ru>
;;; This program is free software. It comes without any warranty, to
;;; the extent permitted by applicable law. You can redistribute it
;;; and/or modify it under the terms of the Do What The Fuck You Want
;;; To Public License, Version 2, as published by Sam Hocevar. See
;;; http://www.wtfpl.net/ for more details.
;;;

(in-package #:cl-user)

(defpackage #:cl-sophia-test
  (:use #:cl
        #:lisp-unit
        #:cl-sophia)
  (:import-from #:alexandria
                #:with-unique-names
                #:once-only
                #:iota
                #:curry))

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

(define-test comparators
  (with-temp-sophia-directory ()
    (macrolet ((expected-type (form)
                 `(handler-case
                      (progn
                        ,form
                        (values))
                    (type-error (e)
                      (type-error-expected-type e)))))
      (with-named-databases ((db-str "db-str" :cmp :string)
                             (db-u32 "db-u32" :cmp :u32)
                             (db-u64 "db-u64" :cmp :u64))
        ;; string
        (assert-false (expected-type ($ "foobar" db-str)))
        (assert-equal 'string (expected-type ($ pi db-str)))

        ;; u32
        (assert-false (expected-type ($ #x0 db-u32)))
        (assert-false (expected-type ($ #xFFFFFFFF db-u32)))
        (assert-equal '(unsigned-byte 32) (expected-type ($ pi db-u32)))
        (assert-equal '(unsigned-byte 32) (expected-type ($ -1 db-u32)))
        (assert-equal '(unsigned-byte 32) (expected-type ($ (1+ #xFFFFFFFF) db-u32)))

        ;; u64
        (assert-false (expected-type ($ #x0 db-u64)))
        (assert-false (expected-type ($ #xFFFFFFFFFFFFFFFF db-u64)))
        (assert-equal '(unsigned-byte 64) (expected-type ($ pi db-u64)))
        (assert-equal '(unsigned-byte 64) (expected-type ($ -1 db-u64)))
        (assert-equal '(unsigned-byte 64) (expected-type ($ (1+ #xFFFFFFFFFFFFFFFF) db-u64)))))))

(define-test transaction-successed
  (with-temp-sophia-directory ()
    (with-database ("test")
      (with-transaction ()
        (setf ($ "x") "a"
              ($ "y") "b"))
      (assert-equal "a" ($ "x"))
      (assert-equal "b" ($ "y")))))

(define-test transaction-failed
  (with-temp-sophia-directory ()
    (with-database ("test" :cmp :u32)
      (with-transaction ()
        (setf ($ 1) "a"
              ($ 2) "b"))
      (ignore-errors
        (with-transaction ()
          (setf ($ 1) nil)
          (error "Bam!")))
      (assert-equal "a" ($ 1))
      (assert-equal "b" ($ 2)))))

(define-test nested-transaction-sucessed
  (with-temp-sophia-directory ()
    (with-named-databases ((strdb "strdb" :cmp :string)
                           (u32db "u32db" :cmp :u32))
      (with-transaction ()
        (setf ($ "x" strdb) "a"
              ($ "y" strdb) "b")
        (setf ($ 100 u32db) "a"
              ($ 101 u32db) "b")
        (with-transaction ()
          (setf ($ "z" strdb) "c"
                ($ 102 u32db) "c")))

      (assert-equal "a" ($ "x" strdb))
      (assert-equal "b" ($ "y" strdb))
      (assert-equal "c" ($ "z" strdb))
      (assert-equal "a" ($ 100 u32db))
      (assert-equal "b" ($ 101 u32db))
      (assert-equal "c" ($ 102 u32db)))

    (with-database ("strdb")
      (assert-equal "a" ($ "x"))
      (assert-equal "b" ($ "y"))
      (assert-equal "c" ($ "z")))

    (with-database ("u32db" :cmp :u32)
      (assert-equal "a" ($ 100))
      (assert-equal "b" ($ 101))
      (assert-equal "c" ($ 102)))))

(define-test nested-transaction-locked-rollback
  (with-temp-sophia-directory ()
    (with-database ("test")
      (with-transaction ()
        (setf ($ "foo") "bar")
        (handler-bind ((transaction-locked #'rollback))
          (with-transaction ()
            (setf ($ "foo") "baz"
                  ($ "xyz") "42"))))
      (assert-equal "bar" ($ "foo"))
      (assert-false ($ "xyz")))))

(defun run ()
  (let ((*print-summary* t))
    (with-temp-directory (temp)
      (run-tests :all :cl-sophia-test))))
