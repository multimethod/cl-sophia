(in-package #:cl-user)

(defpackage #:cl-sophia-asd
  (:use #:cl
        #:asdf))

(in-package #:cl-sophia-asd)

(defsystem #:cl-sophia
  :description "Sophia database binding"
  :version "0.0.1"
  :author "Andrey V. Tikhonov <multimethod at yandex.ru>"
  :licence "BSD License"
  :serial t
  :depends-on (#:cffi
               #:alexandria)
  :components ((:file "package")
               (:file "foreign")
               (:file "sophia")))

(defsystem #:cl-sophia-test
  :description "cl-sophia test suite"
  :version "0.0.1"
  :author "Andrey V. Tikhonov <multimethod at yandex.ru>"
  :licence "BSD License"
  :depends-on (#:cl-sophia
               #:alexandria
               #:cl-fad
               #:cl-utilities
               #:lisp-unit)
  :serial t
  :components ((:file "tests")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :cl-sophia))))
  (load-system :cl-sophia-test)
  (funcall (intern (symbol-name :run) (find-package :cl-sophia-test))))
