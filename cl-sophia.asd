;;;
;;; Copyright (c) 2015 Andrey V. Tikhonov <multimethod@yandex.ru>
;;; This program is free software. It comes without any warranty, to
;;; the extent permitted by applicable law. You can redistribute it
;;; and/or modify it under the terms of the Do What The Fuck You Want
;;; To Public License, Version 2, as published by Sam Hocevar. See
;;; http://www.wtfpl.net/ for more details.
;;;

(in-package #:cl-user)

(defpackage #:cl-sophia-asd
  (:use #:cl
        #:asdf))

(in-package #:cl-sophia-asd)

(defsystem #:cl-sophia
  :description "Sophia key-value storage highlevel API"
  :version "1.0.0"
  :author "Andrey V. Tikhonov <multimethod@yandex.ru>"
  :licence "WTFPL"
  :serial t
  :depends-on (#:cffi
               #:alexandria)
  :components ((:file "package")
               (:file "foreign")
               (:file "sophia")))

(defsystem #:cl-sophia-test
  :description "cl-sophia test suite"
  :version "1.0.0"
  :author "Andrey V. Tikhonov <multimethod@yandex.ru>"
  :licence "WTFPL"
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
