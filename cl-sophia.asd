(in-package #:cl-user)

(defpackage #:cl-sophia-asd
  (:use #:cl
        #:asdf))

(in-package #:cl-sophia-asd)

(defsystem #:cl-sophia
  :name "SophiaDB binding"
  :version "0.0.1"
  :author "Andrey V. Tikhonov <multimethod at yandex.ru>"
  :licence "BSD License"
  :serial t
  :depends-on (:cffi
               :alexandria)
  :components ((:file "package")
               (:file "foreign")
               (:file "sophia")))
