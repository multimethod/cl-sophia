(in-package #:cl-sophia)

(define-foreign-library libsophia
  (:unix "libsophia.so"))

(use-foreign-library libsophia)

(defcfun sp-env :pointer)
(defcfun sp-ctl :pointer (env :pointer))
(defcfun sp-open :int (handle :pointer))
(defcfun sp-get :pointer (handle :pointer) &rest)
(defcfun sp-set :int (handle :pointer) &rest)
(defcfun sp-delete :int (handle :pointer) (object :pointer))
(defcfun sp-object :pointer (handle :pointer))
(defcfun sp-destroy :int (handle :pointer))
(defcfun sp-cursor :pointer (handle :pointer) &rest)
(defcfun sp-begin :pointer (handle :pointer))
(defcfun sp-commit :int (handle :pointer))
