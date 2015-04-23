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
