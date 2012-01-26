;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-89
  (:use)
  (:export))

(defpackage :srfi-89.internal
  (:use :srfi-89 :cl :fiveam :srfi-23 :srfi-5 :srfi-88)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :loop :lambda))
