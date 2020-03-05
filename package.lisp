;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-89"
  (:use)
  (:export define* lambda*)
  (:size 2))


(defpackage "https://github.com/g000001/srfi-89#internals"
  (:use
   "https://github.com/g000001/srfi-89"
   "https://github.com/g000001/srfi-88"
   "https://github.com/g000001/srfi-23"
   "https://github.com/g000001/srfi-5"
   cl 
   fiveam)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-5"
   let)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-23"
   error)
  (:shadow loop lambda))


;;; *EOF*
