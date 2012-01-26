;;;; srfi-89.asd

(cl:in-package :asdf)

(defsystem :srfi-89
  :serial t
  :depends-on (:fiveam
               :srfi-88
               :srfi-23
               :srfi-5)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-89")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-89))))
  (load-system :srfi-89)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-89.internal :srfi-89))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
