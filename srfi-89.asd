;;;; srfi-89.asd

(cl:in-package :asdf)


(defsystem :srfi-89
  :version "20200306"
  :description "SRFI 89 for CL: Optional positional and named parameters"
  :long-description "SRFI 89 for CL: Optional positional and named parameters
https://srfi.schemers.org/srfi-89"
  :author "Marc Feeley"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :srfi-88
               :srfi-23
               :srfi-5)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-89")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-89))))
  (let ((name "https://github.com/g000001/srfi-89")
        (nickname :srfi-89))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-89))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-89#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-89)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
