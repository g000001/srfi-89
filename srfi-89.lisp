;;;; srfi-89.lisp

(cl:in-package :srfi-89.internal)

;------------------------------------------------------------------------------

; Macro expander for define*.

(defmacro define* (pattern &body body)
  (if (pair? pattern)
      `(define-function ,(car pattern)
         (lambda* ,(cdr pattern) ,@body))
      `(define-function ,pattern ,@body)))

;------------------------------------------------------------------------------

; Procedures needed at expansion time.

(define-function (variable? x) (symbol? x))

(define-function (required-positional? x)
  (variable? x) )

(define-function (optional-positional? x)
  (and (pair? x)
       (pair? (cdr x))
           (null? (cddr x))
           (variable? (car x)) ))

(define-function (required-named? x)
  (and (pair? x)
       (pair? (cdr x))
       (null? (cddr x))
       (keyword? (car x))
       (variable? (cadr x)) ))

(define-function (optional-named? x)
  (and (pair? x)
       (pair? (cdr x))
       (pair? (cddr x))
       (null? (cdddr x))
       (keyword? (car x))
       (variable? (cadr x)) ))

(define-function (named? x)
  (or (required-named? x)
      (optional-named? x) ))

(define-function (duplicates? lst)
  (cond ((null? lst)
         nil )
        ((memq (car lst) (cdr lst))
         'T )
        (:else
         (duplicates? (cdr lst)) )))

(define-function (parse-positional-section lst cont)
  (let loop1 ((lst lst) (rev-reqs '()))
       (if (and (pair? lst)
                (required-positional? (car lst)) )
           (loop1 (cdr lst) (cons (car lst) rev-reqs))
           (let loop2 ((lst lst) (rev-opts '()))
                (if (and (pair? lst)
                         (optional-positional? (car lst)) )
                    (loop2 (cdr lst) (cons (car lst) rev-opts))
                    (funcall cont
                             lst
                             (cons (reverse rev-reqs)
                                   (reverse rev-opts) )))))))

(define-function (parse-named-section lst cont)
  (let loop ((lst lst) (rev-named '()))
       (if (and (pair? lst)
                (named? (car lst)) )
           (loop (cdr lst) (cons (car lst) rev-named))
           (funcall cont lst (reverse rev-named)) )))

(define-function (parse-end positional-before-named?
                            positional-reqs/opts
                            named
                            rest )
  (let ((positional-reqs (car positional-reqs/opts))
        (positional-opts (cdr positional-reqs/opts)) )
    (let ((vars
           (append positional-reqs
                   (mapcar #'car positional-opts)
                   (mapcar #'cadr named)
                   (if rest (list rest) '()) ))
          (keys
           (mapcar #'car named) ))
      (cond ((duplicates? vars)
             (error "duplicate variable in formal parameter list") )
            ((duplicates? keys)
             (error "duplicate keyword in formal parameter list") )
            (:else
             (list positional-before-named?
                   positional-reqs
                   positional-opts
                   named
                   rest ))))))

(define-function (parse-rest lst
                             positional-before-named?
                             positional-reqs/opts
                             named )
  (if (null? lst)
      (parse-end positional-before-named?
                 positional-reqs/opts
                 named
                 nil )
      (if (variable? lst)
          (parse-end positional-before-named?
                     positional-reqs/opts
                     named
                     lst )
          (error "syntax error in formal parameter list") )))

(define-function (parse lst)
  (if (and (pair? lst)
           (named? (car lst)) )
      (parse-named-section
       lst
       (lambda (lst named)
         (parse-positional-section
          lst
          (lambda (lst positional-reqs/opts)
            (parse-rest lst
                        nil
                        positional-reqs/opts
                        named )))))
      (parse-positional-section
       lst
       (lambda (lst positional-reqs/opts)
         (parse-named-section
          lst
          (lambda (lst named)
            (parse-rest lst
                        T
                        positional-reqs/opts
                        named )))))))

(define-function (parse-formals formals)
  (parse formals))

(define-function (range lo hi)
  (if (< lo hi)
      (cons lo (range (+ lo 1) hi))
      '() ))

(define-function (expand-lambda* formals body)
  (flet ((expand (positional-before-named?
                  positional-reqs
                  positional-opts
                  named
                  rest )
           (if (and (null? positional-opts) (null? named)) ; direct R5RS equivalent

               `(lambda ,(append positional-reqs (or rest '())) ,@body)

               (cl:let (utility-fns
                        positional-bindings
                        rest-binding
                        named-bindings
                        (arg (gensym "ARG-"))
                        (default (gensym "DEFAULT-"))
                        ($args (gensym "$ARGS-"))
                        ($opt (gensym "$OPT-"))
                        ($req (gensym "$REQ-"))
                        ($key-values (gensym "$KEY-VALUES-")))
                 (setq utility-fns
                       `(,@(if (or positional-before-named?
                                   (null? positional-reqs) )
                               `()
                               `((,$req
                                  (lambda ()
                                    (if (pair? ,$args)
                                        (let ((,arg (car ,$args)))
                                          (set! ,$args (cdr ,$args))
                                          ,arg )
                                        (error "too few actual parameters") )))))
                           ,@(if (null? positional-opts)
                                 `()
                                 `((,$opt
                                    (lambda (,default)
                                      (if (pair? ,$args)
                                          (let ((,arg (car ,$args)))
                                            (set! ,$args (cdr ,$args))
                                            ,arg )
                                          (funcall ,default) )))))))
                 (setq positional-bindings
                     `(,@(if positional-before-named?
                             `()
                             (mapcar (lambda (x)
                                       `(,x (funcall ,$req)) )
                                     positional-reqs ))
                         ,@(mapcar (lambda (x)
                                     `(,(car x) (funcall ,$opt
                                                         (lambda () ,(cadr x)))) )
                                   positional-opts )))
                 (setq named-bindings
                       (if (null? named)
                           `()
                           `((,$key-values
                              (vector ,@(mapcar (lambda (x)
                                                  (declare (ignore x))
                                                  `$undefined)
                                                named )))
                             (,$args
                              ($process-keys
                               ,$args
                               ',(make-perfect-hash-table
                                  (mapcar (lambda (x i)
                                            (cons (car x) i) )
                                          named
                                          (range 0 (length named)) ))
                               ,$key-values ))
                             ,@(mapcar (lambda (x i)
                                         `(,(cadr x)
                                            ,(if (null? (cddr x))
                                                 `($req-key ,$key-values ,i)
                                                 `($opt-key ,$key-values ,i (lambda ()
                                                                             ,(caddr x) )))))
                                     named
                                     (range 0 (length named)) ))))
                 (setq rest-binding
                       (if (not rest)
                           `((,$args (or (null? ,$args)
                                        (error "too many actual parameters") )))
                           `((,rest ,$args)) ))
                 (let ((bindings
                          (append (if positional-before-named?
                                      (append utility-fns
                                              positional-bindings
                                              named-bindings )
                                      (append named-bindings
                                              utility-fns
                                              positional-bindings ))
                                  rest-binding )))
                     `(lambda ,(append (if positional-before-named?
                                           positional-reqs
                                           '() )
                                $args )
                        (let* ,bindings
                          ,@body )))))))
    (apply #'expand (parse-formals formals)) ))

(define-function (make-perfect-hash-table alist)

  ; "alist" is a list of pairs of the form "(keyword . value)"

  ; The result is a perfect hash-table represented as a vector of
  ; length 2*N, where N is the hash modulus.  If the keyword K is in
  ; the hash-table it is at index
  ;
  ;   X = (* 2 ($hash-keyword K N))
  ;
  ; and the associated value is at index X+1.

  (let loop1 ((n (length alist)))
    (let ((v (make-vector (* 2 n) nil)))
      (let loop2 ((lst alist))
        (if (pair? lst)
            (let* ((key-val (car lst))
                   (key (car key-val)))
              (let ((x (* 2 ($hash-keyword key n))))
                (if (vector-ref v x)
                    (loop1 (+ n 1))
                    (begin
                      (vector-set! v x key)
                      (vector-set! v (+ x 1) (cdr key-val))
                      (loop2 (cdr lst))))))
            v)))))

;------------------------------------------------------------------------------

; Macro expander for lambda*.

(defmacro lambda* (formals &body body)
  (expand-lambda* formals body))

;------------------------------------------------------------------------------

; Procedures needed at run time (called by the expanded code):

; Perfect hash-tables with keyword keys.

(define-function ($hash-keyword key n)
  (let ((str (keyword->string key)))
    (let loop ((h 0) (i 0))
      (if (< i (string-length str))
          (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                        n)
                (+ i 1))
          h))))

(define-function ($perfect-hash-table-lookup table key)
  (let* ((n (quotient (vector-length table) 2))
         (x (* 2 ($hash-keyword key n))))
    (and (eq? (vector-ref table x) key)
         (vector-ref table (+ x 1)))))

; Handling of named parameters.

(defvar $undefined (list (gensym "$UNDEFINED-")))

(define-function ($req-key key-values i)
  (let ((val (vector-ref key-values i)))
    (if (eq? val $undefined)
        (error "a required named parameter was not provided")
        val)))

(define-function ($opt-key key-values i default)
  (let ((val (vector-ref key-values i)))
    (if (eq? val $undefined)
        (funcall default)
        val)))

(define-function ($process-keys args key-hash-table key-values)
  (let loop ((args args))
    (if (null? args)
        args
        (let ((k (car args)))
          (if (not (keyword? k))
              args
              (let ((i ($perfect-hash-table-lookup key-hash-table k)))
                (if (not i)
                    (error "unknown parameter keyword" k)
                    (if (null? (cdr args))
                        (error "a value was expected after keyword" k)
                        (begin
                          (if (eq? (vector-ref key-values i) $undefined)
                              (vector-set! key-values i (cadr args))
                              (error "duplicate parameter" k))
                          (loop (cddr args)))))))))))

;------------------------------------------------------------------------------
