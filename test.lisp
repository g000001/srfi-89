(cl:in-package :srfi-89.internal)

(def-suite srfi-89)

(in-suite srfi-89)

(defmacro isqu (x y)
  `(is (equal ,x ,y)))


(defmacro ==> (&body body)
  (do* ((ans (list nil))
        (tem ans
             (cdr (rplacd tem
                          (list
                           (if (equal 'error (nth 2 c))
                               `(signals (cl:error)
                                  ,(nth 0 c) )
                               `(is (equal ,(nth 0 c)
                                           ',(nth 2 c) )))))))
        (c body (cdddr c)) )
       ((endp c) `(progn ,@(cdr ans))) ))

(test define*
  ;;
  (define* (f a (b nil)) (list a b))
  (==>
    (f 1)                  ==>  (1 nil)
    (f 1 2)                ==>  (1 2)
    (f 1 2 3)              ==>  error)
  ;;
  (define* (g a (b a) (:key k (* a b))) (list a b k))
  (==>
     (g 3)                  ==>  (3 3 9)
     (g 3 4)                ==>  (3 4 12)
     (g 3 4 :key)           ==>  error
     (g 3 4 :key 5)         ==>  (3 4 5)
     (g 3 4 :zoo 5)         ==>  error
     (g 3 4 :key 5 :key 6)  ==>  error)
  ;;
  (define* (h1 a (:key k nil) . r) (list a k r))
  (==>
    (h1 7)                 ==>  (7 nil ())
    (h1 7 8 9 10)          ==>  (7 nil (8 9 10))
    (h1 7 :key 8 9 10)     ==>  (7 8 (9 10))
    (h1 7 :key 8 :zoo 9)   ==>  error)
  ;;
  (define* (h2 (:key k nil) a . r) (list a k r))
  (==>
    (h2 7)                 ==>  (7 nil ())
    (h2 7 8 9 10)          ==>  (7 nil (8 9 10))
    (h2 :key 8 9 10)       ==>  (9 8 (10))
    (h2 :key 8 :zoo 9)     ==>  error)

  )


(defvar absent (list 'absent))

(define-function (element tag content . attributes)
  (list "<" tag attributes ">"
        content
        "</" tag ">"))

(define-function (escape value) value)

(define-function (attribute name value)
  (if (eq? value absent)
      '()
      (list " " name "=" (escape value)))) ; could be improved!

(define-function (make-html-styler tag)
  (lambda* ((:id          id          absent)
            (:class       class       absent)
            (:title       title       absent)
            (:style       style       absent)
            (:dir         dir         absent)
            (:lang        lang        absent)
            (:onclick     onclick     absent)
            (:ondblclick  ondblclick  absent)
            (:onmousedown onmousedown absent)
            (:onmouseup   onmouseup   absent)
            (:onmouseover onmouseover absent)
            (:onmousemove onmousemove absent)
            (:onmouseout  onmouseout  absent)
            (:onkeypress  onkeypress  absent)
            (:onkeydown   onkeydown   absent)
            (:onkeyup     onkeyup     absent)
            .
            content )
    (element tag
             content
             (attribute "id" id)
             (attribute "class" class)
             (attribute "title" title)
             (attribute "style" style)
             (attribute "dir" dir)
             (attribute "lang" lang)
             (attribute "onclick" onclick)
             (attribute "ondblclick" ondblclick)
             (attribute "onmousedown" onmousedown)
             (attribute "onmouseup" onmouseup)
             (attribute "onmouseover" onmouseover)
             (attribute "onmousemove" onmousemove)
             (attribute "onmouseout" onmouseout)
             (attribute "onkeypress" onkeypress)
             (attribute "onkeydown" onkeydown)
             (attribute "onkeyup" onkeyup) )))

(define-function html-b      (make-html-styler "b"))
(define-function html-big    (make-html-styler "big"))
(define-function html-cite   (make-html-styler "cite"))
(define-function html-code   (make-html-styler "code"))
(define-function html-dfn    (make-html-styler "dfn"))
(define-function html-em     (make-html-styler "em"))
(define-function html-i      (make-html-styler "i"))
(define-function html-kbd    (make-html-styler "kbd"))
(define-function html-samp   (make-html-styler "samp"))
(define-function html-small  (make-html-styler "small"))
(define-function html-strong (make-html-styler "strong"))
(define-function html-tt     (make-html-styler "tt"))
(define-function html-var    (make-html-styler "var"))

(define* (|print| (:port port *standard-output*) . args)
  (let pr ((x args))
       (cond ((null? x))
             ((pair? x)
              (pr (car x))
              (pr (cdr x)))
             ((vector? x)
              (pr (vector->list x)))
             (:else
              (princ x port)))))

(test |print|
  (is (string=
       (with-output-to-string (out)
         (|print| :port out
                  (html-i :class '|molecule|
                          :id '|water|
                          (html-big "H")
                          (html-small "2")
                          (html-big "O") )
                  ))
       "<i id=water class=molecule><big>H</big><small>2</small><big>O</big></i>")))

;;; eof
