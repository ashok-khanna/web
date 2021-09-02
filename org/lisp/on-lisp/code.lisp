;; Implementation of Non Deterministic Computing - > - > - > - >
;; p. 295 of On Lisp

;; Store paths which have not yet been followed:

(defparameter *paths* nil)

;; Failure here is defined as the symbol @. If you
;; want to be able to have @ as an ordinary return
;; value, you could make failsym a gensym instead

(defconstant failsym '@)

;; choose can take any number of expressions ('choices')
;; from which it takes one to evaluate

;; If choices = not nil, store remaining choices on
;; *paths* and evaluate first alternative

;; Of course if choices nil, fail

(defmacro choose (&rest choices)
  (if choices
      `(progn
	,@ (mapcar #'(lambda (c)
		       `(push #'(lambda () ,c) *paths*))
	    (reverse (cdr choices)))
	,(car choices))
      '(fail)))


;; Remove last stored choice from *paths* and restart
;; the process (with the remainder of choices). If
;; no paths, then indicate failsym which represents
;; total failure

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

;; Alternate version - should be given a symbol,
;; a list of choices and a body of code. It will
;; choose on the list of choices, bind the symbol to
;; the value chosen and evaluate the body of code:

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
      (progn
	(if (cdr choices)
	    (push #'(lambda ()
		      (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (fail)))


;; Implementation of Continuation-Passing Macros - > - > - > - >

(setq *cont* #'identity)

(defmacro =lambda (parameters &body body)
  `#'(lambda (*cont* ,@parameters)
       ,@body))

(defmacro =defun (name parameters &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parameters
	 `(,',f *cont* ,,@parameters))
       (defun ,f (*cont* ,@parameters)
	 ,@body))))

(defmacro =bind (parameters expression &body body)
  `(let ((*cont* #'(lambda ,parameters
		     ,@body)))
     ,expression))

(defmacro =values (&rest return-values)
  `(funcall *cont* ,@return-values))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))


(let ((f #'identity))
  (let ((g #'(lambda (x)
	       (funcall f (list 'a x)))))
    #'(lambda (x)
	(funcall g (list 'b x)))))

;; Example:

(=defun restart1 ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))

(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))

(setq *saved* nil)

(=defun dft-node (tree)
  (cond ((null tree) (restart1))
	((atom tree) (=values tree))
	(t (push #'(lambda ()
		     (dft-node (cdr tree)))
		 *saved*)
	   (dft-node (car tree)))))

(=defun dt2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
      (cond ((eq node 'done) (=values nil))
	    (t (princ node)
	       (restart1)))))

(setq t1 '(a (b (d h)) (c e (f i) g)))

(setq t2 '(1 (2 (3 6 7) 4 5)))

(=bind (node1)
    (dft-node t1)
  (if (eq node1 'done)
      'done
      (=bind (node2) (dft-node t2)
	(list node1 node2))))


;; emulate lexically scoped global variable cont using symbol-macro that can be
;; shadowed by let or lambda:

(defvar *actual-cont* #'values)
(define-symbol-macro *cont* *actual-cont*)

(defparameter *saved* nil)
;; =defun defines restart1 as a macro, as mentioned. However, you have restart1 defined after dft-node, which has a (restart1) form.
;; Bike
;; So if you load the file sequentially, the macroexpansion of dft-node's body will assume that restart1 is a function, and rewrite it as a CPS call.
;; Bike
;; which doesn't work obviously.
;; lisp123__
;; Yay! It works
;; lisp123__
;; :) Thanks!!!!!
;; lisp123__
;; Now I know why it worked sometime and not, I must have evaluated restart1 first in those caes
;; Bike
;; no problem. in the future you might want to default to using slime-compile-and-load-file, C-c C-k in slime
