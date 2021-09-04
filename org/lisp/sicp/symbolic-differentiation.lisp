(defpackage :sym-dx
  (:use :cl))

;;;; Common Lisp Adaptation of Sample Symbolic Differentiation Program
;;;; in SICP, link: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.2

(defmacro safe-symbol-name-check (a b)
  `(and (symbolp ,a)
	(symbolp ,b)
	(equal (symbol-name ,a)
	       (symbol-name ,b))))

(defun differentiation (expression wrt-term)
  (cond ((atom expression)
	 (if (safe-symbol-name-check expression wrt-term)
	     1
	     0))
	((listp expression)
	 (compound-differentiation expression wrt-term))))

(defun compound-differentiation (expression wrt-term)
  (let ((car (car expression)))
    (cond ((safe-symbol-name-check car '+)
	   (list '+
		 (differentiation (cadr expression)
				  wrt-term)
		 (differentiation (caddr expression)
				  wrt-term)))
	  ((safe-symbol-name-check car '*)
	   (list '+
		 (list '* (cadr expression)
		       (differentiation (caddr expression)
					wrt-term))
		 (list '* (caddr expression)
		       (differentiation (cadr expression)
					wrt-term))))
	  (t
	   nil))))


;;;; Differentiation Rules

;;; dc/dx = 0 for a constant c or a variable different to x
;;; dx/dx = 1
;;; d(u + v)/dx = du/dx + dv/dx
;;; d(uv)/dx = u(dv/dx) + v(du/dx)

(defun symbolic-differentiation (expression x-symbol)
  (cond ((contains-x expression) 0)
	((eql expression x-symbol) 1)
	((is-sum expression) )))






;; Observe that the latter two rules are recursive in nature. That is, to obtain the derivative of a sum we first find the derivatives of the terms and add them. Each of the terms may in turn be an expression that needs to be decomposed. Decomposing into smaller and smaller pieces will eventually produce pieces that are either constants or variables, whose derivatives will be either 0 or 1.

;; To embody these rules in a procedure we indulge in a little wishful thinking, as we did in designing the rational-number implementation. If we had a means for representing algebraic expressions, we should be able to tell whether an expression is a sum, a product, a constant, or a variable. We should be able to extract the parts of an expression. For a sum, for example we want to be able to extract the addend (first term) and the augend (second term). We should also be able to construct expressions from parts. Let us assume that we already have procedures to implement the following selectors, constructors, and predicates:

;; (variable? e)	Is e a variable?
;; (same-variable? v1 v2)	Are v1 and v2 the same variable?
;; (sum? e)

;; Is e a sum?
;; (addend e)	Addend of the sum e.
;; (augend e)	Augend of the sum e.
;; (make-sum a1 a2)	Construct the sum of a1 and a2.
;; (product? e)

;; Is e a product?
;; (multiplier e)	Multiplier of the product e.
;; (multiplicand e)	Multiplicand of the product e.
;; (make-product m1 m2)	Construct the product of m1 and m2.
;; Using these, and the primitive predicate number?, which identifies numbers, we can express the differentiation rules as the following procedure:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;            (make-product (multiplier exp)
;;                          (deriv (multiplicand exp) var))
;;            (make-product (deriv (multiplier exp) var)
;;                          (multiplicand exp))))
;;         (else
;;          (error "unknown expression type -- DERIV" exp))))

;; This deriv procedure incorporates the complete differentiation algorithm. Since it is expressed in terms of abstract data, it will work no matter how we choose to represent algebraic expressions, as long as we design a proper set of selectors and constructors. This is the issue we must address next.

;; Representing algebraic expressions

;; We can imagine many ways to use list structure to represent algebraic expressions. For example, we could use lists of symbols that mirror the usual algebraic notation, representing ax + b as the list (a * x + b). However, one especially straightforward choice is to use the same parenthesized prefix notation that Lisp uses for combinations; that is, to represent ax + b as (+ (* a x) b). Then our data representation for the differentiation problem is as follows:

;; The variables are symbols. They are identified by the primitive predicate symbol?:
;; (define (variable? x) (symbol? x))

;; Two variables are the same if the symbols representing them are eq?:
;; (define (same-variable? v1 v2)
;;   (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; Sums and products are constructed as lists:
;; (define (make-sum a1 a2) (list '+ a1 a2))

;; (define (make-product m1 m2) (list '* m1 m2))

;; A sum is a list whose first element is the symbol +:
;; (define (sum? x)
;;   (and (pair? x) (eq? (car x) '+)))

;; The addend is the second item of the sum list:
;; (define (addend s) (cadr s))

;; The augend is the third item of the sum list:
;; (define (augend s) (caddr s))

;; A product is a list whose first element is the symbol *:
;; (define (product? x)
;;   (and (pair? x) (eq? (car x) '*)))

;; The multiplier is the second item of the product list:
;; (define (multiplier p) (cadr p))

;; The multiplicand is the third item of the product list:
;; (define (multiplicand p) (caddr p))

;; Thus, we need only combine these with the algorithm as embodied by deriv in order to have a working symbolic-differentiation program. Let us look at some examples of its behavior:

;; (deriv '(+ x 3) 'x)
;; (+ 1 0)
;; (deriv '(* x y) 'x)
;; (+ (* x 0) (* 1 y))
;; (deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* (* x y) (+ 1 0))
;;    (* (+ (* x 0) (* 1 y))
;;       (+  x 3)))

;; The program produces answers that are correct; however, they are unsimplified. It is true that


;; but we would like the program to know that x · 0 = 0, 1 · y = y, and 0 + y = y. The answer for the second example should have been simply y. As the third example shows, this becomes a serious issue when the expressions are complex.

;; Our difficulty is much like the one we encountered with the rational-number implementation: we haven't reduced answers to simplest form. To accomplish the rational-number reduction, we needed to change only the constructors and the selectors of the implementation. We can adopt a similar strategy here. We won't change deriv at all. Instead, we will change make-sum so that if both summands are numbers, make-sum will add them and return their sum. Also, if one of the summands is 0, then make-sum will return the other summand.

;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else (list '+ a1 a2))))

;; This uses the procedure =number?, which checks whether an expression is equal to a given number:

;; (define (=number? exp num)
;;   (and (number? exp) (= exp num)))

;; Similarly, we will change make-product to build in the rules that 0 times anything is 0 and 1 times anything is the thing itself:

;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2)) (* m1 m2))
;;         (else (list '* m1 m2))))

;; Here is how this version works on our three examples:

;; (deriv '(+ x 3) 'x)
;; 1
;; (deriv '(* x y) 'x)
;; y
;; (deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* x y) (* y (+ x 3)))

;; Although this is quite an improvement, the third example shows that there is still a long way to go before we get a program that puts expressions into a form that we might agree is ``simplest.'' The problem of algebraic simplification is complex because, among other reasons, a form that may be simplest for one purpose may not be for another.

;; Exercise 2.56.  Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule


;; by adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.

;; Exercise 2.57.  Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the last example above could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and products, without changing the deriv procedure at all. For example, the addend of a sum would be the first term, and the augend would be the sum of the rest of the terms.

;; Exercise 2.58.  Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

;; a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.

;; b. The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?
