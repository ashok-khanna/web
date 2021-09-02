(define frozen)

(append '(the call/cc returned)
	(list (call-with-current-continuation)
	      (lambda (cc)
		(set! frozen cc)
		'a)))
