;; Date: Thursday 5 August 2021

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Multiple Processes
;; Chapter 21, On Lisp

;; 'process' is a locked symbol, hence we are shadowing
;; it in this package

(defpackage :multi-process (:use :common-lisp)
	    (:shadow :restart) (:export :restart))
(in-package :multi-process)


;; Background Code - Continuation Macros
;; Chapter 20, On Lisp

;; Need to add macros for symbol let

;; Below replaces (setq *cont* #'identity)
;; Refer https://stackoverflow.com/questions/24721676/continuation-in-common-lisp-by-macros-regarding-an-implemetation-in-onlisp

(defvar *actual-cont* #'identity)
(define-symbol-macro *cont* *actual-cont*)

(defmacro =lambda (parameters &body body)
  `#'(lambda (*cont* ,@parameters)
       ,@body))

(defmacro =defun (name parameters &body body)
  (let ((f (intern (concatenate 'string
                                "="
                                (symbol-name name)))))
    `(progn
       (defmacro ,name ,parameters
         `(,',f *cont* ,,@parameters))
       (defun ,f (*cont* ,@parameters)
         ,@body))))

(defmacro =bind (parameters expression &body body)
  `(let ((*cont* #'(lambda ,parameters ,@body)))
     ,expression))

(defmacro =values (&rest return-values)
  `(funcall *cont* ,@return-values))

(defmacro =funcall (fn &rest arguments)
  `(funcall ,fn *cont* ,@arguments))

(defmacro =apply (fn &rest arguments)
  `(apply ,fn *cont* ,@arguments))

;; Not sure if this one is required:

;; Below replaces (setq *saved* nil)

(defvar *actual-saved* nil)
(define-symbol-macro *saved* *actual-saved*)

(=defun restart ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))


;; Macros for Multiple Processes

;; 1.0 'Process' Data Structure - > - > - > - >

;; First we define a process structure, with three
;; slots: priority, state and wait.

;; - Priority is the priority of the process and should
;;   be a positive number

;; - State is a continuation representing the state of
;;   a suspended process. A process is restarted by
;;   funcalling its state

;; - Wait is usually a function which must return true
;;   for the process to be restarted. Initially the wait
;;   wait of a newly created process is nil and a process
;;   with a null wait can always be restarted.

(defstruct process priority state wait)


;; 2.0 Global Variables - > - > - > - >

(proclaim '(special *suspended-processes* *current-process*))

;; Simulates the Lisp toplevel, allowing users to halt
;; the program or type expressions which enable suspended
;; processes to restart

(defvar *default-process*
  (make-process :state #'(lambda (x)
                           (format t "~%>> ")
                           (princ (eval (read)))
                           (pick-process))))


;; 3.0 Generating Processes - > - > - > - >

;; Instantiates a process from a function call
;; Here lambda is using gensym because lambda
;; needs to accept a parameter due to funcall
;; (see pick-process) but we want avoid
;; shadowing a free variable of the expression

;; Fork creates a process from a function call
;; e.g. (fork (foo 2) 25), whereby we pass in
;; the function call (foo 2) and also the priority
;; to be given to the process (25)

;; Note the use of gensym within the lambda
;; expression. As we will be funcalling these
;; processes, they must accept at least one
;; argument. We use gensym here to allow us
;; to define the lambda function as a function
;; of one argument, but at the same time avoiding
;; to shadow any free variables of the underlying
;; expression

;; Note also prog1 - this returns the value of its
;; first argument before evalauting the remainder
;; of its arguments, in this case, pushing the
;; created process onto the list of suspended
;; processes

(defmacro fork (expression priority)
  `(prog1 ',expression
     (push (make-process :state #'(lambda (,(gensym))
                                    ,expression
                                    (pick-process))
                         :priority ,priority)
           *suspended-processes*)))


;; 4.0 Running & Halting the Multi Processor - > - > - > - >

;; Set list of suspended processes to zero
;; Push the forks to the suspended processes
;; i.e. the variable *processes*
;; And then enter a loop of pick-process

;; We run a multi process program through the
;; macro program. This macro accepts a name, a list
;; of arguments to pass in, and then as a body a list
;; of fork expressions representing the individual
;; processes to be run in a multi-process environment.

;; It encloses these processes within a continuation
;; (hence the use of =defun), allowing us to save
;; and restart state

;; It first sets the list of suspended-processes to
;; nil and then pushes each of the forks onto the
;; suspended process list. It then starts the pick-
;; process function within a loop and allows for
;; termination via a *halt* throw

(defmacro program (name arguments &body body)
  `(=defun ,name ,arguments
     (setq *suspended-processes* nil)
     ,@body
     (catch *halt* (loop (pick-process)))))

;; As a gensym, the *halt* tag will not conflict
;; with tags established by user code:

(defvar *halt* (gensym))

;; Stops the whole program, by throwing control back to the
;; tag established by the expansion of program. It takes an
;; optional value, which will be returned as the value of
;; the program. 

(defun halt (&optional value)
  "Throw *halt* with value supplied."
  (throw *halt* value))


;; 5.0 Running Processes - > - > - > - >

;; As you can see from its reference within 'program',
;; the pick-process function is used to run processes
;; within a loop.

;; It calls the function 'most-urgent-process' to
;; determine which process has the highest priority,
;; and sets the value of the current-process to it,
;; while at the same time removing it from the list
;; of suspended processes

(defun pick-process ()
  (multiple-value-bind (process value) (most-urgent-process)
    (setq *current-process* process
          *suspended-processes* (delete process *suspended-processes*))
    (funcall (process-state process) value)))


;; 6.0 Determining which process to run - > - > - > - >

;; Loops through all suspended processes and picks
;; the one with the highest priority that is eligible

;; Eligibility determined if it has no wait function
;; or its wait function returns true (i.e. its wait
;; function allows it to be eligible for running)

(defun most-urgent-process ()
  "Returns the process with highest priority and without an active wait restriction."
  (let ((most-urgent-process *default-process*)
        (max-priority -1)
        (most-urgent-process-value t))
    (dolist (process *suspended-processes*)
      (let ((priority (process-priority process)))
        (if (> priority max-priority)
            (let ((value (or (not (process-wait process))
                             (funcall (process-wait process)))))
              (when value
                (setq most-urgent-process process
                      max-priority priority
                      most-urgent-process-value value))))))
    (values most-urgent-process
	    most-urgent-process-value)))

;; Notes for above (refer 'More Notes' below)
;; The above picks the most urgent process together with
;; the result of the test function (if one exists)
;; These are funcalled by pick-process. So one could use
;; the value of the test function as an input to the process
;; to be run (see doors example below)


;; 7.0 Determining which processes should wait - > - > - > - >

;; 'More Notes'

;; Wait functions go into the main processes and are at the
;; end. Basically when you run a current process that has
;; a wait at the end, the wait wraps the remaining forms
;; (which are enclosed within the wait) together with
;; a test function and arguments for the remaining forms
;; and creates a new suspended process (via arbitrator)
;; which stores the state (i.e. remaining forms) as a
;; continuation in process-state and the test function in
;; process-wait

;; Then the arbitrator passes control back to pick-process,
;; which determines which process to run. Note that one
;; can seperately set priority (see setpri) before running
;; a wait process

;; Yield is similar to wait, but has no test function
;; and doesn't allow for parameters within the continuation


;; A 'wait' is similar in spirit to =bind and carries the
;; same restriction that it must be the last thing evaluated.
;; Anything we want to happen after the wait must be put in
;; its body

;; Thus if we want to have a process wait several times, the
;; wait expressions must be nested

(defmacro wait (parameters test &body body)
  `(arbitrator #'(lambda ()
                   ,test)
               #'(lambda (,parameters)
                   ,@body)))

;; There is another simpler type of wait expression, yield,
;; whose only purpose is to give other higher-priority
;; processes a chance to run. A process might want to yield
;; after executing a setpri to reset the priority of the
;; current process. As with a wait, any code to be executed
;; after a yield must be put within its body

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym))
                       ,@body)))

;; Sets the current-process's test function & continuation
;; to the values provided (refer wait function),
;; and then adds this process to the suspended-process list
;; and runs pick-process

;; Stores the current proces, calls pick-process to start
;; some process (perhaps the same one) to run again

;; test is called later to determine if it can be restarted
;; continuation if called will restart the suspended process

(defun arbitrator (test continuation)
  (setf (process-state *current-process*) continuation
        (process-wait *current-process*) test)
  (push *current-process* *suspended-processes*)
  (pick-process))


;; 8.0 Remaining functions - > - > - > - >

(defun set-priority (n)
  "Set the priority of *current-process* to the provided argument."
  (setf (process-priority *current-process*) n))

;; At any time, an individual process can be
;; killed with a kill command. If given no arguments
;; the current process is killed by neglecting to store
;; it back in the suspended-processes (note how pick
;; process removes the current process from the list
;; of suspended processes, so it needs to be added back
;; in or otherwise is lost - which is done via arbitrator)

;; The default process can't be killed, because it isn't
;; kept in the list *suspended-processes*

(defun kill (&optional object &rest arguments)
  "Kill the process supplied or the current process if no arguments provided"
  (if object
      (setq *suspended-processes* (apply #'delete object *suspended-processes* arguments))
      (pick-process)))


;; 9.0 Example of Multi Process - > - > - > - >

;; Remember to switch to this package first
;; via (in-package :multi-process) in your REPL

(defvar *open-doors* nil)

(=defun pedestrian ()
  (wait door (car *open-doors*)
    (format t "Entering ~A~%" door)))

(program ped ()
  (fork (pedestrian) 1))

;; Try the following code

;; > (ped)
;; >> (push 'door2 *open-doors*)
;; Should return ENTERING DOOR2
;; >> (halt)
;; Should return NIL

;; We need this macro (defined elsewhere in on lisp)

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))


;; By asseting facts aimed at one another, processes
;; can cooperate in reaching some goal as per the following
;; example:

(defvar *blackboard* nil)

(defun claim (&rest f) (push f *blackboard*))

(defun unclaim (&rest f) (pull f *blackboard* :test #'equal))

(defun check (&rest f) (find f *blackboard* :test #'equal))

(=defun visitor (door)
  (format t "Approach ~A. " door)
  (claim 'knock door)
  (wait d (check 'open door)
    (format t "Enter ~A. " door)
    (unclaim 'knock door)
    (claim 'inside door)))

(=defun host (door)
  (wait k (check 'knock door)
    (format t "Open ~A. " door)
    (claim 'open door)
    (wait g (check 'inside door)
      (format t "Close ~A.~%" door)
      (unclaim 'open door))))

(program ballet ()
  (fork (visitor 'door1) 1)
  (fork (host 'door1) 1)
  (fork (visitor 'door2) 1)
  (fork (host 'door2) 1))


;; Way this case study works:

;; First add four processes to the program. As they
;; all have the same priority (1), which is higher
;; than the default priority of (0), we will choose
;; the first, which will be third (since because
;; these processes are 'pushed' onto the suspended
;; process list, they are accessed last in first out.
;; Note that the last one (host 'door2) does not run
;; because (knock door) does not currently exist
;; on the *blackboard*.

;; The third process runs until the wait, at which
;; point it is packaged up and added back into the
;; list of suspended processes, with a new test.
;; The next process to run will be number 4, as it
;; is now eligible (due to number 3 pushing (knock
;; door) onto the blackboard.

;; We re-run the process and now number 3 is re-run
;; as it passes the test (open door). This prints
;; out Enter DOOR2, after which again we pass control
;; back to the scheduler, which now runs Close Door1
;; and ends the process. Now we are only left with the
;; first two forks, which basically repeat the same
;; as the above.

;; Note how the first 'Door2' in the output is a result
;; of the fork being called on 'door2 (see number 3).
;; Same thing for the second door2 (open door2) as it
;; is a result of the argument supplied to number 4.

;; These variables are kept due to the continuation
;; saving state (so the door in each, knows what it
;; refers to from the first function calls)

;; In the above, we don't make use of the parameters
;; of the wait funciton (d, k, g) - however if we
;; were to reference them, they would get their values
;; from the test value that is passed in via multiple-
;; urgent-process. which can be seen via changing one
;; of the doros in the format calls above to g, k etc

;; This way, one can use the value of the test functions
;; (which can interact with shared state - to allow one
;; to cooperatively move towards a goal) within the
;; continuations

;; Another example, showing effect of changing priorities

(=defun capture (city)
  (take city)
  (set-priority 1)
  (yield
   (fortify city)))

(=defun plunder (city)
  (loot city)
  (ransom city))

(defun take (city) (format t "Liberating ~A.~%" city))
(defun fortify (city) (format t "Rebuilding ~A.~%" city))
(defun loot (city) (format t "Nationalizing ~A.~%" city))
(defun ransom (city) (format t "Refinancing ~A.~%" city))

(program barbarians ()
  (fork (capture 'rome) 100)
  (fork (plunder 'rome) 98))


;; Way this function works:

;; Add two processes - capture & plunder to the suspended-
;; processes list. Run pick-process, which will select
;; capture as it has a higher priority (100). Capture will
;; take city, then set the priority of its process to 1,
;; and then yield. Note that the current process (capture)
;; is removed from suspended processes when running.

;; Yield will (via arbitrator) add the remaining party of
;; capture (fortify city) back into the suspended processes
;; with a priority 1 (note that set-priority works on the
;; current process (which yield modifies and adds back in
;; to suspended process) unless an object is passed in,
;; which isn't the case here.

;; Yield (via arbitrator) restarts the pick process, which
;; this time will choose plunder city. This will run loot
;; city and ransom city. Once this is run, pick process
;; comes to a conclusion, but because of the loop in 'program',
;; it will be re-run. The only remaining fork left is the
;; parts of capture that was yielded, which is now run, and
;; then we are returned to the default process (from which
;; we can exit).
