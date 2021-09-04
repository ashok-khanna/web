(ql:quickload "hunchentoot")   

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 4242
		 :document-root #p"/Users/ashokkhanna/web/"))

(hunchentoot:start *acceptor*)

