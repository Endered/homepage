(lisp_engine
 (lambda (TRANSPILER-TEMPORAL-VARIABLE-326)
   (if (= (lambda (x) x) TRANSPILER-TEMPORAL-VARIABLE-326 "lisp-type")
       "lisp-continuation"
       (if (= (lambda (x) x) TRANSPILER-TEMPORAL-VARIABLE-326 "lisp-continue")
	   (list (lambda (TRANSPILER-TEMPORAL-VARIABLE-324)
		   (lambda (TRANSPILER-TEMPORAL-VARIABLE-325)
		     (if (= (lambda (x) x) TRANSPILER-TEMPORAL-VARIABLE-325 "lisp-type")
			 "lisp-continuation"
			 (if (= (lambda (x) x) TRANSPILER-TEMPORAL-VARIABLE-325 "lisp-continue")
			     (apply (lambda (x)
				      (lambda (y)
					(if (= (lambda (x) x) y "lisp-type")
					    "lisp-result"
					    (if (= (lambda (x) x) y "lisp-get-result")
						x
						"I don't know that argument"))))
				    myprint
				    "HE"
				    TRANSPILER-TEMPORAL-VARIABLE-324)
			     "I don't know that value"))))
		 "HEEE")
	   "I don't know that value"))))
