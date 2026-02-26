(load (compile-file "loader.lisp"))
(setf *invoke-debugger-hook*
      (lambda (condition hook)
	(declare (ignore hook))
	(format *error-output* "Error: ~A~%" condition)
	(quit :unix-status 1 :recklessly-p t)))
(save-lisp-and-die "chemboy.core" 
                   :toplevel (lambda ()
			       (chemboy-text:chemboy-repl)
			       (quit)))