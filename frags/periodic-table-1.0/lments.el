(require 'cl-lib)

(defmacro cl-defparameter (name initial-value &optional documentation)
  (declare (indent 2))
  ;; eval-and-compile?
  (let ((value (make-symbol "initial-value")))
    `(eval-and-compile
       (let ((,value ,initial-value))
         (prog1 (defvar ,name ,value ,documentation)
           (setq ,name ,value))))))

(cl-defparameter *elements-table* nil
  "Table of elements")

(cl-defstruct element
  (name "" :type string)
  (atomic-weight 0.0 :type float)
  (atomic-number 0 :type integer)
  (oxidation-states "" :type string)
  (boiling-point 0.0 :type (or float null))
  (symbol "" :type string)
  (electronegativity 0.0 :type (or float null)))

(defun defelement (name atomic-weight atomic-number
			oxidation-states boiling-point
			symbol electronegativity)
  (push (make-element :name name
		      :atomic-weight (cl-coerce atomic-weight 'float)
		      :atomic-number atomic-number
		      :oxidation-states oxidation-states
		      :boiling-point
		      (if boiling-point
			  (cl-coerce boiling-point 'float))
		      :symbol symbol
		      :electronegativity
		      (if electronegativity
			  (cl-coerce electronegativity 'float)))
	*elements-table*)
  nil)

(defun find-element-if (predicate)
  "A version of `find-if' that searches in `*elements-table'"
  (declare (list *elements-table*))
  (find-if (lambda (element)
	     (funcall predicate element))
	   *elements-table*))

(defun symbol-element (symbol)
  "Get an element from its symbol"
  (find-element-if (lambda (element)
		     (string-equal (element-symbol element)
				   symbol))))

(defun name-element (name)
  "Get an element from its name"
  (find-element-if (lambda (element)
		     (string-equal (element-name element)
				   name))))

(defun atomic-number-element (atomic-number)
  "Get an element from its atomic number"
  (find-element-if (lambda (element)
		     (= (element-atomic-number element)
			atomic-number))))


(defun find-element-if-slot-near (slot-accessor value)
  "Find the element  where the value returned by calling `slot-accessor' on
the element produces the closest match to `value'"
  (declare (list *elements-table*))
  (car
   (sort (copy-seq *elements-table*)
	 #'(lambda (e1 e2)
	     (cond ((null (funcall slot-accessor e1))
		    e2)
		   ((null (funcall slot-accessor e2))
		    e1)
		   (t (let ((delta-e1 (abs (- value
					      (funcall slot-accessor e1))))
			    (delta-e2 (abs (- value
					      (funcall slot-accessor e2)))))
			(< delta-e1 delta-e2))))))))

(defun atomic-weight-element (atomic-weight)
  "Get an element from its approximate atomic weight"
  (find-element-if-slot-near #'element-atomic-weight
			     atomic-weight))

(defun boiling-point-element (boiling-point)
  "Get an element from its approximate boiling point"
  (find-element-if-slot-near #'element-boiling-point
			     boiling-point))

(defun electronegativity-element (electronegativity)
  "Get an element from its approximate electronegativity"
  (find-element-if-slot-near #'element-electronegativity
			     electronegativity))


(deftest test-elements ()
  "Test all the element lookup functions"
  (check
   (string= (element-name (symbol-element "Kr"))
	    "Krypton")
   (string= (element-name (symbol-element "o"))
	    "Oxygen")
   (string= (element-symbol (name-element "gold"))
	    "Au")
   (string= (element-name (atomic-number-element 12))
	    "Magnesium")
   (string= (element-name (atomic-weight-element 16))
	    "Oxygen")
   (string= (element-name (boiling-point-element 120.0))
	    "Krypton")
   (string= (element-name (electronegativity-element 4.0))
	    "Fluorine")))

;; Element getting
;;;;;;;;;;;;;;;;;;

(defun get-element (element &key error-p)
  "Get an element structure from either an element structure (just return it),
an element name, or a symbol for an element. This is not case sensitive. Or
you can search by atomic number if `element' is a positive integer. Or search
for an element whose symbol or name equal the symbol-name if element is a
symbol. If `error-p' is nil, then unmatched elements will simply return nil
but if it is t, then an error will be raised."
  (or (ctypecase element
	(element element)
	(string (or (symbol-element element)
		    (name-element element)))
	(symbol (let ((name (symbol-name element)))
		  (or (symbol-element name)
		      (name-element name))))
	(integer (atomic-number-element element)))
      (if error-p
	  (error "Element ~S not found" element))))



;;(deftest test-get-element ()
;;  (check
;;   (string= (element-name (get-element "Kr"))
;;	    "Krypton")
;;   (string= (element-name (get-element "Lead"))
;;	    "Lead")
;;   (null (get-element "Unobtainium"))
;;   (string= (element-name (get-element (symbol-element "Kr")))
;;	    "Krypton")
;;   (string= (element-name (get-element 82))
;;	    "Lead")
;;   (string= (element-name (get-element 'Pb))
;;	    "Lead")))
