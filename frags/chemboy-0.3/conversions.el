;; 22.414 L/mol at STP

;; I want to be able to write:
;; 23.2 g O => 1.4500544 mol, 32.509533 L at STP
;; 2 mol Hg => 2 mol, 44.83 L at STP

;;(defpackage :conversions
;;  (:use :common-lisp
;;	:elements
;;	:compounds
;;	:gigamonkeys-unittest)
;;  (:export #:grams->moles
;;	   #:moles->grams
;;	   #:moles->liters
;;	   #:liters->moles
;;	   #:celcius->kelvin
;;	   #:kelvin->celcius
;;	   #:test-temperature-conversions
;;	   #:get-prefix-multiplier
;;	   #:prefixed->base
;;	   #:split-unit-and-return-base
;;	   #:unit->moles
;;	   #:unit->grams
;;	   #:unit->liters
;;	   #:parse-amount
;;	   #:pretty-print-element
;;	   #:parse-query-list
;;	   #:parse-query))
;;
;;(in-package :conversions)


(defun grams->moles (grams compound)
  "Convert a number of `grams' of an element to moles of that element.
`element' can be either an element specifier or a molecule specifier."
  (let ((weight (formula-weight compound)))
    (/ grams weight)))

(defun moles->grams (moles compound)
  "Convert a number of moles of a compound to grams of that compound"
  (let ((weight (formula-weight compound)))
    (* moles weight)))

(defconst +molar-gas-constant+ 8.314472
  "The molar gas constant in L * kPa / mole * K. This works with moles,
Kelvins, kPa, and L.")

(defun moles->liters (moles &key (temperature 273.15)
			         (pressure 101300))
  "Convert moles to liters at a given temperature and pressure, which
default to STP. Temperature is in Kelvins, pressure in Pascals."
  (/ (* moles
	+molar-gas-constant+
	temperature)
     (/ pressure			; Convert pressure to kPa
	1000)))

(defun liters->moles (liters &key (temperature 273.15)
			          (pressure 101300))
  "Convert liters to moles at a given temperature and pressure, which
default to STP. Temperature is in Kelvins, pressure in Pascals."
  (/ (* liters
	(/ pressure			; Convert pressure to kPa
	1000))
     (* +molar-gas-constant+
	temperature)))

;; FIXME: liters->moles

;; Unit conversions
;;;;;;;;;;;;;;;;;;;

(defun celcius->kelvin (celcius)
  "Convert degrees Celcius to Kelvins"
  (+ celcius 273.15))

(defun kelvin->celcius (kelvin)
  "Convert Kelvins to degrees Celcius"
  (- kelvin 273.15))

(deftest test-temperature-conversions ()
  (check
   (= (celcius->kelvin 0)
      273.15)
   (= (kelvin->celcius 273.15)
      0.0)))

(defparameter *allowed-units* '("g" "mol" "L")
  "A list of valid units")

(defparameter *prefix-table* '(("y" . #.(expt 10 -24))
			       ("z" . #.(expt 10 -21))
			       ("a" . #.(expt 10 -18))
			       ("f" . #.(expt 10 -15))
			       ("p" . #.(expt 10 -12))
			       ("n" . #.(expt 10 -9))
			       ("u" . #.(expt 10 -6))
			       ("m" . #.(expt 10 -3))
			       ("c" . #.(expt 10 -2))
			       ("d" . #.(expt 10 -1))
			       ("da" . #.(expt 10 1))
			       ("h" . #.(expt 10 2))
			       ("k" . #.(expt 10 3))
			       ("M" . #.(expt 10 6))
			       ("G" . #.(expt 10 9))
			       ("T" . #.(expt 10 12))
			       ("P" . #.(expt 10 15))
			       ("E" . #.(expt 10 18))
			       ("Z" . #.(expt 10 21))
			       ("Y" . #.(expt 10 24)))
  "A table mapping metric prefixes to the multipliers they represent.")

(defun get-prefix-multiplier (prefix)
  "Get the metric prefix multiplier of `prefix', given as a string."
  (or (cdr (assoc prefix *prefix-table*
		  :test #'string=))
      (error "Metric prefix ~S not found" prefix)))

(defun prefixed->base (prefix number)
  "Convert a number of units with a certain metric prefix to a number of units
without a prefix. For example, 3 kPa = 3000 Pa"
  (* number
     (get-prefix-multiplier prefix)))

(deftest check-metric-prefixes ()
  (check
   (= (prefixed->base "k" 3)
      3000)
   (= (prefixed->base "f" 4500)
      9/2000000000000)))

;; Query parser
;;;;;;;;;;;;;;;

;; 23.2 g O => 1.4500544 mol, 32.509533 L at STP
;; 2 mol Hg => 2 mol, 44.83 L at STP

;; <query> ::= <amount> [(at|and) (<temperature>|<pressure>)]*
;; <amount> ::= <number> <unit> <element>
;; (<temperature>|<pressure>) ::= <number> <units>

(defun split-unit-and-return-base (unit number)
  "Take a unit like kPa and a number with that unit, and return the base unit
like Pa and the number of that unit."
  (if (member unit *allowed-units*
	      :test #'string=)
      (values unit number)
    (let ((prefix (subseq unit 0 1))
	  (base (subseq unit 1)))
      (values base
	      (prefixed->base prefix number)))))

(defun unit->moles (unit number compound &key (temperature 273.15)
		                	      (pressure 101300))
  "Take a unit string, a number which has that unit, and a compound, and
return the number of moles of that compound"
  (cond ((string= unit "mol")
	 number)
	((string= unit "g")
	 (grams->moles number compound))
	((string= unit "L")
	 (liters->moles number
			:temperature temperature
			:pressure pressure))))

(defun unit->grams (unit number compound &key (temperature 273.15)
			                     (pressure 101300))
  "Take a unit string, a number which has that unit, and a compound, and
return the number of grams of that compound."
  (cond ((string= unit "mol")
	 (moles->grams number compound))
	((string= unit "g")
	 number)
	((string= unit "L")
	 (moles->grams (liters->moles number
				      :temperature temperature
				      :pressure pressure)
		       compound))))

(defun unit->liters (unit number compound &key (temperature 273.15)
			                      (pressure 101300))
  "Take a unit string, a number which has that unit, and a compound, and
return the number of liters of that compound at a given temperature and
pressure, which default to STP."
  (cond ((string= unit "mol")
	 (moles->liters number
			:temperature temperature
			:pressure pressure))
	((string= unit "g")
	 (moles->liters (grams->moles number compound)
			:temperature temperature
			:pressure pressure))
	((string= unit "L")
	 number)))

;; FIXME: there should be error checking
(defun parse-amount (amount)
  "Parse an amount, returning number of moles, # of grams, # of liters,
and the compound"
  (destructuring-bind (number unit compound)
      amount
    (let ((number (read-from-string number))
	  (compound (parse-compound compound)))
      (multiple-value-bind (base-unit base-number)
	  (split-unit-and-return-base unit number)
	(values (unit->moles base-unit base-number compound)
		(unit->grams base-unit base-number compound)
		(unit->liters base-unit base-number compound)
		compound)))))

;; FIXME: find out where this is called from, replace with pprint-compound
(defun pretty-print-element (element)
  (if (consp element)
      (format nil "~A~S"
	      (element-symbol (get-element (car element)))
	      (cdr element))
    (element-symbol (get-element element))))

(defun parse-query-list (query)
  "Parse a query given as a list of strings and return a string for the
result, giving all the proper information"
  (multiple-value-bind (moles grams liters element)
      (parse-amount query)		; Yes, this discards the "at ..." part
    (format nil "~S mol of ~A, ~S g, ~S L at STP"
	    moles
	    (with-output-to-string (s)
	       (pprint-compound element s))
	    grams
	    liters)))

(defun parse-element-query (element)
  "Return some text describing an element, given as a valid argument
to get-element"
  (let ((e (get-element (read-from-string element))))
    (when e
      (format nil
	      "~A (~A)
Atomic weight: ~A g/mol
Atomic number: ~A
Electronegativity: ~A
Oxidation states: ~A
Boiling point: ~:[none~;~A K~]"
	      (element-name e)
	      (element-symbol e)
	      (element-atomic-weight e)
	      (element-atomic-number e)
	      (element-electronegativity e)
	      (element-oxidation-states e)
              (element-boiling-point e) ; For conditional
	      (element-boiling-point e)))))

(defun parse-compound-query (text)
  "Parse a compound query and return the information that should
be displayed to the user"
  (let ((parsed-compound (parse-compound text)))
    (when parsed-compound
      (format nil "Compound: ~A~A
Formula weight: ~A"
	      (with-output-to-string (s)
				     (pprint-compound parsed-compound s))
	      (compound-names:get-compound-description-text parsed-compound)
	      (formula-weight parsed-compound)))))

(defun parse-query (query)
  (let ((split-query (chemboy-query-parsing:amount-query query)))
    (if split-query
	(handler-case (parse-query-list split-query)
          (error ()
                 (handler-case (parse-compound-query query)
                   (error () nil))))
      (let ((element-query (chemboy-query-parsing:number-or-letters query)))
	(if element-query
	    (handler-case (parse-element-query element-query)
              (error ()
                     (handler-case (parse-compound-query query)
                       (error () nil))))
	  (handler-case (parse-compound-query query)
			(error () nil)))))))

;(defun parse-query (query)
;  (let ((split-query (handler-case (coerce
;                                    (second
;                                     (multiple-value-list
;                                      (cl-ppcre:scan-to-strings
;                                       "^([^\\s]+)\\s+([a-zA-Z]+)\\s+(.*)$"
;                                       query)))
;                                    'list)
;                       (error () nil))))
;    (if split-query
;        (handler-case (parse-query-list split-query)
;          (error ()
;                 (handler-case (parse-compound-query query)
;                   (error () nil))))
;      (let ((element-query (handler-case (elt
;                                          (second
;                                           (multiple-value-list
;                                            (cl-ppcre:scan-to-strings
;                                             "^([a-zA-Z]+|[0-9]+)$"
;                                             query)))
;                                          0)
;                             (error () nil))))
;        (if element-query
;            (handler-case (parse-element-query element-query)
;              (error ()
;                     (handler-case (parse-compound-query query)
;                       (error () nil))))
;          (parse-compound-query query))))))

;(parse-query "This was written in LispWorks")
;(parse-query "23.2 g O")
;(parse-query "23.2 g O2")
;(parse-query "0.01 kg C6 H12 O6")
;(parse-query "1.2444 L c6 h12 o6")
;(parse-query "12")
;(parse-query "o")
;(parse-query "yttrium")
;(parse-query "c h4")
