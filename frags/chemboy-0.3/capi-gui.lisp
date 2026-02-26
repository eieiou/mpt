;(capi:ensure-motif-libraries) ; Make sure that CAPI is loaded

(defpackage :capi-gui
  (:add-use-defaults t)
  (:use :capi
        :chemboy-rootsolve))

(in-package :capi-gui)

;; Generic CAPI development stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-callback (data interface)
  (display-message "Data ~S in interface ~S" 
                   data interface))


;; Loading and saving transcripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-chemboy-transcript-as (data interface)
  (declare (ignore data))
  (let ((filename (prompt-for-file "Select a File:"
                                   :operation :save
                                   :filters (list "Text Documents" "*.TXT"
                                                  "All Files" "*.*"))))
    (when filename
      (with-open-file (file filename
                            :direction :output
                            :if-exists :supersede)
        (princ (editor-pane-text (output interface))
               file)))))

(defun open-chemboy-transcript (data interface)
  (declare (ignore data))
  (let ((filename (prompt-for-file "Select a File:"
                                   :operation :open
                                   :filters (list "Text Documents" "*.TXT"
                                                  "All Files" "*.*")))
        (output-stream (collector-pane-stream (output interface))))
    (when filename
      (with-open-file (file filename)
        (let ((tmp (make-array 1024 :element-type 'character)))
          (loop for bytes = (read-sequence tmp file) until (zerop bytes)
                do (write-sequence tmp output-stream :end bytes)))))))

;; Chemboy queries
;;;;;;;;;;;;;;;;;;

(defun do-query (data interface)
  "Callback to process a Chemboy query"
  (declare (ignore data))
  (let* ((query (text-input-pane-text 
                 (input-query-pane interface)))
         (result (conversions:parse-query query))
         (stream (collector-pane-stream (output interface))))
    (format stream
            "~&> ~A~%~A~%~%"
            query
            (or result
	      "Error: bad query. Have you read the manual?"))))


;; Chemboy toplevel and query interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface chemboy-capi-gui ()
  ()
  (:panes
   (input-query text-input-pane
                :title "Ask me:"
                :callback 'do-query
                :buttons '(:ok t)
                :reader input-query-pane)
   (output collector-pane
           :visible-min-width '(:character 80)
           :visible-min-height '(:character 38)
           :fixed-fill 80
           :reader output))
  (:layouts
   (main-layout column-layout
                '(input-query output)))
  (:menus
   (load-and-save :component
                  (("Open..." :selection-callback 'open-chemboy-transcript)
                   ("Save As..." :selection-callback 'save-chemboy-transcript-as)))
   (clone-menu-item :component
                    (("New window" :selection-callback
                                   #'(lambda (data interface)
                                       (declare (ignore data))
                                       (display (clone interface))))))
   (file-menu "File"
              (load-and-save
               clone-menu-item
               ("Quit" :selection-callback
                       #'(lambda (data interface)
                           (declare (ignore data))
                           (quit-interface interface)))))
   (tools-menu "Tools"
               (("Boyle's Law (pressure & volume)"
                 :selection-callback
                 #'(lambda (data interface)
                     (declare (ignore data interface))
                     (display (make-instance 'chemboy-capi-equation-boyle))))
                ("Charles' Law (volume & temp.)"
                 :selection-callback
                 #'(lambda (data interface)
                     (declare (ignore data interface))
                     (display (make-instance 'chemboy-capi-equation-charles))))
                ("Combined gas Law (everything)"
                 :selection-callback
                 #'(lambda (data interface)
                     (declare (ignore data interface))
                     (display (make-instance 'chemboy-capi-equation-combined-gas))))
                ("Ideal gas Law (just one state)"
                 :selection-callback
                 #'(lambda (data interface)
                     (declare (ignore data interface))
                     (display (make-instance 'chemboy-capi-equation-ideal-gas)))))))
  (:menu-bar file-menu tools-menu)
  (:default-initargs :title "Chemboy 0.3"))

;(display (make-instance 'chemboy-capi-gui))


;; Equation root solving
;;;;;;;;;;;;;;;;;;;;;;;;

(defun safe-read-number-from-string (string)
  "Read from string, safely, ensuring that the result is either
a number or nil"
  (let ((result (let ((*read-eval* nil))
                  (handler-case (read-from-string string)
                    (error () nil)))))
    (when (numberp result)
      result)))

(defun just-one-missing (&rest args)
  "Is just one of the arguments nil?"
  (= (count-if #'identity args)
     (1- (length args))))

;; Boyle's Law
;;;;;;;;;;;;;;

(defun calculate-boyle (data interface)
  "Do the Boyle's law calculations for a given interface
and fill in the blank."
  (declare (ignore data))
  (let ((p1 (safe-read-number-from-string (text-input-pane-text (p1 interface))))
        (v1 (safe-read-number-from-string (text-input-pane-text (v1 interface))))
        (p2 (safe-read-number-from-string (text-input-pane-text (p2 interface))))
        (v2 (safe-read-number-from-string (text-input-pane-text (v2 interface)))))
    (if (not (just-one-missing p1 v1 p2 v2))
        (display-message "You're supposed to leave one thing blank!")
      (let ((result (rootsolve (boyle :p1 p1
                                      :v1 v1
                                      :p2 p2
                                      :v2 v2))))
        (cond ((not p1)
               (setf (text-input-pane-text (p1 interface))
                     (format nil "~S" result)))
              ((not v1)
               (setf (text-input-pane-text (v1 interface))
                     (format nil "~S" result)))
              ((not p2)
               (setf (text-input-pane-text (p2 interface))
                     (format nil "~S" result)))
              ((not v2)
               (setf (text-input-pane-text (v2 interface))
                     (format nil "~S" result))))))))

(define-interface chemboy-capi-equation-boyle ()
  ()
  (:panes
   ;; Explanation at the top
   (explanation display-pane
                  :text '("Enter all quantities except the one to solve for."
                          "Use any units you like as long as they are consistant."))
   (spacer simple-pane
           :external-min-height '(:character 1)
           :external-max-height '(:character 1))
   ;; Pressure 1
   (pressure-1-title title-pane
                     :text "Pressure 1: ")
   (pressure-1 text-input-pane
               :reader p1
               :callback 'calculate-boyle)
   ;; Volume 1
   (volume-1-title title-pane
                   :text "Volume 1: ")
   (volume-1 text-input-pane
             :reader v1
             :callback 'calculate-boyle)
   ;; Pressure 2
   (pressure-2-title title-pane
                     :text "Pressure 2: ")
   (pressure-2 text-input-pane
               :reader p2
               :callback 'calculate-boyle)
   ;; Volume 2
   (volume-2-title title-pane
                   :text "Volume 2: ")
   (volume-2 text-input-pane
             :reader v2
             :callback 'calculate-boyle)
   ;; Big ol' button at the bottom
   (calculate-button push-button
                     :text "Calculate Missing Quantity"
                     :font (gp:make-font-description
                            :size 22
                            :weight :bold)
                     :callback 'calculate-boyle))
  (:layouts
   (main-layout column-layout
                '(explanation spacer input-layout calculate-button))
   (input-layout grid-layout
                 '(pressure-1-title pressure-1
                   volume-1-title volume-1
                   pressure-2-title pressure-2
                   volume-2-title volume-2)
                 :columns 2))
  (:default-initargs :title "Boyle's Law (pressure & volume)"))

;(display (make-instance 'chemboy-capi-equation-boyle))



;; Charles' Law
;;;;;;;;;;;;;;;

(defun calculate-charles (data interface)
  "Do the Charles' law calculations for a given interface
and fill in the blank."
  (declare (ignore data))
  (let ((t1 (safe-read-number-from-string (text-input-pane-text (t1 interface))))
        (v1 (safe-read-number-from-string (text-input-pane-text (v1 interface))))
        (t2 (safe-read-number-from-string (text-input-pane-text (t2 interface))))
        (v2 (safe-read-number-from-string (text-input-pane-text (v2 interface)))))
    (if (not (just-one-missing t1 v1 t2 v2))
        (display-message "You're supposed to leave one thing blank!")
      (let ((result (rootsolve (charles :t1 t1
                                        :v1 v1
                                        :t2 t2
                                        :v2 v2))))
        (cond ((not t1)
               (setf (text-input-pane-text (t1 interface))
                     (format nil "~S" result)))
              ((not v1)
               (setf (text-input-pane-text (v1 interface))
                     (format nil "~S" result)))
              ((not t2)
               (setf (text-input-pane-text (t2 interface))
                     (format nil "~S" result)))
              ((not v2)
               (setf (text-input-pane-text (v2 interface))
                     (format nil "~S" result))))))))

(define-interface chemboy-capi-equation-charles ()
  ()
  (:panes
   ;; Explanation at the top
   (explanation display-pane
                  :text '("Enter all quantities except the one to solve for."
                          "Use any units you like for volume, but temperature"
                          "must be given in Kelvins."))
   (spacer simple-pane
           :external-min-height '(:character 1)
           :external-max-height '(:character 1))
   ;; Temperature 1
   (temperature-1-title title-pane
                     :text "Temperature 1: ")
   (temperature-1 text-input-pane
               :reader t1
               :callback 'calculate-charles)
   ;; Volume 1
   (volume-1-title title-pane
                   :text "Volume 1: ")
   (volume-1 text-input-pane
             :reader v1
             :callback 'calculate-charles)
   ;; Temperature 2
   (temperature-2-title title-pane
                     :text "Temperature 2: ")
   (temperature-2 text-input-pane
               :reader t2
               :callback 'calculate-charles)
   ;; Volume 2
   (volume-2-title title-pane
                   :text "Volume 2: ")
   (volume-2 text-input-pane
             :reader v2
             :callback 'calculate-charles)
   ;; Big ol' button at the bottom
   (calculate-button push-button
                     :text "Calculate Missing Quantity"
                     :font (gp:make-font-description
                            :size 22
                            :weight :bold)
                     :callback 'calculate-charles))
  (:layouts
   (main-layout column-layout
                '(explanation spacer input-layout calculate-button))
   (input-layout grid-layout
                 '(temperature-1-title temperature-1
                   volume-1-title volume-1
                   temperature-2-title temperature-2
                   volume-2-title volume-2)
                 :columns 2))
  (:default-initargs :title "Charles' Law (volume & temp.)"))

;(display (make-instance 'chemboy-capi-equation-charles))


;; Combined Gas Law
;;;;;;;;;;;;;;;;;;;

(defun calculate-combined-gas (data interface)
  "Do the Combined gas law calculations for a given interface
and fill in the blank."
  (declare (ignore data))
  (let ((t1 (safe-read-number-from-string (text-input-pane-text (t1 interface))))
        (v1 (safe-read-number-from-string (text-input-pane-text (v1 interface))))
        (p1 (safe-read-number-from-string (text-input-pane-text (p1 interface))))
        (n1 (safe-read-number-from-string (text-input-pane-text (n1 interface))))
        (t2 (safe-read-number-from-string (text-input-pane-text (t2 interface))))
        (v2 (safe-read-number-from-string (text-input-pane-text (v2 interface))))
        (p2 (safe-read-number-from-string (text-input-pane-text (p2 interface))))
        (n2 (safe-read-number-from-string (text-input-pane-text (n2 interface)))))
    (if (not (just-one-missing t1 v1 p1 n1 t2 v2 p2 n2))
        (display-message "You're supposed to leave one thing blank!
~%If one variable remains constant, just put 1 in for it everywhere.")
      (let ((result (rootsolve (combined-gas :t1 t1
                                             :v1 v1
                                             :p1 p1
                                             :n1 n1
                                             :t2 t2
                                             :v2 v2
                                             :p2 p2
                                             :n2 n2))))
        (cond ((not t1)
               (setf (text-input-pane-text (t1 interface))
                     (format nil "~S" result)))
              ((not v1)
               (setf (text-input-pane-text (v1 interface))
                     (format nil "~S" result)))
              ((not p1)
               (setf (text-input-pane-text (p1 interface))
                     (format nil "~S" result)))
              ((not n1)
               (setf (text-input-pane-text (n1 interface))
                     (format nil "~S" result)))
              ((not t2)
               (setf (text-input-pane-text (t2 interface))
                     (format nil "~S" result)))
              ((not v2)
               (setf (text-input-pane-text (v2 interface))
                     (format nil "~S" result)))
              ((not p2)
               (setf (text-input-pane-text (p2 interface))
                     (format nil "~S" result)))
              ((not n2)
               (setf (text-input-pane-text (n2 interface))
                     (format nil "~S" result))))))))

(define-interface chemboy-capi-equation-combined-gas ()
  ()
  (:panes
   ;; Explanation at the top
   (explanation display-pane
                  :text '("Enter all quantities except the one to solve for."
                          "Temperature must be given in Kelvins; moles must, of"
                          "course, be given in moles; use any units you like for"
                          "everything else. If something (like temperature or"
                          "pressure) remains constant, put in 1 for it everywhere"
                          "to disregard it in the calculations."))
   (spacer simple-pane
           :external-min-height '(:character 1)
           :external-max-height '(:character 1))
   ;; Temperature 1
   (temperature-1-title title-pane
                     :text "Temperature 1: ")
   (temperature-1 text-input-pane
               :reader t1
               :callback 'calculate-combined-gas)
   ;; Volume 1
   (volume-1-title title-pane
                   :text "Volume 1: ")
   (volume-1 text-input-pane
             :reader v1
             :callback 'calculate-combined-gas)
   ;; Pressure 1
   (pressure-1-title title-pane
                     :text "Pressure 1: ")
   (pressure-1 text-input-pane
               :reader p1
               :callback 'calculate-combined-gas)
   ;; Moles 1
   (moles-1-title title-pane
                  :text "Moles 1: ")
   (moles-1 text-input-pane
            :reader n1
            :callback 'calculate-combined-gas)
   ;; Temperature 2
   (temperature-2-title title-pane
                     :text "Temperature 2: ")
   (temperature-2 text-input-pane
               :reader t2
               :callback 'calculate-combined-gas)
   ;; Volume 2
   (volume-2-title title-pane
                   :text "Volume 2: ")
   (volume-2 text-input-pane
             :reader v2
             :callback 'calculate-combined-gas)
   ;; Pressure 2
   (pressure-2-title title-pane
                     :text "Pressure 2: ")
   (pressure-2 text-input-pane
               :reader p2
               :callback 'calculate-combined-gas)
   ;; Moles 1
   (moles-2-title title-pane
                  :text "Moles 2: ")
   (moles-2 text-input-pane
            :reader n2
            :callback 'calculate-combined-gas)
   ;; Big ol' button at the bottom
   (calculate-button push-button
                     :text "Calculate Missing Quantity"
                     :font (gp:make-font-description
                            :size 22
                            :weight :bold)
                     :callback 'calculate-combined-gas))
  (:layouts
   (main-layout column-layout
                '(explanation spacer input-layout calculate-button))
   (input-layout grid-layout
                 '(temperature-1-title temperature-1
                   volume-1-title volume-1
                   pressure-1-title pressure-1
                   moles-1-title moles-1
                   temperature-2-title temperature-2
                   volume-2-title volume-2
                   pressure-2-title pressure-2
                   moles-2-title moles-2)
                 :columns 2))
  (:default-initargs :title "Combined gas law (everything)"))

;(display (make-instance 'chemboy-capi-equation-combined-gas))



;; Ideal Gas Law
;;;;;;;;;;;;;;;;

(defun calculate-ideal-gas (data interface)
  "Do the Ideal gas law calculations for a given interface
and fill in the blank."
  (declare (ignore data))
  (let ((p (safe-read-number-from-string (text-input-pane-text (p interface))))
        (v (safe-read-number-from-string (text-input-pane-text (v interface))))
        (n (safe-read-number-from-string (text-input-pane-text (n interface))))
        (temp (safe-read-number-from-string (text-input-pane-text (temp interface)))))
    (if (not (just-one-missing p v n temp))
        (display-message "You're supposed to leave one thing blank!")
      (let ((result (rootsolve (ideal-gas :p p
                                          :v v
                                          :n n
                                          :temp temp))))
        (cond ((not p)
               (setf (text-input-pane-text (p interface))
                     (format nil "~S" result)))
              ((not v)
               (setf (text-input-pane-text (v interface))
                     (format nil "~S" result)))
              ((not n)
               (setf (text-input-pane-text (n interface))
                     (format nil "~S" result)))
              ((not temp)
               (setf (text-input-pane-text (temp interface))
                     (format nil "~S" result))))))))

(define-interface chemboy-capi-equation-ideal-gas ()
  ()
  (:panes
   ;; Explanation at the top
   (explanation display-pane
                :text '("Enter all quantities except the one to solve for."
                        "Temperature must be given in Kelvins; Pressure"
                        "must be given in kPa (kiloPascals); moles must, of"
                        "course, be given in moles; Volume must be given"
                        "in liters (or \"litres\" if you like British spellings)."))
   (spacer simple-pane
           :external-min-height '(:character 1)
           :external-max-height '(:character 1))
   ;; Pressure
   (pressure-title title-pane
                   :text "Pressure: ")
   (pressure text-input-pane
             :reader p
             :callback 'calculate-ideal-gas)
   ;; Volume
   (volume-title title-pane
                 :text "Volume: ")
   (volume text-input-pane
           :reader v
           :callback 'calculate-ideal-gas)
   ;; Moles
   (moles-title title-pane
                :text "Moles: ")
   (moles text-input-pane
          :reader n
          :callback 'calculate-ideal-gas)
   ;; Temperature
   (temperature-title title-pane
                      :text "Temperature: ")
   (temperature text-input-pane
                :reader temp
                :callback 'calculate-ideal-gas)
   ;; Big ol' button at the bottom
   (calculate-button push-button
                     :text "Calculate Missing Quantity"
                     :font (gp:make-font-description
                            :size 22
                            :weight :bold)
                     :callback 'calculate-ideal-gas))
  (:layouts
   (main-layout column-layout
                '(explanation spacer input-layout calculate-button))
   (input-layout grid-layout
                 '(pressure-title pressure
                   volume-title volume
                   moles-title moles
                   temperature-title temperature)
                 :columns 2))
  (:default-initargs :title "Ideal gas law (just one state)"))

;(display (make-instance 'chemboy-capi-equation-ideal-gas))