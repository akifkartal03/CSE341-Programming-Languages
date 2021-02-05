; *********************************************
; *  Author: Akif Kartal                      *
; *  Part : collatz sequence                  *
; *********************************************
(defun get-numbers-from-file (filename)
  (with-open-file (stream "collatz_outputs.txt" :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create))
  (print "Calculating...")
  (print "File has created.")
  (setq num1 0) (setq num2 0) (setq space 0) (setq i 0)
  (with-open-file (stream filename :direction :input)
     (loop for line = (read-line stream nil)
       while line do (evaluate-string line)
       )
   )

)
;evaluate each number in the string separetly
(defun evaluate-string (str)
   (setq num 1) (setq counter 0) (setq temp_end 0)
   (loop for start_index = 0 then (+ 1 end_index)
         as end_index = (position #\Space str :start start_index)
         while end_index do (if (< counter 5) (progn (setf num (parse-integer (subseq str start_index end_index)))
                                                     (write-number num)
                                                     (find-collactz num)
                                                     (write-newline)
                                                     (setf counter (+ 1 counter))
                                                     (setf temp_end end_index)))
   )
   (if (< counter 5) (progn (setf num (parse-integer (subseq str (+ 1 temp_end) (length str))))
                                               (write-number num)
                                               (find-collactz num)
                                               (write-newline))
   )
)
;recursive method to find collatz sequence of a number
(defun find-collactz(number)
  ;write number to file
  (with-open-file (stream "collatz_outputs.txt" :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-sequence (format nil "~d " (truncate number)) stream))
  (if (<= number 1) 1 ; base case
    (progn (if (= 0 (mod number 2))
             (setf number (/ number 2.0))
             (setf number (+ 1 (* 3 number))))
           (find-collactz number)));call again

)
(defun write-number(number)
       (with-open-file (stream "collatz_outputs.txt" :direction :output
                                               :if-exists :append
                                               :if-does-not-exist :create)
            (write-sequence (format nil "~d: " number) stream))

)
(defun write-newline()
       (with-open-file (stream "collatz_outputs.txt" :direction :output
                                               :if-exists :append
                                               :if-does-not-exist :create)
            (write-sequence (format nil "~%") stream))

)
(get-numbers-from-file "integer_inputs.txt")
