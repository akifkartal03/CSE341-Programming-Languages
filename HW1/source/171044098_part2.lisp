; *********************************************
; *  Author: Akif Kartal                      *
; *  Part : primecrawler                      *
; *********************************************
;;This code works for multi-lines in boundries.txt file as well.
(defun get-boundries(filename)
  (with-open-file (stream "primedistribution.txt" :direction :output
                                                  :if-exists :supersede
                                                  :if-does-not-exist :create))
  (print "File has created...")
  (setq num1 0) (setq num2 0) (setq space 0) (setq i 0)
     (with-open-file (stream filename :direction :input)
       (loop for line = (read-line stream nil)
         while line do (progn (setf space (search " " line))
                              (setf num1 (parse-integer (subseq line 0 space)))
                              (setf num2 (parse-integer (subseq line (+ 1 space) (length line))))
                              (setf i (+ i 1))
                              (determine-and-write num1 num2 i))
         )
     )
)
(defun determine-and-write (num1 num2 count)
  ;;if file contains more than one line
  (if (> count 1) (with-open-file (stream "primedistribution.txt" :direction :output
                                                                  :if-exists :append
                                                                  :if-does-not-exist :create)
                    (write-sequence (format nil "---------Line ~d----------~%" i) stream)))
  (loop for i from num1 to num2 do
    (if (= 1 (is-prime i))  (with-open-file (stream "primedistribution.txt" :direction :output
                                                                            :if-exists :append
                                                                            :if-does-not-exist :create)
                 (write-sequence (format nil "~d is Prime~%" i) stream))
    (if (= 1 (is-semi-prime i))  (with-open-file (stream "primedistribution.txt" :direction :output
                                                                                 :if-exists :append
                                                                                 :if-does-not-exist :create)
                (write-sequence (format nil "~d is Semi-prime~%" i) stream)
       )))
  )
)
(defun is-prime (number)
  (setq divider 0)
  (loop for i from 2 to (- number 1)
    do (if (= 0 (mod number i)) (setf divider (+ divider 1)))
  )
  (if (>= divider 1) 0 1)
)
(defun is-semi-prime (number)
  (setq counter 0) (setq prime1 1) (setq prime2 1)
  (loop for i from 2 to (- number 1)
    do (cond ((and (= 0 (mod number i)) (= 1 (is-prime i))) (setf counter (+ counter 1)) (if (= counter 1) (setf prime1 i) (setf prime2 i))))
  )
  (setq res 0)
  (if (= counter 1) (if (= number (* prime1 prime1)) (setf res (+ res 1))))
  (if (= counter 2) (if (= number (* prime1 prime2)) (setf res (+ res 1))))
  (if (>= res 1) 1 0)
)
(get-boundries "boundries.txt");call function to test
