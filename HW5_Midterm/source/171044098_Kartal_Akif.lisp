;; ***************************************
;; *  Author: Akif Kartal/171044098      *
;; *  Task : Midterm HW5                 *
;; ***************************************
;; read parsed list of Horn clauses from file
(defun read-horn-clauses (filename)
  (let ((wholeList ""))

    (with-open-file (stream filename :direction :input)
      (loop for line1 = (read-line stream nil)
        while line1 do (setf wholeList (concatenate 'string wholeList " " line1))
        )
      )
    (setf bigList (read-from-string wholeList))
    (loop for eachList in bigList
      do (evaluate-one-line eachList)
    )
    (set-fact-parameter-lst)
    (set-predicate-parameter-lst)
    (open-file-to-write "output.txt")
    (setf queryList (reverse queryList))
    (loop for query in queryList
      do (resolve-query query)
    )

  )
  (print "Output.txt file has created.")
)

;;evaluate given list and make operation with them.
(defun evaluate-one-line (line)
  (cond ((is-predicate line) (add-predicate line))
    ((is-query line) (add-query line))
    ((add-fact line) nil))
)
;;clasify each list
(defun is-predicate (item)
    (if (and (nth 0 item) (nth 1 item)) t nil)
)
(defun is-query (item)
    (if (nth 0 item) nil t)
)

(defun add-predicate (item)
    (push item predicateList)
    (push (nth 0 (nth 0 item)) predicateNames)
)
(defun add-query (item)
    (push item queryList)
)
(defun add-fact (item)
    (push item factList)
    (push (nth 0 (nth 0 item)) factNames)
)

(defun resolve-query (query)
   (set-query-name query)
   (set-query-params query)
   (setf currentQuery query)
   (setf resultList '())
   (setf res nil)
   (if (is-query-true queryName)
     (fill-bigList)
     (write-query-result-to-file  "()"))
  (setf queryParams '())

)
;;fill the result list for current query
(defun fill-bigList ()
 ;; I wrote empty List with "()" otherwise it will write only NIL into file.
  (if (is-query-contain-variables queryParams) (if (not (check-facts-with-variables queryName queryParams)) (write-query-result-to-file "()"))
    (cond ((is-query-fact queryName queryParams) (push "true" resultList))
      ((is-normal-predicate queryName queryParams) (push "true" resultList))
      ((is-predicate-with-variables queryName queryParams) (push "true" resultList))
      (t (write-query-result-to-file "()")))
  )
  (if (/= 0 (length resultList)) (write-query-result-to-file resultList))

)
;;------CHECK FOR FACTS---------------------
;;check query is fact or not
(defun is-query-fact (query_name query_parameter)
  (setq r1 nil)
  (let ((counter 0) (index '()))
    (loop for name in factNames
      do (progn (if (string= name query_name) (push counter index))
                (setf counter (+ 1 counter)))
      )
     (if (/= (length index) 0)
       (setf r1 (check-query-deeply1 index query_parameter))
     )

  )
  r1;; return result
)
(defun check-query-deeply1 (indexList query_parameter)
 (setq r2 nil)
  (loop for index in indexList do
    (if (= (length (nth index factParameterList)) (length query_parameter))
      (let ((is-nott-fact nil) (counter 0) (params (nth index factParameterList)))
        (loop for element in params
          do (progn (cond ((typep element 'integer) (if (/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                      ((typep element 'string) (if (string/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                      ((typep element 'character) (if (char/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                      (t (print "Your paramater data type is not allowed!!")))
                    (setf counter (+ 1 counter)))
          )
        (if (not is-nott-fact)
          (setf r2 t))
        )
      )
  )
  r2;;return result
)

(defun set-query-name (item)
    (setf queryName (nth 0 (nth 1 item)))
)
(defun set-query-params (item)

 (setf queryParams (nth 1 (nth 1 item)))
)
(defun is-query-true (query_name)
 (setq result nil)
  (loop for name in predicateNames
    do (if (string= name query_name) (setf result t))
  )
  (loop for name in factNames
    do (if (string= name query_name) (setf result t))
  )
 result
)

(defun set-fact-parameter-lst ()
  (loop for item in factList do
    (push (nth 1 (nth 0 item)) factParameterList)
    )
 (setf factParameterList (reverse factParameterList))
)
(defun set-predicate-parameter-lst ()
  (loop for item in predicateList do
    (push (nth 1 (nth 0 item)) predicateParameterList)
    (push (nth 1 item) predicateCases)
  )
 (setf predicateParameterList (reverse predicateParameterList))
 (setf predicateCases (reverse predicateCases))
)
;;---------------------------------------------------
;;------CHECK FOR PREDICATES WHICH HAS NO VARIABLE---------------------
;;check if there is no Cap in the predicate
(defun is-normal-predicate (query_name query_parameter)
 (setq r3 nil)
  (let ((counter 0) (index '()))
    (loop for name in predicateNames
      do (progn (if (string= name query_name) (push counter index))
                (setf counter (+ 1 counter)))
      )
    (if (/= (length index) 0)
      (setf r3 (check-query-deeply2 index query_parameter))
      (setf r3 nil))
    )
 r3
)
(defun check-query-deeply2 (indexList query_parameter)
 (setq r4 nil)
   (loop for index in indexList do
     (if (= (length (nth index predicateParameterList)) (length query_parameter))
       (let ((is-nott-fact nil) (counter 0) (params (nth index predicateParameterList)))
         (loop for element in params
           do (progn (cond ((typep element 'integer) (if (/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       ((typep element 'string) (if (string/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       ((typep element 'character) (if (char/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       (t (print "Your paramater data type is not allowed!!")))
                     (setf counter (+ 1 counter)))
           )
         (if is-nott-fact
           (setf r4 nil)
           (setf r4 (check-predicate index)))
         )
       (setf r4 nil)
       )
  )
 r4
)

(defun check-predicate (index)
 (setq r5 nil)
   (let ((currentList (nth index predicateCases)) (counter 0))
    (loop for statement in currentList do
      (if (is-query-fact (nth 0 statement) (nth 1 statement)) (setf counter (+ 1 counter)))
    )
    (if (= (length currentList) counter) (setf r5 t) (setf r5 nil))
  )
 r5
)
;;------------------------------------------------------------
;;------CHECK FOR PREDICATES WHICH HAS VARIABLES---------------------
;;check is there a cap variable in predicate paramaters
(defun is-predicate-with-variables (query_name query_parameter)
 (setq r6 nil)
  (let ((counter 0) (index '()))
    (loop for name in predicateNames
      do (progn (if (string= name query_name) (push counter index))
                (setf counter (+ 1 counter)))
      )
    (if (/= (length index) 0)
      (setf r6 (check-query-deeply3 index query_parameter))
      (setf r6 nil))
    )

 r6
)
(defun check-query-deeply3 (indexList query_parameter)
   (setq r7 nil)
   (loop for index in indexList do
     (if (= (length (nth index predicateParameterList)) (length query_parameter))
       (if (determine-variables index query_parameter) (setf r7 t))
      )
  )
 r7
)
(defun determine-variables (index query_parameter)
 (setq r8 nil)
  (let ((params (nth index predicateParameterList)) (uperList '()) (uperLetterList '()) (counter 0) (is-nott-fact nil))
    (loop for item in params do
      (progn (if (typep item 'string) (if (upper-case-p (char item 0)) (progn (push counter uperList)
                                                                              (push item uperLetterList))))
             (setf counter (+ 1 counter)))

    )

    (setf counter 0)
    (loop for element in params
      do (progn (if (not (member counter uperList))
           (progn (cond ((typep element 'integer) (if (/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       ((typep element 'string) (if (string/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       ((typep element 'character) (if (char/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       (t (print "Your paramater data type is not allowed!!"))))

             )
             (setf counter (+ 1 counter)))
    )

    (if is-nott-fact
      (setf r8 nil)
      (setf r8 (check-with-resolution index uperList uperLetterList query_parameter))
   )

 )

 r8
)
;;replace variable with real value
(defun check-with-resolution (index uperList uperLetterList query_parameter)

  (let ((currentList (nth index predicateCases)) (counter 0) (parIndex -1) (paramList '()) (myResult t))
   (setq lastResult nil)
    (loop for statement in currentList do
      (progn (setf paramList '())
             (setf myResult t)
      (loop for item in (nth 1 statement) do
        (progn (setf parIndex -1)
              (if (typep item 'string) (if (upper-case-p (char item 0))
              (setf parIndex (position item uperLetterList :test #'equal)) (push item paramList)) (push item paramList))
              (if (/= parIndex -1) (push (nth (nth parIndex uperList) query_parameter) paramList) (setf res nil))
               (setf counter (+ 1 counter)))

      )

            (setf paramList (reverse paramList))
             (if (not (is-query-fact (nth 0 statement) paramList)) (setf myResult nil))
             (setf paramList '()))
    )
   (setf lastResult myResult)
 )

 lastResult
)
;;check if query contain a variable(uppercase)
(defun is-query-contain-variables (query_parameter)
 (setq r9 nil)
 (setq c 0)
  (loop for item in query_parameter do
    (progn (if (typep item 'string) (if (upper-case-p (char item 0)) (progn (setf r9 t)
                                                                     (setf varIndex c))))
           (setf c (+ 1 c)))
  )
 r9
)
;;-----------------------------------------------------------------------
;;------CHECK FOR QUERIES WHICH HAS VARIABLES---------------------
(defun check-facts-with-variables (query_name query_parameter)
 (setq r10 nil)
  (let ((counter 0) (index '()))
    (loop for name in factNames
      do (progn (if (string= name query_name) (push counter index))
                (setf counter (+ 1 counter)))
      )
    (if (/= (length index) 0)
      (setf r10 (check-query-deeply4 index query_parameter))
      (setf r10 nil))
    )
 r10
)
(defun check-query-deeply4 (indexList query_parameter)
   (setq r11 nil)
   (loop for index in indexList do
     (if (= (length (nth index factParameterList)) (length query_parameter))
       (setf r11 (determine-variables2 index query_parameter))
       (setf r11 nil)
      )
  )
 r11
)
(defun determine-variables2 (index query_parameter)
 (setq r12 nil)
  (let ((params (nth index factParameterList)) (counter 0) (is-nott-fact nil))
    (loop for element in params
      do (progn (if (not (= counter varIndex))
           (progn (cond ((typep element 'integer) (if (/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       ((typep element 'string) (if (string/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       ((typep element 'character) (if (char/= element (nth counter query_parameter)) (setf is-nott-fact t)))
                       (t (print "Your paramater data type is not allowed!!"))))

             )
             (setf counter (+ 1 counter)))
    )
    (if (not is-nott-fact)
      (progn (push (nth varIndex params) resultList)
             (setf r12 t))
      (setf r12 nil)
   )

 )
 r12
)
;;---------------------------------------------------------------------------
;;open file in supersede mode to create.
(defun open-file-to-write (filename)
  (with-open-file (stream filename  :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create))
)
;;open file in append mode to write.
(defun write-query-result-to-file (query_result)
   (with-open-file (stream "output.txt" :direction :output
                                                :if-exists :append
                                                :if-does-not-exist :create)
        (write-sequence (format nil "-----------Query: ?-~A~A.-----------~%" (nth 0 (nth 1 currentQuery)) (nth 1 (nth 1 currentQuery))) stream)
        (write-sequence (format nil "~A~%" query_result) stream)

    )
)
;;helper variables in program
(setq predicateList '())
(setq factList '())
(setq queryList '())
(setq predicateNames '())
(setq factNames '())
(setq factParameterList '())
(setq predicateParameterList '())
(setq predicateCases '())
(setq queryParams '())
(setq bigList '())
(setq resultList '())
(setq queryName "default")
(setq currentQuery "default")
(setq res nil)
(setq varIndex -1)

;;start program
(read-horn-clauses "input.txt")
