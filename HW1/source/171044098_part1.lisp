; *********************************************
; *  Author: Akif Kartal                      *
; *  Part : flattener                         *
; *********************************************
;Note that all operations was performed with list operations
(defun get-nested-list-and-convert (filename)
  (open-file-to-write "flattened_list.txt") ;open to write
  (setq nested (list))
  (with-open-file (stream filename :direction :input) ; read whole file and
    (loop for token = (read stream nil) ;create nested list with each token
      while token do (setf nested (append nested (list token)))
      )
  )
 (write-single-list-to-file (convert-single-list nested)) ;then create single list
)
;recursive function to perform list operations on the nested list to convert single list .
(defun convert-single-list (nested_list)
   (if (not nested_list) nil ;if parameter is not a list return nil(false)
   (if (atom nested_list) (list nested_list) ;if list is a single list return it
    ;else append the returned lists with recursive call to create a single list
     (append (convert-single-list (first nested_list)) (convert-single-list (rest nested_list)))))
)
;open file in supersede mode(delete and write)
(defun open-file-to-write (filename)
  (with-open-file (stream filename  :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create))
  (print "File has created.")
)
;open file in append mode and write single list
(defun write-single-list-to-file (single_list)
   (with-open-file (stream "flattened_list.txt" :direction :output
                                                :if-exists :append
                                                :if-does-not-exist :create)
         (write-sequence (format nil "~A~%" single_list) stream))
)
(get-nested-list-and-convert "nested_list.txt") ; call function to test.
