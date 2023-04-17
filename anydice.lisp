(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))
	  
(defun copy-hash-table (table)
  (let ((new-table (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               (setf (gethash key new-table) value))
             table)
    new-table))


(defun const-distribution (constant)
  (check-type constant number)
  (sequence-to-distribution (list (floor constant))))

(defun sides-to-sequence (sides)
  (check-type sides integer)
  (loop for n from 1 below (1+ sides) by 1 collect n))

(defun sequence-to-distribution (sequence)
  (check-type sequence list)
  (setf distribution (make-hash-table))
  (setf sequence-length (length sequence))
  (loop for x in sequence
    do (setf (gethash x distribution)
             (+ (/ 1 sequence-length) (gethash x distribution 0))))
  distribution)

(defun distribution-to-sequence (distribution)
  (check-type distribution hash-table)
  (loop for key being each hash-key of distribution
    using (hash-value value) nconc (loop repeat value collect key)))

(defun dice (left right)
  (cond ((integerp right)
         (setf right-distribution
               (sequence-to-distribution (sides-to-sequence right))))
        ((hash-table-p right)
         (setf right-distribution right))
        ((listp right)
         (cond ((every #'integerp right)
                (setf right-distribution (sequence-to-distribution right)))
               (t (error (make-condition 'type-error :datum right
                                         :expected-type
                                         (list 'integer 'hash-table 'list))))))
        (t (error (make-condition 'type-error :datum right
                                  :expected-type
                                  (list 'integer 'hash-table 'list)))))
  (cond ((integerp left)
         (reduce '+ (loop repeat left collect right-distribution)))
        (t (error (make-condition 'type-error :datum left
                                  :expected-type
                                  (list 'integer))))))

(defmacro d (arg1 &optional arg2)
  `(dice ,@(if (null arg2) (list 1 arg1) (list arg1 arg2))))

(defun sorted-hash-table-keys (hash-table)
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    (sort keys (lambda (k1 k2) (< k1 k2)))))
	
(defmacro named (expression name)
  `(list ,name ,expression))

(defun output (&rest named-distributions)
  (let ((output-index 1))
    (dolist (named-distribution named-distributions)
      (unless (listp named-distribution)
        (setf named-distribution (list (format nil "output ~d" output-index) named-distribution)))
      (destructuring-bind (name distribution) named-distribution
        (format t "~%~%~%=========== ~a ===========~%~%" name)
        (cond ((numberp distribution)
               (setf distribution (const-distribution distribution))))
        (check-type distribution hash-table)
        (format t "   #      %")
        (multiple-value-bind (mean stddev) (mean-and-stddev distribution)
          (format t " (~,2f / ~,2f)~%" mean stddev))
        (let* ((bar-chars (list #.(code-char 32) #.(code-char 9615)
                                #.(code-char 9614) #.(code-char 9613)
                                #.(code-char 9612) #.(code-char 9611)
                                #.(code-char 9610) #.(code-char 9609)
                                #.(code-char 9608)))
               (bar-width 100)
               (bar-width-eighths (* bar-width 8)))
          (loop for key in (sorted-hash-table-keys distribution)
                do (let ((value (gethash key distribution)))
                     (format t "~4d ~6,2f ~{~c~}~%" key (* 100 value)
                           (multiple-value-bind
                               (char-column char-decimal) (truncate (* value bar-width))
                             (loop for i from 0 below bar-width
                                  collect
                                  (cond ((< i char-column) (car (last bar-chars)))
                                        ((> i char-column) (first bar-chars))
                                        (t (nth (truncate (* 8 char-decimal)) bar-chars)))))))))
        (incf output-index)))))




  
; define the relational operators for distributions
(defun relational-operation (predicate left right)
  (if (numberp left)
      (setf left (const-distribution left)))
  (if (numberp right)
      (setf right (const-distribution right)))
  (check-type left hash-table)
  (check-type right hash-table)
  (setf result-distribution (make-hash-table))
  (loop for left-key being each hash-key of left using (hash-value left-value)
    do (loop for right-key being each hash-key of right using (hash-value right-value)
             do (if (funcall predicate left-key right-key)
                    (setf (gethash 1 result-distribution)
                          (+ (* left-value right-value)
                             (gethash 1 result-distribution 0)))
                    (setf (gethash 0 result-distribution)
                          (+ (* left-value right-value)
                             (gethash 0 result-distribution 0))))))
  result-distribution)
  
; define the binary arithmetic operator
(defun binary-operation (operator left right)
  (if (numberp left) (setf left (const-distribution left)))
  (if (numberp right) (setf right (const-distribution right)))
  (check-type left hash-table)
  (check-type right hash-table)
  (let ((new-distribution (make-hash-table)))
    (loop for left-key being each hash-key of left using (hash-value left-value)
      do (loop for right-key being each hash-key of right using (hash-value right-value)
        do (let ((result (cond
                           ((eq operator '+) (+ left-key right-key))
                           ((eq operator '-) (- left-key right-key))
                           ((eq operator '*) (* left-key right-key))
                           ((eq operator '/) (floor left-key right-key))
                           ((eq operator 'expt) (expt left-key right-key))
                           (t (error "Invalid argument: ~a" operator)))))
             (setf (gethash result new-distribution)
                   (+ (* left-value right-value)
                      (gethash result new-distribution 0))))))
    new-distribution))
	
; overload operators
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (operator '(< > <= >= = - + * / expt))
    (labels ((create-lambda (operator original-op)
               (lambda (&rest args)
                 (if (some #'hash-table-p args)
                     (reduce (curry (if (member operator '(< > <= >= =))
                                        'relational-operation
                                        'binary-operation)
                                    operator)
                             args)
                     (apply original-op args)))))
      (setf (symbol-function operator)
            (create-lambda operator (symbol-function operator))))))

(defmacro ^ (base exponent)
  `(expt ,base ,exponent))

; custom sequence macro, ({} 1 2 2 (4 . 6)) returns the same as anydice {1,2,2,4..6}
(defmacro {} (&rest faces)
  (let ((result '()))
    (loop for face in faces
          do (cond
               ((numberp face) (push face result))
               ((consp face)
                (let ((start (car face))
                      (end (cdr face)))
                  (loop for i from start to end do (push i result))))
               (t (error "Invalid argument: ~a" face))))
    `(sequence-to-distribution (list ,@(reverse result)))))


(defun reroll (distribution predicate &optional (reroll-limit 1))
  (check-type distribution hash-table)
  (check-type predicate function)
  (check-type reroll-limit integer)
  (let ((new-distribution (copy-hash-table distribution))
        (reroll-count 0))
    (loop while (< reroll-count reroll-limit)
          do (let ((rerolled-distribution (make-hash-table)))
               (loop for key being each hash-key of new-distribution
                     using (hash-value value)
                     do (if (funcall predicate key)
                            (maphash (lambda (k v)
                                       (setf (gethash k rerolled-distribution)
                                             (+ (gethash k rerolled-distribution 0)
                                                (* value v))))
                                     distribution)
                            (setf (gethash key rerolled-distribution)
                                  (+ (gethash key rerolled-distribution 0)
                                     value))))
               (setf new-distribution rerolled-distribution)
               (incf reroll-count)))
    new-distribution))


(defun mean-and-stddev (distribution)
  (check-type distribution hash-table)
  (let ((mean 0)
        (sum-squares 0)
        (count 0))
    (maphash (lambda (key value)
               (incf mean (* key value))
               (incf sum-squares (* (expt key 2) value))
               (incf count value))
             distribution)
    (let ((variance (- sum-squares (/ (expt mean 2) count))))
      (values mean (sqrt (/ variance count))))))


; conditional probability
(defun given (distribution predicate)
  (check-type distribution hash-table)
  (check-type predicate function)
  (let ((new-distribution (make-hash-table))
        (total-probability 0))
    ;; Calculate the total probability of outcomes that satisfy the predicate
    (maphash (lambda (key value)
               (when (funcall predicate key)
                 (incf total-probability value)))
             distribution)
    ;; Create a new distribution with the probabilities of the filtered outcomes
    (maphash (lambda (key value)
               (when (funcall predicate key)
                 (setf (gethash key new-distribution)
                       (/ value total-probability))))
             distribution)
    new-distribution))

(defun middle (values count)
  (let ((sorted-values (sort (copy-list values) #'<))
        (middle-start (ceiling (- (length values) count) 2)))
    (subseq sorted-values middle-start (+ middle-start count))))


(defun pool (selection-type count pool-size distribution)
  (let ((selection-fn (cond
                       ((eq selection-type 'highest) (lambda (values count) (sort-n-highest values count)))
                       ((eq selection-type 'lowest) (lambda (values count) (sort-n-lowest values count)))
                       ((eq selection-type 'middle) (lambda (values count) (middle values count)))
                       (t (error "Invalid selection-type: ~a" selection-type))))
        (result-distribution (make-hash-table)))

  (labels ((generate-permutations (n elements)
             (if (<= n 0)
                 (list nil)
                 (loop for elem in elements
                       nconc (loop for tail in (generate-permutations (1- n) elements)
                                   collect (cons elem tail))))))
    
    (let* ((permutations (generate-permutations pool-size (distribution-to-sequence distribution)))
           (result-distribution (make-hash-table)))
      (dolist (permutation permutations)
        (let* ((sorted-permutation (funcall selection-fn permutation count))
               (sum-selected-values (reduce #'+ sorted-permutation)))
          (setf (gethash sum-selected-values result-distribution)
                (+ (reduce #'* (mapcar (lambda (x) (gethash x distribution)) permutation))
                   (gethash sum-selected-values result-distribution 0)))))
      result-distribution))))

