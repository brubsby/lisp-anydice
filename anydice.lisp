(defun curry (function &rest args)
    (lambda (&rest more-args)
      (apply function (append args more-args))))

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

(defun output (distribution)
  (cond ((numberp distribution)
         (setf distribution (const-distribution distribution))))
  (check-type distribution hash-table)
  (format t "   #  %~%")
  (let* ((bar-chars (list #.(code-char 32) #.(code-char 9615)
                          #.(code-char 9614) #.(code-char 9613)
                          #.(code-char 9612) #.(code-char 9611)
                          #.(code-char 9610) #.(code-char 9609)
                          #.(code-char 9608)))
    (bar-width 100)
    (bar-width-eighths (* bar-width 8)))
    (loop for key in (sorted-hash-table-keys distribution)
      do (let ((value (gethash key distribution)))
           (format t "~4d ~5,2f ~{~c~}~%" key (* 100 value)
                 (multiple-value-bind
                  (char-column char-decimal) (truncate (* value bar-width))
                   (loop for i from 0 below bar-width
                    collect
                    (cond ((< i char-column) (car (last bar-chars)))
                          ((> i char-column) (first bar-chars))
                          (t (nth (truncate (* 8 char-decimal)) bar-chars))))))))))
  
  
(defun relational-distributions (predicate left right)
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
	
(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (operator '(< > <= >= = - + * / expt))
    (labels ((create-lambda (operator original-op)
               (lambda (&rest args)
                 (if (some #'hash-table-p args)
                     (reduce (curry (if (member operator '(< > <= >= =))
                                        'relational-distributions
                                        'binary-operation)
                                    operator)
                             args)
                     (apply original-op args)))))
      (setf (symbol-function operator)
            (create-lambda operator (symbol-function operator))))))

(defmacro ^ (base exponent)
  `(expt ,base ,exponent))

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
