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
        ((listp right)
         (setf right-distribution (sequence-to-distribution right)))
        ((typep right 'hash-table) (setf right-distribution right))
        (t (error (make-condition 'type-error :datum right
                                  :expected-type
                                  (list 'integer 'list 'hash-table)))))
  (cond ((integerp left)
         (reduce 'add-distributions (loop repeat left collect right-distribution)))
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
  (cond ((typep distribution 'ratio)
         (setf distribution (d (floor distribution) 1)))
        ((typep distribution 'integer)
         (setf distribution (d distribution 1))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function '+)
        (let ((original-plus (symbol-function '+)))
          (lambda (x &rest more-numbers)
            (cond ((some #'hash-table-p (cons x more-numbers))
			  (reduce #'add-distributions (cons x more-numbers)))
                (t (apply original-plus x more-numbers)))))))
  
  
(defun relational-distributions (predicate left right)
  (if (integerp left)
      (setf left (d left 1)))
  (if (integerp right)
      (setf right (d right 1)))
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
  (check-type left hash-table)
  (check-type right hash-table)
  (let ((new-distribution (make-hash-table))) ; Define new-distribution here
    (loop for left-key being each hash-key of left using (hash-value left-value)
      do (loop for right-key being each hash-key of right using (hash-value right-value)
        do (let ((result (cond
                           ((eq operator '+) (+ left-key right-key))
                           ((eq operator '-) (- left-key right-key))
                           ((eq operator '*) (* left-key right-key))
                           ((eq operator '/) (floor left-key right-key)))))
             (setf (gethash result new-distribution)
                   (+ (* left-value right-value)
                      (gethash result new-distribution 0))))))
    new-distribution)) ; Return new-distribution


(defun add-distributions (left right)
  (binary-operation '+ left right))

(defun subtract-distributions (left right)
  (binary-operation '- left right))

(defun multiply-distributions (left right)
  (binary-operation '* left right))

(defun divide-distributions (left right)
  (binary-operation '/ left right))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (operator '(< > <= >= =))
    (setf (symbol-function operator)
          (let ((original-op (symbol-function operator)))
            (lambda (x y)
              (cond ((and (hash-table-p x) (hash-table-p y))
                     (relational-distributions original-op x y))
                    (t (funcall original-op x y))))))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function '-)
        (let ((original-minus (symbol-function '-)))
          (lambda (x &rest more-numbers)
            (cond ((some #'hash-table-p (cons x more-numbers))
			  (reduce #'subtract-distributions (cons x more-numbers)))
                (t (apply original-minus x more-numbers)))))))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function '*)
        (let ((original-times (symbol-function '*)))
          (lambda (x &rest more-numbers)
            (cond ((some #'hash-table-p (cons x more-numbers))
			  (reduce #'multiply-distributions (cons x more-numbers)))
                (t (apply original-times x more-numbers)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function '/)
        (let ((original-division (symbol-function '/)))
          (lambda (x &rest more-numbers)
            (cond ((some #'hash-table-p (cons x more-numbers))
			  (reduce #'divide-distributions (cons x more-numbers)))
                (t (apply original-division x more-numbers)))))))

