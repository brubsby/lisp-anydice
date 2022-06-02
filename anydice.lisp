(defun sides-to-sequence (sides)
  (check-type sides integer)
  (loop for n from 1 below (1+ sides) by 1 collect n))

(defun sequence-to-distribution (sequence)
  (check-type sequence list)
  (setf distribution (make-hash-table))
  (loop for x in sequence
    do (setf (gethash x distribution) (1+ (gethash x distribution 0))))
  distribution)

(defun distribution-to-sequence (distribution)
  (check-type distribution hash-table)
  (loop for key being each hash-key of distribution
    using (hash-value value) nconc (loop repeat value collect key)))

(defun combinations (&rest lists)
  (if (endp lists)
      (list nil)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'combinations (cdr lists)))))

(defun mapcar* (func lists) (mapcar (lambda (args) (apply func args)) lists))

(defun dice (left right)
  ""
  (setf diceprobhash (make-hash-table))
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
         (sequence-to-distribution
          (mapcar* #'+
                   (apply 'combinations
                     (loop repeat left collect
                       (distribution-to-sequence right-distribution))))))
        (t (error (make-condition 'type-error :datum left
                                  :expected-type
                                  (list 'integer))))))

(defmacro d (arg1 &optional arg2)
  `(dice ,@(if (null arg2) (list 1 arg1) (list arg1 arg2))))

(defun distribution-to-probability (distribution)
  (setf probability-distribution (make-hash-table))
  (setf total-outcome-count
        (loop for value being the hash-values of distribution sum value))
  (loop for key being each hash-key of distribution using (hash-value value)
    do (setf (gethash key probability-distribution)
             (float (/ (gethash key distribution) total-outcome-count))))
  probability-distribution)

(defun output (distribution)
  (check-type distribution hash-table)
  (format t "   #  %~%")
  (let* ((bar-chars (list #.(code-char 32) #.(code-char 9615)
                          #.(code-char 9614) #.(code-char 9613)
                          #.(code-char 9612) #.(code-char 9611)
                          #.(code-char 9610) #.(code-char 9609)
                          #.(code-char 9608)))
    (bar-width 100)
    (bar-width-eighths (* bar-width 8))
    (probability-distribution (distribution-to-probability distribution)))
    (loop for key being each hash-key of
      probability-distribution using (hash-value value)
      do (format t "~4d ~5,2f ~{~c~}~%" key (* 100 value)
                 (multiple-value-bind
                  (char-column char-decimal) (truncate (* value bar-width))
                   (loop for i from 0 below bar-width
                    collect
                    (cond ((< i char-column) (car (last bar-chars)))
                          ((> i char-column) (first bar-chars))
                          (t (nth (truncate (* 8 char-decimal)) bar-chars)))))))))
