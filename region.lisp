;;;; Implement a region as a cons of two values.

; Return a region from two states
(defun make-region (&key state0 state1)
  (assert (value-p state0))
  (assert (value-p state1))

  (cons state0 state1)
)

(defun region-new (state0 &optional state1)
  (if state1
      (make-region :state0 state0 :state1 state1)
      (make-region :state0 state0 :state1 state0))
)

; Return t if an argument is a region
(defun region-p (regx)
  (and
      (consp regx)
      (value-p (car regx))
      (value-p (cdr regx)))
)

; Return the first state of a region
(defun region-state0 (regx)
      (region-p regx)
      (car regx)
)

; Return the second state of a region
(defun region-state1 (regx)
      (region-p regx)
      (cdr regx)
)

;;; Return a string representing a region
(defun region-str (regx)
    (assert (region-p regx))

    (let ((str "") (bitx *value-msb*))

        (loop while (plusp bitx) do
           (if (zerop (value-and bitx (region-state0 regx)))
               (progn
                   (if (zerop (value-and bitx (region-state1 regx)))
                       (setf str (concatenate 'string str "0"))
                       (setf str (concatenate 'string str "x")))
                  )
               (progn
                   (if (zerop (value-and bitx (region-state1 regx)))
                       (setf str (concatenate 'string str "X"))
                       (setf str (concatenate 'string str "1")))
                  )
           )
           (setf bitx (value-shift-right bitx))
        )

        ;(setf str (concatenate 'string str "]"))
        str
    )
)

