;;;; Implement a list for region structs.

(defun regionlist-p (reglst)
  (or (null reglst) (region-p (car reglst)))
)

;;; Return a string representing a region list.
(defun regionlist-str (alist)
    (assert (regionlist-p alist))

    (let ((str "["))

        (loop for itemx in alist
              for count from 0 do

                 (if (plusp count)
                     (setf str (concatenate 'string str " ")))

                 (setf str (concatenate 'string str (region-str itemx)))
        )
        (setf str (concatenate 'string str "]"))
        str
    )

)

