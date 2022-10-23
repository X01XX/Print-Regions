; Return the factorial of a given number.
(defun factorial (x)
  (assert (plusp x))

  (if (= 1 x) (return-from factorial 1))

  (* x (factorial (1- x)))
)

; Return number of permutations of x of n, order not important.
(defun x-of-n (x n)
  (assert (plusp n))
  (assert (plusp x))

  (if (= x n) (return-from x-of-n 1))

  (assert (> n x))

  (/ (factorial n) (* (factorial x) (factorial (- n x))))
)

; Return the number of regions given the number of non-x-bits
; and the total number of bits.
; (num-regions 4 0) ->  1 (all X)
; (num-regions 4 1) ->  8 (3 X positions)
; (num-regions 4 2) -> 24 (2 X positions)
; (num-regions 4 3) -> 32 (1 X positions)
; (num-regions 4 4) -> 16 (0 X positions, 2 to the num-bits power)
;                      --
;                      81
; The total number of regions is 3 to the number-bits power.
(defun num-regions (num-bits num-non-x)
  (assert (plusp num-bits))
  (assert (>= num-bits num-non-x))
  
  (if (zerop num-non-x) (return-from num-regions 1))

  (let ((num-values (expt 2 num-non-x)))

    (when (= num-non-x num-bits)
      (return-from num-regions num-values))

    (* num-values (x-of-n (- num-bits num-non-x) num-bits))
  )
)
