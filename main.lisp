(load #p "xofn.lisp")
(load #p "anyxofn.lisp")
(load #p "value.lisp")
(load #p "region.lisp")
(load #p "regionlist.lisp")

; Return a list of positions, given the number of bits.
; (positions 4) -> (1 2 4 8)
(defun positions (num-bits)
  (assert (integerp num-bits))
  (assert (plusp num-bits))

  (let (ret bit)
    (loop for posx from 0 to (1- num-bits) do
      (setf bit (expt 2 posx))
;     (format t "~&posx is ~D bit is ~D" posx bit)
      (setf ret (append ret (list bit)))
    )
    ret
  )
)

(defun anyxofn-positions (num-positions num-bits)
  (assert (integerp num-positions))
  (assert (integerp num-bits))
  (assert (plusp num-positions))
  (assert (plusp num-bits))
  (assert (>= num-bits num-positions))

  (any-x-of-n num-positions (positions num-bits))
)

; Return a span of numbers, start to end (exclusive)
; (span 2 5) -> (2 3 4)
(defun span (start-num max-num)
  (assert (integerp start-num))
  (assert (integerp max-num))
  (assert (or (zerop start-num) (plusp start-num)))
  (assert (plusp max-num))
  (assert (> max-num start-num))

  (let (ret)
    (loop for numx from start-num to (1- max-num) do
        (setf ret (append ret (list numx)))
    )
    ret
  )
)

(defun allregs (num-non-x num-positions)

  (let (ret nums num-x any bit-pos bit-pos-ary x-ary bits-ary)

     (setf nums (span 0 (expt 2 num-non-x)))
     (setf num-x (- num-positions num-non-x))
     (setf bit-pos (anyxofn-positions num-non-x num-positions))

;    (format t "~&nums ~A" nums)
;    (format t "~&num-x ~d" num-x)
;    (format t "~&bit-pos ~A" bit-pos)

     (loop for numx in nums do
;       (format t "~&numx ~D" numx)
	(loop for posx in bit-pos do
;             (format t "~&numx ~D posx ~A" numx posx)

	      (setf numy 0)
	      (setf amasky *value-mask*)
	      
	      (setf i 1)
              (loop for posi in posx do
		  
;                 (format t "~&    numx ~D posi ~D logand ~D" numy posi (logand numx posi)) 
		  (if (zerop (logand numx i))
		      (setf amasky (logxor amasky posi))
		      (setf numy (logior numy posi))
		  )
		  (setf i (* 2 i))
              )
	      (setf regx (region-new numy amasky))
	      (push regx ret)
;             (format t "~&  numy ~D amasky ~A region ~A" numy amasky (region-str regx))

	)
     )
     ret
  )
)

;; Change this as needed. Values above 9 may exceed memory or time limits.
(value-set-num-bits 4)

(setf tot 1)

(format t "~& ~&3 to power of ~D = ~D~& " *value-number-bits* (expt 3 *value-number-bits*))
(format t "~&Region all X =  1, [~A]" (region-str (region-new 0 *value-mask*))) 

(loop for num-non-x-bits from 1 to (- *value-number-bits* 1) do
  (setf regions (allregs num-non-x-bits *value-number-bits*))
  (setf num-values (expt 2 num-non-x-bits))
  (setf xn (x-of-n num-non-x-bits *value-number-bits*))
  (format t "~& ~&Number values of ~D bits = ~D, " num-non-x-bits num-values)
  (format t "any ~D of ~D = ~D" num-non-x-bits *value-number-bits* (x-of-n num-non-x-bits *value-number-bits*))
  (format t ", ~D * ~D = ~D" num-values (x-of-n num-non-x-bits *value-number-bits*) (* num-values xn))

  (format t "~& ~&  ~A (length = ~D)~&" (regionlist-str regions) (length regions))
  (setf tot (+ tot (length regions)))
)
(setf regsx nil)
(loop for i from 0 to *value-mask* do
     (push (region-new i i) regsx)
)

(format t "~& ~&2 to the ~D power = ~2,' D~&" *value-number-bits* (expt 2 *value-number-bits*))
(format t "~& ~&  ~A (length = ~D)" (regionlist-str regsx) (length regsx))

(setf tot (+ tot (length regsx)))
(format t "~& ~&Total region lists = ~D regions (should be = 3 to the ~D power, above)~& ~&" tot *value-number-bits*)

