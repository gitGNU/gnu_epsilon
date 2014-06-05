;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (fact n)
  (e1:if-in n (0)
    1
    (fixnum:* n (fact (fixnum:1- n)))))

(e1:define (gauss n)
  (e0:if-in n (0)
    0
    (e0:primitive fixnum:+ n (gauss (e0:primitive fixnum:1- n)))))

(e1:define (fibo n)
  (e1:if (fixnum:< n 2)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))
(e1:define (test n)
  (e1:dotimes (i n)
    (fio:write "fibo(" (i i) ") = " (i (fibo i)) "\n")))

(e1:define (iter a b)
  (e1:if-in a (0)
    b
    (iter (e0:primitive fixnum:1- a)
          (e0:primitive fixnum:1+ b))))


(e1:define (stripes-once n)
  (e1:primitive io:store-byte! 53280 n)
  (e1:primitive io:store-byte! 53281 n)
  (e0:if-in n (0)
    (e1:bundle)
    (stripes-once (e1:primitive fixnum:1- n))))
(e1:define (stripes)
  (stripes-once 15)
  (stripes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


