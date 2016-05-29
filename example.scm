(define-module example.gauche-mode
  (export)
  )

;; you can export symbol at point by `C-c M-x`
(define (fact n)
  (if (= n 0)
      1
      ;; you can toggle #?= (debug-print) by `C-c C-d`
      #?=(* (fact (- n 1)))))

;; `C-c M-d` disassemble a procedure at point
(display fact)

;; `C-c ;` toggle a datum comment
#;
(display (fact 10))

#|block comment|#

#0=(write-shared #0#)

(define uvectors
  (list #s8() #u8()
        #s16() #u16()
        #s32() #u32()
        #s64() #u64()
        #f16() #f32() #f64()))
;; #vu8()

(map (^(x) #?=(list x)) uvectors)

#/aa\n/
