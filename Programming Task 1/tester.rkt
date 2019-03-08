#lang racket

(require racket/include)

;; Read 100m and 200m times
(define time-100 9.58)
(define time-200 19.19)

;; Function definitions
(include "ps1.scm")

;; Validation
(define (good-enough? gold checking)
  (< (abs (- gold
	     checking))
     0.0001))
  
;; Check all outputs
(if (good-enough? usain-accel-100
         2.17920)
         (display "PASS\n")
         (void))

(if (good-enough? usain-v-final-100
         20.876826)
         (display "PASS\n")
         (void))

(if (good-enough? usain-v-max-200
         12.20235)
         (display "PASS\n")
         (void))

(if (good-enough? usain-t-v-max-200
         5.599440)
         (display "PASS\n")
         (void))

(if (good-enough? usain-t-2nd-100-of-200
         8.195139)
         (display "PASS\n")
         (void))