;; Complete the following functions

;; compute constant acceleration
;; a = (2s) / (t^2)
(define (const-accel s t)
  (/ (* 2 s) (* t t)))

;; find Usain's acceleration (100m)
(define usain-accel-100 (const-accel 100 time-100))

;; find Usain's final speed (100m)
(define usain-v-final-100
  (* usain-accel-100 time-100))

;; find amount of for Usain to reach max speed (200m)
(define usain-t-v-max-200
(/ (- (* time-200 usain-accel-100) (sqrt(- (* time-200 usain-accel-100 time-200 usain-accel-100) (* 400 usain-accel-100)))) usain-accel-100 ))

;; find Usain's max speed (200m)
(define usain-v-max-200
  (* usain-t-v-max-200 usain-accel-100) )

;; find Usain's time (second 100m)
(define usain-t-2nd-100-of-200
  (/ 100 usain-v-max-200) )
