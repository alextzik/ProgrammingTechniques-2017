;; ##########################################
;; Test case mobile

(define mobile-1
  (make-mobile 30))

(define mobile-2
  (make-mobile 4
	       50
	       4))

(define mobile-3
  (make-mobile (make-mobile 20
			    5
			    20)
	       50
	       4))

(define mobile-4
  (make-mobile (make-mobile 20
                            2
                            (make-mobile 50
                                         4
                                         60))
               1
               (make-mobile 30
                            3
                            40)))

;; ##########################################
;; Examples to understand the interface
(display-mobile mobile-2)

(is-sculpture? mobile-2)

(is-sculpture? mobile-1)

(get-weight mobile-2)

(get-weight mobile-1)

(display-mobile mobile-3)

;; ##########################################
;; Code to be completed by the students

(define (weights# mobile)
  (if (is-sculpture? mobile) 1
      (+ 1 (weights# (left-mobile mobile)) (weights# (right-mobile mobile)))))

(define (total-weight mobile)
  (if (is-sculpture? mobile) (get-weight mobile)
      (+ (total-weight (left-mobile mobile)) (total-weight (right-mobile mobile)) (get-weight mobile))))

(define (depth mobile)
  (if (is-sculpture? mobile) 1
      (if (> (+ 1 (depth (left-mobile mobile))) (+ 1 (depth (right-mobile mobile)))) (+ 1 (depth (left-mobile mobile))) (+ 1 (depth (right-mobile mobile))))))

(define (balanced? mobile)
  (if (is-sculpture? mobile) #t
    (if (and (= (total-weight (right-mobile mobile)) (total-weight (left-mobile mobile))) (balanced? (left-mobile mobile)) (balanced? (right-mobile mobile))) #t #f)))



;; ##########################################
;; Sample results

(weights# mobile-2)			; EXPECTED: 3
(total-weight mobile-1)			; EXPECTED: 30
(depth mobile-3)			; EXPECTED: 3
(balanced? mobile-4)			; EXPECTED: #f
