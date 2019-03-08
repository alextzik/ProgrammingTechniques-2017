#lang racket

;; ##########################################
;; YOU DON'T NEED TO READ OR UNDERSTAND THESE

;; Constructor for a mobile
(define make-mobile
  (lambda x
    (cond ((= (length x) 1) (car x))              ;; Sculpture with weight x
          ((= (length x) 3) x)                    ;; Mobile (L, bar, R)
          (else (error "Not a valid Mobile!"))))) ;; nothing else is a mobile

;; Check if a mobile is just a sculpture
(define is-sculpture? number?)

;; Get the weight of the top bar a mobile, or of the sculpture
(define (get-weight m)
  (if (is-sculpture? m)
      m
      (cadr m)))

;; Get the left branch of a mobile
(define left-mobile car)

;; Get the right branch of a mobile
(define right-mobile caddr)

;; Display a mobile
(define (display-mobile mob)
  (define (helper tab m)
    (if (is-sculpture? m)
        (begin 
          (display tab)
          (display m)
          (display "\n"))
        (begin
          (helper (string-append tab "  ") (right-mobile m))
          (helper tab (get-weight m))
          (helper (string-append tab "  ") (left-mobile m)))))
  (display '//////////////////) (newline)
  (helper "" mob)
  (display '//////////////////)(newline))


;; ##########################################
;; Include your code definitions
(include "ps2.scm")

;; ##########################################
;; Validation Helper functions

(define (good-enough? checking gold)
  (if (symbol? checking)
      checking
      (< (abs (- gold
		 checking))
	 0.0001)))
  
(define (correct? exp)
  (if (symbol? exp)
      (display "FAIL\n")
      (if exp
	  (display "PASS\n")
	  (display "FAIL\n"))))


;; ##########################################
;; Validations

(correct? (good-enough? (weights# mobile-1)
			1))

(correct? (good-enough? (total-weight mobile-1)
			30))

(correct? (good-enough? (depth mobile-1)
			1))

(correct? (balanced? mobile-2))

(correct? (good-enough? (weights# mobile-2)
			3))

(correct? (good-enough? (total-weight mobile-2)
			58))

(correct? (good-enough? (depth mobile-2)
			2))

(correct? (balanced? mobile-2))

(correct? (good-enough? (weights# mobile-3)
			5))

(correct? (good-enough? (total-weight mobile-3)
			99))

(correct? (good-enough? (depth mobile-3)
			3))

(correct? (not (balanced? mobile-3)))

(correct? (good-enough? (weights# mobile-4)
			9))

(correct? (good-enough? (total-weight mobile-4)
			210))

(correct? (good-enough? (depth mobile-4)
			4))

(correct? (not (balanced? mobile-4)))

;; ##########################################