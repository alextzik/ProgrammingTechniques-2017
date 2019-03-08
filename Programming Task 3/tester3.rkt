#lang racket

;; ##########################################
;; Basic definitions

;; fast modular exponentiation. From the textbook, section 1.2.4

(define (expmod b e m)
  (cond ((zero? e) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (square x) (* x x))

;; The Fermat test for primality, from the texbook section 1.2.6

(define (fermat-test n)
    (let ((a (choose-random n)))
      (= (expmod a n n) a)))

(define (fast-prime? n times)
    (cond ((zero? times) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

;; The following procedure picks a random number in a given range

(define choose-random
  ;; restriction of Scheme RANDOM primitive
  (let ((max-random-number (expt 10 18))) 
    (lambda (n)
      (random (exact-floor (min n max-random-number))))))

;; To choose a prime, we start searching at a random odd number in a
;; specifed range

(define (choose-prime smallest range)
  (let ((start (+ smallest (choose-random range))))
    (search-for-prime (if (even? start) (+ start 1) start))))

(define (search-for-prime guess)
  (if (fast-prime? guess 2)
      guess
      (search-for-prime (+ guess 2))))

;; An RSA key consists of a modulus and an exponent.

(define make-key cons)
(define key-modulus car)
(define key-exponent cdr)

(define (RSA-transform number key)
  (expmod number (key-exponent key) (key-modulus key)))

;; The following routine compresses a list of numbers to a single
;; number for use in creating digital signatures.

(define (compress intlist)
  (define (add-loop l)
    (if (null? l)
        0
        (+ (car l) (add-loop (cdr l)))))
  (modulo (add-loop intlist) (expt 2 28)))

;; ##########################################
;; RSA key pairs

;; RSA key pairs are pairs of keys

(define make-key-pair cons)
(define key-pair-public car)
(define key-pair-private cdr)

;; ##########################################
;; generate an RSA key pair (k1, k2).

;; This has the property that
;; transforming by k1 and transforming by k2 are inverse operations.
;; Thus, we can use one key as the public key andone as the private key.

(define (generate-RSA-key-pair)
 (let ((size (expt 2 14)))
   ;; we choose p and q in the range from 2^14 to 2^15.  This insures
   ;; that the pq will be in the range 2^28 to 2^30, which is large
   ;; enough to encode four characters per number.
   (let ((p (choose-prime size size))
         (q (choose-prime size size)))
   (if (= p q)       ;check that we haven't chosen the same prime twice
       (generate-RSA-key-pair)     ;(VERY unlikely)
       (let ((n (* p q))
             (m (* (- p 1) (- q 1))))
         (let ((e (select-exponent m)))
           (let ((d (invert-modulo e m)))
             (make-key-pair (make-key n e) (make-key n d)))))))))


;; The RSA exponent can be any random number relatively prime to m

(define (select-exponent m)
  (let ((try (choose-random m)))
    (if (= (gcd try m) 1)
        try
        (select-exponent m))))


;; Invert e modulo m

(define (invert-modulo e m)
 (if (= (gcd e m) 1)
     (let ((y (cdr (solve-ax+by=1 m e))))
       (modulo y m))                   ;just in case y was negative
     (error "gcd not 1" e m)))

;; ##########################################
;; Actual RSA encryption and decryption

(define (RSA-encrypt string key1)
  (RSA-convert-list (string->intlist string) key1))

(define (RSA-convert-list intlist key)
  (let ((n (key-modulus key)))
    (define (convert l sum)
      (if (null? l)
          '()
          (let ((x (RSA-transform (modulo (- (car l) sum) n)
                                  key)))
            (cons x (convert (cdr l) x)))))
    (convert intlist 0)))

(define (RSA-decrypt intlist key2)
  (intlist->string (RSA-unconvert-list intlist key2)))

;; ##########################################
;; Converting between strings and numbers

;; The following procedures are used to convert between strings, and
;; lists of integers in the range 0 through 2^28.  You are not
;; responsible for studying this code -- just use it.

;; Convert a string into a list of integers, where each integer
;; encodes a block of characters.  Pad the string with spaces if the
;; length of the string is not a multiple of the blocksize.

(define (string->intlist string)
  (let ((blocksize 4))
    (let ((padded-string (pad-string string blocksize)))
      (let ((length (string-length padded-string)))
        (block-convert padded-string 0 length blocksize)))))

(define (block-convert string start-index end-index blocksize)
  (if (= start-index end-index)
      '()
      (let ((block-end (+ start-index blocksize)))
        (cons (charlist->integer
	       (string->list (substring string start-index block-end)))
              (block-convert string block-end end-index blocksize)))))

(define (pad-string string blocksize)
  (let ((rem (remainder (string-length string) blocksize)))
    (if (= rem 0)
        string
        (string-append string (make-string (- blocksize rem) #\Space)))))

;; Encode a list of characters as a single number
;; Each character gets converted to an ascii code between 0 and 127.
;; Then the resulting number is c[0]+c[1]*128+c[2]*128^2,...

(define (charlist->integer charlist)
  (let ((n (char->integer (car charlist))))
    (if (null? (cdr charlist))
        n
        (+ n (* 128 (charlist->integer (cdr charlist)))))))

;; Convert a list of integers to a string. (Inverse of
;; string->intlist, except for the padding.)

(define (intlist->string intlist)
  (list->string
   (apply
    append
    (map integer->charlist intlist))))

;; Decode an integer into a list of characters.  (This is essentially
;; writing the integer in base 128, and converting each "digit" to a
;; character.)

(define (integer->charlist integer)
  (if (< integer 128)
      (list (integer->char integer))
      (cons (integer->char (remainder integer 128))
            (integer->charlist (quotient integer 128)))))

;; ##########################################
;; Searching for divisors.

;;; The following procedure is very much like the find-divisor
;;; procedure of section 1.2.6 of the test, except that it increments
;;; the test divisor by 2 each time (compare exercise 1.18 of the
;;; text).  You should be careful to call it only with odd numbers n.

(define (smallest-divisor n)
  (find-divisor n 3))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))

;;;; the following procedure is handy for timing things

(define (timed f . args)
  (match-let-values (((_ _ v _) (time-apply f args)))
                    (display "Execution time: ")
                    (display v)
                    (display " ms\n")))
;; ##########################################







;; ##########################################
;; Include your code definitions
(include "ps3.scm")
;; ##########################################






;; ##########################################
;; Examples to understand the interface

(define test-public-key1 (key-pair-public test-key-pair1))
(define result1 (RSA-encrypt "This is a test message." test-public-key1))

(define test-public-key2 (key-pair-public test-key-pair2))
(define test-private-key2 (key-pair-private test-key-pair2))
(define result2 (RSA-encrypt "I am a private message." test-public-key2))

(define test-private-key1 (key-pair-private test-key-pair1))

(define result3
  (encrypt-and-sign "Test message from user 1 to user 2"
		    test-private-key1
		    test-public-key2))

(define result-forged
  (forge-message clinton-to-gore-fake
                 bill-clinton-public-key
                 al-gore-public-key))

;; ##########################################





;; ##########################################
;; Validations


;; Expected: '(230182985 235417760 205370610 229126900 205322725 67875559)
(RSA-unconvert-list result2 test-private-key2)

;; Expected: "I am a private message. "
(RSA-decrypt result2 test-private-key2)

;; Expected: '(242906196 69006496 213717089 229128819 205322725 67875559)
(RSA-unconvert-list result1 test-private-key1)

;; Expected: "This is a test message. "
(RSA-decrypt result1 test-private-key1)

;; Expected: '((499609777 242153055 12244841 376031918 242988502... . 15378444)
; result3

;; Expected: "Test message from user 1 to user 2  "
 (authenticate-and-decrypt result3
                           test-public-key1
			  test-private-key2)

;; Expected: "The time has come for the Pineapple to stir up the Whitewater.  "
 (decrypt-and-identify)

;; Expected: '(29283862 . -166669675)
 (solve-ax+by=1 233987973
	       41111687)

;; Random pair
 (generate-RSA-key-pair)

;; Expected: '(513756253 . 462557987)
 (crack-rsa (car test-key-pair2))

;; Expected: '((441287624 358718873 8836922 638569903 204999044 162169733… . 826551832)
(forge-message clinton-to-gore-fake bill-clinton-public-key al-gore-public-key)


;; ##########################################





