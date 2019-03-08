;; ##########################################
;; Initial test data

(define test-key-pair1
  (make-key-pair
   (make-key 816898139 180798509)
   (make-key 816898139 301956869)))

(define test-key-pair2
  (make-key-pair
   (make-key 513756253 416427023)
   (make-key 513756253 462557987)))

;;;public keys for political figures

(define bill-clinton-public-key (make-key 833653283 583595407))
(define al-gore-public-key (make-key 655587853 463279441))
(define bob-dole-public-key (make-key 507803083 445001911))
(define ross-perot-public-key (make-key 865784123 362279729))
(define hillary-clinton-public-key (make-key 725123713 150990017))
(define tipper-gore-public-key (make-key 376496027 270523157))
(define chuck-vest-public-key (make-key 780450379 512015071))
(define rupert-murdoch-public-key (make-key 412581307 251545759))
(define newt-gingrich-public-key (make-key 718616329 290820109))
(define newt-gingrich-private-key (make-key 718616329 129033029))


;;;message received by Newt Gingrich -- Who sent it?
(define received-mystery-message
  '(510560918 588076790 115222453 249656722 408910590 69814552
    690687967 281490047 41430131 256420885 184791295 75938032
    693840839 663727111 593617709 335351412))

(define received-mystery-signature 65732336)

;;;fake message from Clinton to Gore
(define clinton-to-gore-fake
  "Dear Al, we are preparing to impose several tax increases, including VAT and fuel levy. Please prepare a press release to inform the public. Kind regards, Bill Cinton")

;; ##########################################
;; ##########################################
;; Code to be completed by the students

;; the inverse procedure of RSA-convert-list
(define (RSA-unconvert-list intlist key)
(let ((n (key-modulus key)))
  (define (unconvert lst sum)
    (if (null? lst)
        '()
        (let ((x (remainder (+ sum (RSA-transform  (car lst)  key)) (* -1 n))))
          (cons x (unconvert (cdr lst) (car lst))))))
  (unconvert intlist 0)
    ))



;; simple signed-message structure
(define (make-signed-message message signature)
  (cons message signature))

(define (get-signed-body signed-message)
  (car signed-message))

(define (get-signed-signature signed-message)
  (cdr signed-message))
  
;; Encrypt message and sign
(define (encrypt-and-sign message s-private-key r-public-key)
  (let ((secret (RSA-encrypt message r-public-key)))
  (make-signed-message secret (car (RSA-convert-list (list (compress secret)) s-private-key)))))

;; Authenticate message and decrypt it
(define (authenticate-and-decrypt signed-message s-public-key r-private-key)
  (let* ((text (RSA-unconvert-list (get-signed-body signed-message) r-private-key))
         (real-sig (compress (get-signed-body signed-message)))
         (claim-sig (RSA-unconvert-list (list (get-signed-signature signed-message)) s-public-key)))
     (if (= real-sig (car claim-sig))
             (intlist->string text)
             "Authentication Failed!")))

;; Decrypt and identify message to Gingrich
(define (decrypt-and-identify)
  (intlist->string (RSA-unconvert-list received-mystery-message newt-gingrich-private-key)))
 

;; solve ax+by=1
;; The idea is to let a=bq+r and solve bx+ry=1 recursively
(define (solve-ax+by=1 a b) 
    (cond
      ((< a b) (solve-ax+by=1 b a))
      ((= (remainder a b) 1) (cons 1 (* -1 (quotient a b)))) 
      ((let ((q (quotient a b)) 
            (r (remainder a b)))
       (let ((res (solve-ax+by=1 b r)))
       (cons (+ (cdr res) (/ b (gcd a b))) (- (+ (car res) (* (* -1 q) (cdr res))) (/ a (gcd a b)))))))
        ))
;; crack the RSA cryptography system
;; given a public key find the private one
(define (crack-rsa public-key)
   (let* ((n (key-modulus public-key))
         (e (key-exponent public-key))
         (p (smallest-divisor n))
         (q (/ n p))
         (m (* (- p 1) (- q 1))) 
         (d (cdr (solve-ax+by=1 e m))))
           (make-key n (+ d (* 1 (/ m (gcd e m)))))))
    

;; forge a message between s (sender) and r (recipient)
;; given only their public keys
(define (forge-message message s-public-key r-public-key)
        (encrypt-and-sign message (crack-rsa s-public-key) r-public-key))



 