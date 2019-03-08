 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                           Exercise 0                          ;;
 ;;                                                               ;;
 ;; Create an object that every time it's called, flips its state ;;
 ;;                        between 0 and 1                        ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ok
(define (make-flip)
  (let ((count 0))
  (lambda ()
    (if (= count 0)
      (set! count 1)
      (set! count 0))
      count
     )))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                               Exercise 1                              ;;
 ;;                                                                       ;;
 ;; Install new character with free will, create late-homework object and ;;
 ;;                    start character in dormitory.                      ;;
 ;;                                                                       ;;
 ;;   Also create a function that finds gerry and gives him the homework  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ok
(define myself
  (make&install-person 'myself dormitory 100))
;;ok
(define late-homework
  (make&install-thing 'late-homework dormitory))
;;ok
(define (give-gerry-my-homework)
   (ask myself 'take late-homework)
  (ask myself 'go 'west)
  (ask myself 'go 'north)
  (ask myself 'go 'up)
  (ask myself 'go 'up)
  (ask myself 'lose late-homework)
  (ask gerry 'take late-homework)
  )


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                       Exercise 2                       ;;
 ;;                                                        ;;
 ;; Creates a card locked place (overriding accept-person) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ok
(define (check-possessions-for-type list type)
  (cond ((null? list) #f)
        ((is-a (car list) type) #t)
        (else (check-possessions-for-type (cdr list) type))))
;;ok
(define (make-card-locked-place name)
  (let ((place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
             (lambda (self person)
               (check-possessions-for-type (ask person 'possessions) 'sd-card?)))
             (else (get-method place message))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                             Exercise 3                            ;;
  ;;                                                                   ;;
  ;; Create student-residence class that accepts only persons carrying ;;
  ;;                        cards with valid IDs                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ok
(define (check-list-for-value list value)
  (cond ((null? list) #f)
        ((eq? (car list) value) #t)
        (else (check-list-for-value (cdr list) value))))

;;ok
(define (check-possessions-for-specific-item-belonging-to-ids possessions-list type property ids)
  (cond ((null? possessions-list) #f)
        ((is-a (car possessions-list) type)
         (if (check-list-for-value ids (ask (car possessions-list) property))
             #t
             (check-possessions-for-specific-item-belonging-to-ids (cdr possessions-list) type property ids)
               ))
        (else (check-possessions-for-specific-item-belonging-to-ids (cdr possessions-list) type property ids))
        )
  )

;;ok
(define (append-item-in-list items value)
  (if (null? items)
      (list value)
      (append items (list value)))
  )

;;ok
(define (make-student-residence name)
  (let ((place (make-card-locked-place name))
        (ids '()))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
             (lambda (self person)
               (check-possessions-for-specific-item-belonging-to-ids (ask person 'possessions) 'sd-card? 'id ids)
               )
             )
             ((eq? message 'register-card)
              (lambda (self card)
                 (cond ((eq? (ask card 'place) self)
                        (set! ids (append-item-in-list ids (ask card 'id)))
                        #t)
                     (else #f))
                ))
             ((eq? message 'ids)
             (lambda (self)
               ids
               )
             )
             (else (get-method place message))
             )
      )
    )
  )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                             Exercise 4                             ;;
 ;;                                                                    ;;
 ;;              Create ogre that hunts specific card id.              ;;
 ;; Also create procedure that reports a stolen card and start an ogre ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ok
(define
  (check-person-for-item-belonging-to-stolen-ids possessions-list type property value)
  (cond ((null? possessions-list) #f)
        ((is-a (car possessions-list) type)
         (if (eq? (ask (car possessions-list) property) value)
             #t
             (check-person-for-item-belonging-to-stolen-ids (cdr possessions-list) type property value))
         )              
        (else (check-person-for-item-belonging-to-stolen-ids (cdr possessions-list) type property value))
        )
  )

;;ok
(define
  (check-people-list-for-person-with-stolen-id others stolen-id)
  (if (null? others)
      null
        (if (check-person-for-item-belonging-to-stolen-ids (ask (car others) 'possessions) 'sd-card? 'id stolen-id)
             (car others)
             (check-people-list-for-person-with-stolen-id (cdr others) stolen-id))))
             

;;
(define (make-ogre name birthplace threshold card-id)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
        (lambda (self)
	      (let ((others (other-people-at-place self (ask self 'place))))
		 (if (not (null? others))
              (let ((person1 (check-people-list-for-person-with-stolen-id others card-id)))
                (if (not (null? person1))
                           (ask self 'eat-person person1)
                           ((get-method person 'act) self)
                           ))
              ((get-method person 'act) self)
                     )
                )
                 )
               )
	    ((eq? message 'eat-person)
	     (lambda (self person)
	       (ask self 'say
		    (list "Thief!!!!! I'm going to eat you,"
			  (ask person 'name)))
	       (go-to-heaven person)
	       (ask self 'say
		    (list "Chomp chomp." (ask person 'name)
			  "tastes yummy!"))
	       '*burp*))
	    (else (get-method person message))))))

(define (make&install-ogre name birthplace threshold card-id)
 (let ((ogre (make-ogre name birthplace threshold card-id)))
   (ask ogre 'install)
   ogre))

(define (report-stolen-card id)
 (make&install-ogre (string->symbol (apply string-append (list "ogre" (symbol->string id)))) dungeon 1 id))


             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;;                  Exercise 5                 ;;
             ;;                                             ;;
             ;; Implement big-brother and surveillance-room ;;
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (return-possessions-for-specific-item-belonging-to-ids possessions-list type property ids)
  (cond ((null? possessions-list) null)
        ((is-a (car possessions-list) type)
         (if (check-list-for-value ids (ask (car possessions-list) property))
             (ask (car possessions-list) property)
             (check-possessions-for-specific-item-belonging-to-ids (cdr possessions-list) type property ids)))
        (else (check-possessions-for-specific-item-belonging-to-ids (cdr possessions-list) type property ids))))

(define (check-list-for-eq-val list val)
  (cond ((null? list) #f)
        ((equal? (car list) val) #t)
        (else (check-list-for-value (cdr list) val))))

(define (get-last-entry list proc1 val1 proc2 val2 last)
  (if (null? list)
      last
      (if (and (eq? (proc1 (car list)) val1) (eq? (proc2 (car list)) val2))
          (get-last-entry (cdr list) proc1 val1 proc2 val2 (car list))
          (get-last-entry (cdr list) proc1 val1 proc2 val2 last)
          )
      )
  )

(define (make-big-brother name)
 (let ((named-obj (make-named-object name))
        (logs '())
        (stolen-cards '()))
    (lambda (message)
      (cond ((eq? message 'inform)
            (lambda (self place card-id)
              (cond ((check-list-for-eq-val logs (list place (current-time) card-id))
                      (set! stolen-cards (append-item-in-list stolen-cards card-id))
                      (report-stolen-card card-id)
                      )
                     (else (set! logs (append-item-in-list logs (list place (current-time) card-id))))
                     )
               ))
            ((eq? message 'display-stolen-card)
             (lambda (self)
               stolen-cards
               )
             )  
            (else (get-method named-obj message))))))


(define (make-surveillance-room name big-brother)
  (let ((student-residence (make-student-residence name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
            (lambda (self person)
              (cond ((ask student-residence 'accept-person? person)
                      (ask big-brother 'inform student-residence (return-possessions-for-specific-item-belonging-to-ids (ask person 'possessions) 'sd-card? 'id (ask student-residence 'ids)))
                      #t
                      )
                     (else #f)
                     )
               )
             )
            (else (get-method student-residence message))))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                              Exercise 6                              ;;
 ;;                                                                      ;;
 ;; Create class secret that extends thing and when taken opens a secret ;;
 ;;            passage way between dormitory and Tech Square             ;;
 ;;                                                                      ;;
 ;;  Also create object suspicious-book and install it in the dormitory  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-secret name birthplace)
  (let ((thing (make-thing name birthplace)))
    (lambda (message)
      (cond 
	    ((eq? message 'set-owner)
	     (lambda (self new-owner)
	       (ask thing 'set-owner new-owner)
	        (can-go-both-ways dormitory 'north  'east  Tech-Square)
               (ask new-owner 'say (list "A new passage opens north!"))
               ))
	    (else (get-method thing message))))))

(define (make&install-secret name birthplace)
   (let ((secret (make-secret name birthplace)))
   (ask secret 'install)
   secret))

(define suspicious-book
  (make&install-secret 'suspicious-book dormitory))

