#lang racket

;;; ---------------------------------------------------------------------------
;;; Simple object system with inheritance

(define (ask object message . args)  ;; See your Scheme manual to explain `.'
  (let ((method (get-method object message)))
    (if (method? method)
	(apply method (cons object args))
	(error "No method" message (cadr method)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (method? x)
  (not (no-method? x)))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      false))

;;; ----------------------------------------------------------------------------
;;; Persons, places, and things will all be kinds of named objects

(define (make-named-object name)
  (lambda (message) 
    (cond ((eq? message 'name) (lambda (self) name))
	  (else (no-method name)))))

;;; Persons and things are mobile since their places can change

(define (make-mobile-object name location)
  (let ((named-obj (make-named-object name)))
    (lambda (message)
      (cond ((eq? message 'place)    (lambda (self) location))
	    ((eq? message 'install)
	     (lambda (self)
	       (ask location 'add-thing self)))	; Synchonize thing and place
	    ;; Following method should never be called by the user...
	    ;;  it is a system-internal method.
	    ;; See CHANGE-PLACE instead
	    ((eq? message 'set-place)
	     (lambda (self new-place)
	       (set! location new-place)
	       'place-set))
	    (else (get-method named-obj message))))))

(define (make&install-mobile-object name place)
  (let ((mobile-obj (make-mobile-object name place)))
    (ask mobile-obj 'install)
    mobile-obj))

;;; A thing is something that can be owned

(define (make-thing name birthplace)
  (let ((owner     'nobody)
	(mobile-obj (make-mobile-object name birthplace)))
    (lambda (message)
      (cond ((eq? message 'owner)    (lambda (self) owner))
	    ((eq? message 'ownable?) (lambda (self) true))
	    ((eq? message 'owned?)
	     (lambda (self)
	       (not (eq? owner 'nobody))))
	    ;; Following method should never be called by the user...
	    ;;  it is a system-internal method.
	    ;; See TAKE and LOSE instead.
	    ((eq? message 'set-owner)
	     (lambda (self new-owner)
	       (set! owner new-owner)
	       'owner-set))
	    (else (get-method mobile-obj message))))))

(define (make&install-thing name birthplace)	
  (let ((thing  (make-thing name birthplace)))
    (ask thing 'install)
    thing))

;;; Implementation of places

(define (make-place name)
  (let ((neighbor-map '())		
	(things       '())
	(named-obj (make-named-object name)))
    (lambda (message)
      (cond ((eq? message 'things) (lambda (self) things))
	    ((eq? message 'neighbors)
	     (lambda (self) (map cdr neighbor-map)))
	    ((eq? message 'exits)
	     (lambda (self) (map car neighbor-map)))
	    ((eq? message 'neighbor-towards)
	     (lambda (self direction)
	       (let ((places (assq direction neighbor-map)))
		 (if places
		     (cdr places)
		     false))))
            ((eq? message 'add-neighbor)
             (lambda (self direction new-neighbor)
               (cond ((assq direction neighbor-map)
                      (display-message (list "Direction already assigned"
					      direction name))
		      false)
                     (else
                      (set! neighbor-map
                            (cons (cons direction new-neighbor) neighbor-map))
		      true))))
	    ((eq? message 'accept-person?)
	     (lambda (self person)
	       true))
 
	    ;; Following two methods should never be called by the user...
	    ;;  they are system-internal methods. See CHANGE-PLACE instead.
            ((eq? message 'add-thing)
             (lambda (self new-thing)
               (cond ((memq new-thing things)
                      (display-message (list (ask new-thing 'name)
					     "is already at" name))
		      false)
                     (else (set! things (cons new-thing things))
			   true))))
            ((eq? message 'del-thing)
             (lambda (self thing)
               (cond ((not (memq thing things))
                      (display-message (list (ask thing 'name)
					     "is not at" name))
		      false)
                     (else (set! things (delq thing things))	;; DELQ defined
			   true))))                             ;; below

            (else (get-method named-obj message))))))

;;; ----------------------------------------------------------------------------
;;; Implementation of people

(define (make-person name birthplace threshold)
  (let ((possessions '())
	(mobile-obj  (make-mobile-object name birthplace)))
    (lambda (message)
      (cond ((eq? message 'person?)     (lambda (self) true))
	    ((eq? message 'possessions) (lambda (self) possessions))
	    ((eq? message 'list-possessions)
	     (lambda (self)
	       (ask self 'say
		    (cons "I have"
			  (if (null? possessions)
			      '("nothing")
			      (map (lambda (p) (ask p 'name))
				      possessions))))
	       possessions))
	    ((eq? message 'say)
	     (lambda (self list-of-stuff)
	       (display-message
		 (append (list "At" (ask (ask self 'place) 'name)
			       ":"  name "says --")
			 (if (null? list-of-stuff)
			     '("Oh, nevermind.")
			     list-of-stuff)))
	       'said))
	    ((eq? message 'have-fit)
	     (lambda (self)
	       (ask self 'say '("Yaaaah! I am upset!"))
	       'I-feel-better-now))
	    ((eq? message 'look-around)
	     (lambda (self)
	       (let ((other-things
		       (map (lambda (thing) (ask thing 'name))
                               (delq self                       ;; DELQ
                                     (ask (ask self 'place)     ;; defined
                                          'things)))))          ;; below
                 (ask self 'say (cons "I see" (if (null? other-things)
						  '("nothing")
						  other-things)))
		 other-things)))

	    ((eq? message 'take)
	     (lambda (self thing)
	       (cond ((memq thing possessions)
		      (ask self 'say
			   (list "I already have" (ask thing 'name)))
		      true)
		     ((and (let ((things-at-place (ask (ask self 'place) 'things)))
			     (memq thing things-at-place))
			   (is-a thing 'ownable?))
		      (if (ask thing 'owned?)
			  (let ((owner (ask thing 'owner)))
			    (ask owner 'lose thing)
			    (ask owner 'have-fit))
			  'unowned)

		      (ask thing 'set-owner self)
		      (set! possessions (cons thing possessions))
		      (ask self 'say
			   (list "I take" (ask thing 'name)))
		      true)
		     (else
		      (display-message
		       (list "You cannot take" (ask thing 'name)))
		      false))))
	    ((eq? message 'lose)
	     (lambda (self thing)
	       (cond ((eq? self (ask thing 'owner))
		      (set! possessions (delq thing possessions)) ;; DELQ
		      (ask thing 'set-owner 'nobody)              ;; defined
		      (ask self 'say                              ;; below
			   (list "I lose" (ask thing 'name)))
		      true)
		     (else
		      (display-message (list name "does not own"
					     (ask thing 'name)))
		      false))))
	    ((eq? message 'move)
	     (lambda (self)
	       (cond ((= (random threshold) 0)
		      (ask self 'act)
		      true))))
	    ((eq? message 'act)
	     (lambda (self)
	       (let ((new-place (random-neighbor (ask self 'place))))
		 (if new-place
		     (ask self 'move-to new-place)
		     false))))		; All dressed up and no place to go

	    ((eq? message 'move-to)
	     (lambda (self new-place)
	       (let ((old-place (ask self 'place)))
		 (cond ((eq? new-place old-place)
			(display-message (list name "is already at"
					       (ask new-place 'name)))
			false)
		       ((ask new-place 'accept-person? self)
			(change-place self new-place)
			(for-each (lambda (p) (change-place p new-place))
				  possessions)
			(display-message
			  (list name "moves from" (ask old-place 'name)
				     "to"         (ask new-place 'name)))
			(greet-people self (other-people-at-place self new-place))
			true)
		       (else
			(display-message (list name "can't move to"
					       (ask new-place 'name))))))))
	    ((eq? message 'go)
	     (lambda (self direction)
	       (let ((old-place (ask self 'place)))
		 (let ((new-place (ask old-place 'neighbor-towards direction)))
		   (cond (new-place
			  (ask self 'move-to new-place))
			 (else
			  (display-message (list "You cannot go" direction
						 "from" (ask old-place 'name)))
			  false))))))
	    ((eq? message 'install)
	     (lambda (self)
	       (add-to-clock-list self)
	       ((get-method mobile-obj 'install) self)))
	    (else (get-method mobile-obj message))))))
  
(define (make&install-person name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (ask person 'install)
    person))

;;; A troll is a kind of person (but not a kind person!)

(define (make-troll name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self)
	       (let ((others (other-people-at-place self (ask self 'place))))
		 (if (not (null? others))
		     (ask self 'eat-person (pick-random others))
		     ((get-method person 'act) self)))))
	    ((eq? message 'eat-person)
	     (lambda (self person)
	       (ask self 'say
		    (list "Growl.... I'm going to eat you,"
			  (ask person 'name)))
	       (go-to-heaven person)
	       (ask self 'say
		    (list "Chomp chomp." (ask person 'name)
			  "tastes yummy!"))
	       '*burp*))
	    (else (get-method person message))))))

(define (make&install-troll name birthplace threshold)
  (let ((troll  (make-troll name birthplace threshold)))
    (ask troll 'install)
    troll))


(define (go-to-heaven person)
  (for-each (lambda (item) (ask person 'lose item))
	    (ask person 'possessions))
  (ask person 'say
       '("
                   Dulce et decorum est 
                   pro computatore mori!"
	 ))
  (ask person 'move-to heaven)
  (remove-from-clock-list person)
  'game-over-for-you-dude)

(define heaven (make-place 'heaven))		; The point of no return

;;; --------------------------------------------------------------------------
;;; Clock routines

(define *clock-list* '())
(define *the-time* 0)

(define (initialize-clock-list)
  (set! *clock-list* '())
  'initialized)

(define (add-to-clock-list person)
  (set! *clock-list* (cons person *clock-list*))
  'added)

(define (remove-from-clock-list person)
  (set! *clock-list* (delq person *clock-list*))  ;; DELQ defined below
  'removed)

(define (clock)
  (newline)
  (display "---Tick---")
  (set! *the-time* (+ *the-time* 1))
  (for-each (lambda (person) (ask person 'move))
	    *clock-list*)
  'tick-tock)
	     

(define (current-time)
  *the-time*)

(define (run-clock n)
  (cond ((zero? n) 'done)
	(else (clock)
	      (run-clock (sub1 n)))))

;;; --------------------------------------------------------------------------
;;; Miscellaneous procedures

(define (is-a object property)
  (let ((method (get-method object property)))
    (if (method? method)
	(ask object property)
	false)))

(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1)
                                 (symbol->string s2))))

(define (change-place mobile-object new-place)	; Since this bridges the gap
  (let ((old-place (ask mobile-object 'place))) ; between MOBILE-OBJECT and
    (ask mobile-object 'set-place new-place)	; PLACE, it is best it not
    (ask old-place 'del-thing mobile-object))	; be internal to either one.
  (ask new-place 'add-thing mobile-object)
  'place-changed)

(define (other-people-at-place person place)
  (filter (lambda (object)
	    (if (not (eq? object person))
		(is-a object 'person?)
		false))
	  (ask place 'things)))

(define (greet-people person people)
  (if (not (null? people))
      (ask person 'say
	   (cons "Hi"
		 (map (lambda (p) (ask p 'name))
			 people)))
      'sure-is-lonely-in-here))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
	    list-of-stuff)
  'message-displayed)

(define (random-neighbor place)
  (pick-random (ask place 'neighbors)))

(define (filter predicate lst)
  (cond ((null? lst) '())
	((predicate (car lst))
	 (cons (car lst) (filter predicate (cdr lst))))
	(else (filter predicate (cdr lst)))))

(define (pick-random lst)
  (if (null? lst)
      false
      (list-ref lst (random (length lst)))))  ;; See manual for LIST-REF

(define (delq item lst)
  (cond ((null? lst) '())
	((eq? item (car lst)) (delq item (cdr lst)))
	(else (cons (car lst) (delq item (cdr lst))))))

;;; -------------------------------------------------------------------
;;; Other interesting procedures for PS6, Spring 95

(define (make&install-sd-card name birthplace id)
  (let ((card (make-sd-card name birthplace id)))
    (ask card 'install)
    card))

(define (make-sd-card name birthplace idnumber)
  (let ((id idnumber)
        (thing (make-thing name birthplace)))
    (lambda (message)
      (cond ((eq? message 'sd-card?) (lambda (self) true))
            ((eq? message 'id) (lambda (self) id))
            (else (get-method thing message))))))

(define (copy-sd-card card)
  (let ((name   (symbol-append 'copy-of- (ask card 'name)))
	(place  (ask card 'place))
	(id     (ask card 'id)))
    (make&install-sd-card name place id)))




(initialize-clock-list)

;; Here we define the places in our world...
;;------------------------------------------

(define EGG-Atrium   (make-place 'EGG-Atrium))
(define dungeon      (make-place 'dungeon))
(define Building-36  (make-place 'Building-36))
(define computer-lab (make-place 'computer-lab))
(define Tech-Square  (make-place 'Tech-Square))
(define gerry-office  (make-place 'gerry-office))
(define albert-office  (make-place  'albert-office))
(define dormitory    (make-place 'dormitory))

;; One-way paths connect individual places in the world.
;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))

(can-go-both-ways Building-36   'up    'down  computer-lab)
(can-go-both-ways Building-36   'north 'south Tech-Square)
(can-go-both-ways Building-36   'west  'east  EGG-Atrium)
(can-go-both-ways Tech-Square   'up    'down  albert-office)
(can-go-both-ways albert-office 'up    'down  gerry-office)
(can-go-both-ways dormitory     'west  'east  Building-36)

(can-go dungeon      'up    EGG-Atrium)

;; The important critters in our world...
;;---------------------------------------

(define albert   (make&install-person 'albert albert-office 3))
(define gerry    (make&install-person 'gerry  gerry-office  2))

(define grendel  (make&install-troll  'grendel dungeon     4))

(define gerry-card
  (make&install-sd-card 'gerry-card gerry-office '888-12-3456))
(define albert-card
  (make&install-sd-card 'albert-card albert-office '888-98-7654))

;; The beginning of an ever-expanding game script
;;------------------------------------------------

(include "ps4.scm")

;; in order to test ex 2
(define card-place (make-card-locked-place 'card-place))
(can-go-both-ways card-place 'up 'down Building-36)
(define card1 (make&install-sd-card 'card1 dormitory '1234))

;; in order to test ex3
(define safe-residence (make-student-residence 'safe-residence))
(define residence-card (make&install-sd-card 'residence-card safe-residence 'mit-7217))
(can-go-both-ways safe-residence 'up 'down card-place)
(define forged-residence-card (make&install-sd-card 'forged-residence-card card-place 'mit-7217))

;; in order to test ex5
(define big-eye (make-big-brother 'big-eye))
(define very-safe-place (make-surveillance-room 'safe-dormitory big-eye))
(can-go-both-ways very-safe-place 'west 'east dormitory)
(define my-buddy (make&install-person 'my-buddy very-safe-place 3))
(define very-safe-card-forged (make&install-sd-card 'very-safe-card-forged dormitory 'mit-7280))
(define very-safe-card (make&install-sd-card 'very-safe-card very-safe-place 'mit-7280))


(define (play-game)
  (ask gerry 'take gerry-card)
  (ask gerry 'go 'down)
  (ask gerry 'go 'down)
  )

(define (get-in-surveillance)
  (ask myself 'go 'east) ;; there is a residence cand in Card place
  (ask very-safe-place 'register-card very-safe-card)
  (ask my-buddy 'take very-safe-card)
  (ask my-buddy 'go 'west)
  (ask myself 'take very-safe-card-forged)
  (ask myself 'go 'east)
  (ask my-buddy 'go 'east)
  (ask big-eye 'display-stolen-card)
  )
;; ...now whenever you re-load this file, you can bring things up to
;; date by invoking PLAY-GAME.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                                         ;;
;; At dormitory : myself says -- I take late-homework      ;;
;; myself moves from dormitory to Building-36              ;;
;; myself moves from Building-36 to Tech-Square            ;;
;; myself moves from Tech-Square to albert-office          ;;
;; At albert-office : myself says -- Hi albert             ;;
;; myself moves from albert-office to gerry-office         ;;
;; At gerry-office : myself says -- Hi gerry               ;;
;; At gerry-office : myself says -- I lose late-homework   ;;
;; At gerry-office : gerry says -- I take late-homework #t ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(give-gerry-my-homework)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPECTED OUTPUT                                             ;;
;; myself can't move to safe-dormitory                         ;;
;; At safe-dormitory : my-buddy says -- I take very-safe-card  ;;
;; my-buddy moves from safe-dormitory to dormitory             ;;
;; At dormitory : my-buddy says -- Hi myself                   ;;
;; At dormitory : myself says -- I take very-safe-card-forged  ;;
;; myself moves from dormitory to safe-dormitory               ;;
;; my-buddy moves from dormitory to safe-dormitory             ;;
;; At safe-dormitory : my-buddy says -- Hi myself '(mit-7280)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(get-in-surveillance)

(give-gerry-my-homework)
'----
(define flip (make-flip))
(flip)
(flip)
(flip)

;(define icecream (make-thing 'ice-cream dormitory))
;(define b (icecream 'set-owner))
;(define c (b 'icecream gerry))
'----
'----
;;Question 4
(define icecream (make-thing 'icecream dormitory))
(ask icecream 'install)
(define rum-and-raisin (make-named-object 'icecream))
(ask dormitory 'add-thing rum-and-raisin)
(ask myself 'move-to dormitory)
(ask myself 'look-around)
(eq? icecream rum-and-raisin)
'----
;;Exercise 3
(ask myself 'possessions)
(ask myself 'take icecream)
(ask myself 'list-possessions)
(define sd-card1 (make&install-sd-card 'sd-card dormitory 1234))
(ask myself 'take sd-card1)
(ask (car (ask myself 'list-possessions)) 'id)
