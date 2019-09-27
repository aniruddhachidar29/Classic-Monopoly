#lang racket
(require "utilities-for-color-sim.rkt")


(define master_chest '(0 40 40 40 40 10 40 40 40 40 40 40 40 40 40 40))
(define master_chance '(0 24 11 #\U #\R 40 40 #\B 10 40 40 5 39 40 40 40))
(define chest (shuffle master_chest))
(define chance (shuffle master_chance))
(define (not-bankrupt player)
  (> (send player wealth) 0))

  ; (or (not (null? (get-field list-of-properties player)))  (> (get-field balance player) 0)))
(define t (expt 10 4)) ;;;can change the number of simulations to be run here
 

(define color-hash (make-hash  (zip '("orange" "yellow" "brown" "light blue" "red" "pink" "green" "blue") '(0 0 0 0 0 0 0 0))))

(time (while (> t 0)
       
       (define player1 (make-object game-player% "player1"))
       (define player2 (make-object game-player% "player2"))

       (define pos1 0)
       (define pos2 0)
       (define (diceroll) (+ (random 1 7) (random 1 7)))
       ;(define diceroll2 (+ (random 1 7) (random 1 7)))
       
       (while (and (and (not-bankrupt player1) (not-bankrupt player2)) (and (< (get-field balance player1) 10000) (< (get-field balance player1) 10000)))
              (let ((nextpos (modulo (+ pos1 (diceroll)) 40)))
                (if (> (- nextpos pos1) 0)
                    (set! pos1 nextpos)
                    (begin (set! pos1 nextpos)
                           (set-field! balance player1 (+ (get-field balance player1) 200)))))
              (cond  ((equal? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos1))) '(cons 272.5 908.5))
                      (send player1 pay-tax))
                     ((equal? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos1))) '(cons 88.5 574.5))
                      (send player1 supertax))
                     (else (void 1)))
              (cond ((string=? "" (get-field property-owner (hash-ref boxes (list-ref str-lst pos1))))
                     (if (= (random 0 2) 0)
                         (void)
                         (send player1 purchase (hash-ref boxes (list-ref str-lst pos1)))))            
                    ((string=? "player1" (get-field property-owner (hash-ref boxes (list-ref str-lst pos1))))
                     (let ((a (random 0 3)))
                       (cond ((= a 0) (void))
                             ((= a 1) (send player1 upgrade-property (hash-ref boxes (list-ref str-lst pos1))))
                             (else (send player1 mortgage (hash-ref boxes (list-ref str-lst pos1)))))))
                    ((string=? "player2" (get-field property-owner (hash-ref boxes (list-ref str-lst pos1))))
                     (begin (set-field! balance player1
                                        (- (get-field balance player1)
                                           (get-field rent (hash-ref boxes (list-ref str-lst pos1)))))
                            (set-field! balance player2
                                        (+ (get-field balance player2)
                                           (get-field rent (hash-ref boxes (list-ref str-lst pos1))))))))
              (cond [(member? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos1)))
                              '((cons 88.5 349.5) (cons 906.5 274.5) (cons 422.5 908.5))) ;;chance box
                     (begin (define chance_card (list-ref chance 0))
                            (set! chance (cdr chance))
                    
                            (cond [(= (length chance) 0)
                                   (begin (set! chance (shuffle master_chance)))]
                                  [(not (equal? chance_card 40))
                                   (if (number? chance_card) (set! pos1 chance_card)
                                       (cond [(equal? chance_card #\U)
                                              (begin (define test 1)
                                                     (while (not (member? pos1 '(12 28)))
                                                            (set! pos1 (modulo (+ pos1 1) 40))
                                                            (set! test 0)))]
                                             [(equal? chance_card #\R)
                                              (begin (define test2 1)
                                                     (while (not (member? pos1 '(5 15 25 35)))
                                                            (set! pos1 (modulo (+ pos1 1) 40))
                                                            (set! test2 0)))]
                                             [(equal? chance_card #\B)
                                              (set! pos1 (- pos1 3))]))]))]
                    [(member? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos1))) '((cons 88.5 724.5) (cons 647.5 90.5) (cons  647.5 908.5))) ;;community chest
                     (begin (define chest_card (list-ref chest 0))
                            (set! chest (cdr chest))
                            (cond [(= (length chest) 0)
                                   (begin (set! chest (shuffle master_chest)))]
                                  [(not (= chest_card 40)) (set! pos1 chest_card)]))]
                    [(= pos1 30) ;;go to jail
                     (begin (set! pos1 10)
                            (set-field! balance player1 (- (get-field balance player1) 50)))])
              (let ((nextpos (modulo (+ pos2 (diceroll)) 40)))
                (if (> (- nextpos pos2) 0)
                    (set! pos2 nextpos)
                    (begin (set! pos2 nextpos)
                           (set-field! balance player2 (+ (get-field balance player2) 200)))))
              (cond  ((equal? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos2))) '(cons 272.5 908.5))
                      (send player2 pay-tax))
                     ((equal? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos1))) '(cons 88.5 574.5))
                      (send player2 supertax))
                     (else (void 1)))
              (cond ((string=? "" (get-field property-owner (hash-ref boxes (list-ref str-lst pos2))))
                     (if (= (random 0 2) 0)
                         (void)
                         (send player2 purchase (hash-ref boxes (list-ref str-lst pos2)))))            
                    ((string=? "player2" (get-field property-owner (hash-ref boxes (list-ref str-lst pos2))))
                     (let ((a (random 0 3)))
                       (cond ((= a 0) (void))
                             ((= a 1) (send player2 upgrade-property (hash-ref boxes (list-ref str-lst pos2))))
                             (else (send player2 mortgage (hash-ref boxes (list-ref str-lst pos2)))))))
                    ((string=? "player1" (get-field property-owner (hash-ref boxes (list-ref str-lst pos2))))
                     (begin (set-field! balance player2
                                        (- (get-field balance player2)
                                           (get-field rent (hash-ref boxes (list-ref str-lst pos2)))))
                            (set-field! balance player1
                                        (+ (get-field balance player1)
                                           (get-field rent (hash-ref boxes (list-ref str-lst pos2))))))))
              (cond [(member? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos2)))
                              '((cons 88.5 349.5) (cons 906.5 274.5) (cons 422.5 908.5))) ;;chance box
                     (begin (define chance_card (list-ref chance 0))
                            (set! chance (cdr chance))
                    
                            (cond [(= (length chance) 0)
                                   (begin (set! chance (shuffle master_chance)))]
                                  [(not (equal? chance_card 40))
                                   (if (number? chance_card) (set! pos2 chance_card)
                                       (cond [(equal? chance_card #\U)
                                              (begin (define test 1)
                                                     (while (not (member? pos2 '(12 28)))
                                                            (set! pos2 (modulo (+ pos2 1) 40))
                                                            (set! test 0)))]
                                             [(equal? chance_card #\R)
                                              (begin (define test2 1)
                                                     (while (not (member? pos2 '(5 15 25 35)))
                                                            (set! pos2 (modulo (+ pos2 1) 40))
                                                            (set! test2 0)))]
                                             [(equal? chance_card #\B)
                                              (set! pos2 (- pos2 3))]))]))]
                    [(member? (get-field co-ordinates (hash-ref boxes (list-ref str-lst pos2))) '((cons 88.5 724.5) (cons 647.5 90.5) (cons  647.5 908.5))) ;;community chest
                     (begin (define chest_card (list-ref chest 0))
                            (set! chest (cdr chest))
                            (cond [(= (length chest) 0)
                                   (begin (set! chest (shuffle master_chest)))]
                                  [(not (= chest_card 40)) (set! pos2 chest_card)]))]
                    [(= pos2 30) ;;go to jail
                     (begin (set! pos2 10)
                            (set-field! balance player2 (- (get-field balance player2) 50)))]))
       (define (winner-decider player1 player2)
         (define winner (filter (lambda (x) (not-bankrupt x)) (list player1 player2)))
         (if (= 2 (length winner)) (if (> (send player1 wealth) (send player2 wealth)) player1 player2)
             (car winner)))
       (define winner (winner-decider player1 player2))
       (define winner-color-list (filter (lambda (y) (not (or (equal? y "white") (equal? y 0))))
                                         (map (lambda (x) (get-field color (hash-ref boxes (hash-ref tagged-hash x))))
                                              (get-field list-of-properties winner))))
       (define orange-frequency (frequency-str "orange" winner-color-list))
       (define yellow-frequency (frequency-str "yellow" winner-color-list))
       (define brown-frequency (frequency-str "brown" winner-color-list))
       (define light-blue-frequency (frequency-str "light blue" winner-color-list))
       (define red-frequency (frequency-str "red" winner-color-list))
       (define pink-frequency (frequency-str "pink" winner-color-list))
       (define green-frequency  (frequency-str "green" winner-color-list))
       (define blue-frequency  (frequency-str "blue" winner-color-list))
       (hash-set! color-hash "orange" (+ (hash-ref color-hash "orange") orange-frequency))
       (hash-set! color-hash "yellow" (+ (hash-ref color-hash "yellow") yellow-frequency))
       (hash-set! color-hash "brown" (+ (hash-ref color-hash "brown") brown-frequency))
       (hash-set! color-hash "light blue" (+ (hash-ref color-hash "light blue") light-blue-frequency))
       (hash-set! color-hash "red" (+ (hash-ref color-hash "red") red-frequency))
       (hash-set! color-hash "pink" (+ (hash-ref color-hash "pink") pink-frequency))
       (hash-set! color-hash "green" (+ (hash-ref color-hash "green") green-frequency))
       (hash-set! color-hash "blue" (+ (hash-ref color-hash "blue") blue-frequency))
        
;       (define color (mode winner-properties))
;       (displayln color)
;       (displayln (get-field playername winner))
       
      (set! t (- t 1))))

(displayln color-hash)
;;PLEASE READ THIS TO UNDERSTAND THE CONCLUSION OF THE SIMULATOR.
;; the number represent here the count of each property color which lead to the victory
;;for example in the result of 1000 simulations the purchase of the red sets in the game lead to the victory 309 times..
;; for 1000 simulations of the game color trends are as follows
;#hash((blue . 149) (brown . 164) (green . 281) (light blue . 239) (orange . 280) (pink . 274) (red . 309) (yellow . 292))

;; for 10000 simulations of the game color trends are as follows
;#hash((blue . 856) (brown . 1092) (green . 1591) (light blue . 1487) (orange . 1660) (pink . 1665) (red . 1933) (yellow . 1703))

;; for 100000 simulations of the game color trends are as follows
;#hash((blue . 7344) (brown . 9895) (green . 14257) (light blue . 12363) (orange . 14906) (pink . 15074) (red . 17112) (yellow . 15548))

;; for 1000000 simulations of the game color trends are as follows
;#hash((blue . 72787) (brown . 98432) (green . 141453) (light blue . 123429) (orange . 148394) (pink . 150795) (red . 170005) (yellow . 154535))

;; for 10000000 simulations of the game color trends are as follows
;#hash((blue . 724352) (brown . 980135) (green . 1412222) (light blue . 1229305) (orange . 1484883) (pink . 1508495) (red . 1698056) (yellow . 1543222))

;; there is a trend to this data collected which can be clearly observed that the winner of the game most likely must have the red, yellow ,pink and orange sets in most of it victories..
;;which leads into the direction which color sets in a two player game are more likely lead to a victory in the game.


      










       
       
             
