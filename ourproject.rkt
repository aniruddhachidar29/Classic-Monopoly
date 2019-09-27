#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(require "classes.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;  BIG BANG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct board-world (image goti dice turnplayer rolldice))

(define (start s)
  (place-image startimg 500 500 (empty-scene 1000 1000)))
(define gameoverimg (bitmap "images/game over.jpg"))
(define boardimg (bitmap "images/boardmain.jpg"))  ;;"/home/aniruddhachidar/Desktop/PROJECT/project/boardmain.jpg"
;;(define test (bitmap "project/monopoly-online.jpg"))
(define startimg (bitmap "images/start1.jpg"))
(define rulesimg (bitmap "images/rules.jpg"))
(define image (list boardimg))
(define buyimg (bitmap "images/(buy,pass and exit).jpg"))
(define upgradeimg (bitmap "images/(upgrade and mortgage).jpg"))
;(define chance (bitmap "project/chnace.jpg"))
(define mainbackground (bitmap "images/main background sized.jpg"))
(define scene (place-image mainbackground
                           500 500
                           (empty-scene 1000 1000)))

  

(define (change-world-image image background)
  (cond ((equal? image 0) boardimg)
  (else (place-image image 500 500 background))))

(define (merge-photo image background x y)
  (cond ((equal? image 0) 0)
        (else (place-image image x y background))))

(define (monopoly s)
  (if startstate (place-image startimg 500 500 scene)
      (if rulesstate (place-image rulesimg 500 500 scene)
   (if (equal? (car image) gameoverimg) (place-image gameoverimg 500 500 scene)
               (place-image goti2
              (second (first (board-world-goti s)))  (last (first (board-world-goti s)))
   (place-image goti1
              (second (second (board-world-goti s))) (last (second (board-world-goti s)))
    (place-image (car image)
               500 500
               scene)))))))
  
(define rulesstate #f)

(define base-distance 37.5)
(define numberofplayers 3)

(define goti1 (bitmap "images/token1.jpg"))
(define goti2 (bitmap "images/token2.jpg"))
;(define goti3 (circle 7 'outline 'blue))

(define board
  ;(if rulesstate rules-world
  (make-board-world boardimg '((cons 88.5 908.5) (cons 88.5 908.5)) '(0 0) 0 #f)); 1st box size 143 2nd box size 75

(define rules-world
  (make-board-world rulesimg '((cons 88.5 908.5) (cons 88.5 908.5)) '(0 0) 0 #f))

(define (move-path l n ip) ;; 0<= ip <=39 n==dice value ;; ip=(index-of co-ordinate-list (list co-ordinates))
  (let* [(lo (append l l l l))]
  (slice lo (+ ip 1) (+ ip n 1))))

(define (slice l i k)
  (cond [(> i 1) (slice (cdr l) (- i 1) (- k 1))]
        [(< k 1) '()]
        [else (cons (car l) (slice (cdr l) i (- k 1)))]))


(define tags-to-co-ordinates
   (begin (let [(x (build-list 40 add1))]
          (set! x (cons 0 x))
            (set! x (slice x 1 40))
            x)))



(define (roll-dice w a-key)
  
  (define a (random 1 7))
  (define b (random 1 7))
  (define dicenumber (+ a b))
  (set-field! distance (player-object-giver-other (board-world-turnplayer w)) (+ (get-field distance (player-object-giver-other (board-world-turnplayer w))) dicenumber))
  (define list (move-path co-ordinate-list dicenumber (modulo (list-ref (board-world-dice w) (board-world-turnplayer w)) 80)))
  
  (cond ((and (or (string=? a-key " ") (string=? a-key "\r")) (not rulesstate) (not startstate))
         (make-board-world boardimg
                           (list-set (board-world-goti w) (board-world-turnplayer w) (last list))
                           (list-set (board-world-dice w) (board-world-turnplayer w) (+ dicenumber (list-ref (board-world-dice w) (board-world-turnplayer w))))
                           (modulo (+ 1 (board-world-turnplayer w)) 2)
                           #t))
        ((and (or (string=? a-key " ") (string=? a-key "\r")) rulesstate) (set! startstate #t) (set! rulesstate #f)
         (make-board-world startimg
                           (list-set (board-world-goti w) (board-world-turnplayer w) (last list))
                           (list-set (board-world-dice w) (board-world-turnplayer w) (+ dicenumber (list-ref (board-world-dice w) (board-world-turnplayer w))))
                           (modulo (+ 1 (board-world-turnplayer w)) 2)
                           #t))
        (else w)))

(define (do-stuff w)
  (define property (hash-ref boxes (hash-ref tagged-hash (list-ref (board-world-goti w) (turn-return (board-world-turnplayer w))))))
  (cond ((not startstate)
  (cond ((and (equal? (board-world-rolldice w) #t) (equal? '(cons 906.5 908.5) (get-field co-ordinates property)))
         (begin (displayln "You are in jail ")
                (displayln "Pay fine and your position is restored")
                (send (player-object-giver (board-world-turnplayer w)) get-out-of-jail)
                (send (player-object-giver-other (board-world-turnplayer w)) round-completion)
                                                                                                                             
                   (make-board-world boardimg
                     (list-set (board-world-goti w) (- 1 (board-world-turnplayer w)) '(cons 88.5 90.5))
                     (board-world-dice w)
                     (board-world-turnplayer w)
                         #f)))
        ((and (equal? (board-world-rolldice w) #t) (or (equal? '(cons 88.5 724.5) (get-field co-ordinates property))
                                                   (equal? '(cons 647.5 90.5) (get-field co-ordinates property))
                                                   (equal? '(cons 647.5 908.5) (get-field co-ordinates property))
                                                   ))
                       (begin (send (player-object-giver-other (board-world-turnplayer w)) round-completion
                                                                                                                               )
                                     (displayln "Community Chest ")
                                                                                                                         
                              (if (= 0 (random 0 2)) (begin (displayln "You are in jail ")
                                    (displayln "Pay fine and your position is restored")
                                    (send (player-object-giver (board-world-turnplayer w)) get-out-of-jail)
                                    (send (player-object-giver-other (board-world-turnplayer w)) round-completion
                                                                                                                               )
                                 (make-board-world boardimg
                                   (list-set (board-world-goti w) (- 1 (board-world-turnplayer w)) '(cons 88.5 90.5))
                                   (board-world-dice w)
                                   (board-world-turnplayer w)
                                             #f))
                             (begin (send (player-object-giver (board-world-turnplayer w)) community-chest)
                                                                                                                         
                                             (make-board-world boardimg
                                                   (board-world-goti w)
                                                   (board-world-dice w)
                                                   (board-world-turnplayer w)
                                                               #f)))))
        ((and (equal? (board-world-rolldice w) #t) (or (equal? '(cons 88.5 349.5) (get-field co-ordinates property))
                                                   (equal? '(cons 906.5 274.5) (get-field co-ordinates property))
                                                   (equal? '(cons 422.5 908.5) (get-field co-ordinates property))
                                                 ))
                              (begin (send (player-object-giver-other (board-world-turnplayer w)) round-completion
                                                                                                                               )
                                      (displayln "Chance ")
                                                                                                                         
                                      (send (player-object-giver (board-world-turnplayer w)) community-chest)
                                                                                                                         
                                                 (make-board-world boardimg
                                                          (board-world-goti w)
                                                          (board-world-dice w)
                                                          (board-world-turnplayer w)
                                                                  #f)))
        ((and (equal? (board-world-rolldice w) #t) (equal? '(cons 88.5 574.5) (get-field co-ordinates property)))
         (begin  (send (player-object-giver-other (board-world-turnplayer w)) round-completion)
                                                                                                                               
                 (send (player-object-giver (board-world-turnplayer w)) pay-tax)
                                                (make-board-world boardimg
                                                       (board-world-goti w)
                                                       (board-world-dice w)
                                                       (board-world-turnplayer w)
                                                                  #f)))
        ((and (equal? (board-world-rolldice w) #t) (equal? '(cons 88.5 90.5) (get-field co-ordinates property)))
                                                          (begin  (displayln "Just Visiting Jail , No Fine")
                                                                  (newline)
                                                                  (make-board-world boardimg
                                                                         (board-world-goti w)
                                                                         (board-world-dice w)
                                                                         (board-world-turnplayer w)
                                                                                  #f)))
        ((and (equal? (board-world-rolldice w) #t) (equal? '(cons 906.5 90.5) (get-field co-ordinates property)))
                                                           (begin  (displayln "Free Parking! Just Sit And Relax")
                                                                   (newline)
                                                                   (make-board-world boardimg
                                                                        (board-world-goti w)
                                                                        (board-world-dice w)
                                                                        (board-world-turnplayer w)
                                                                                        #f)))
        ((and (equal? (board-world-rolldice w) #t) (equal? '(cons 272.5 908.5) (get-field co-ordinates property)))
         (begin (send (player-object-giver-other (board-world-turnplayer w)) round-completion)
                                                                                                                               
                                    (send (player-object-giver (board-world-turnplayer w)) supertax)
                                                                                                                         
                                                                   (make-board-world boardimg
                                                                          (board-world-goti w)
                                                                          (board-world-dice w)
                                                                          (board-world-turnplayer w)
                                                                                    #f)))
       ((and (equal? (board-world-rolldice w) #t) (equal? "" (get-field property-owner property)))
        (begin (set! image (list (change-world-image (merge-photo (get-field image (hash-ref boxes (hash-ref tagged-hash (list-ref (board-world-goti w)
                                                                                                                    (turn-return (board-world-turnplayer w))))))
                                                                                                    buyimg 120 320) boardimg)))
                                                                                       (send (player-object-giver-other (board-world-turnplayer w)) round-completion
                                                                                                                               )
                                                                                        (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f)
                                                  ))
        ((and (equal? (board-world-rolldice w) #t) (not (equal? (player-name-giver (board-world-turnplayer w)) (get-field property-owner property))))
         ;; add conditions for railroads
         (set-field! balance (player-object-giver (board-world-turnplayer w)) (- (get-field balance (player-object-giver (board-world-turnplayer w))) (get-field rent property)))
         (displayln "Rent Deducted : ")
         (display "Player :")
         (displayln (player-name-giver (board-world-turnplayer w)))
         (display "Balance :")
         (displayln (get-field balance (player-object-giver (board-world-turnplayer w))))
         (send (player-object-giver-other (board-world-turnplayer w)) take-rent property)
         (send (player-object-giver-other (board-world-turnplayer w)) round-completion
                                        )
                                                                                            (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f))
        ((and (equal? (board-world-rolldice w) #t) (equal? (player-name-giver (board-world-turnplayer w)) (get-field property-owner property)))
         (begin (set! image (list (change-world-image (merge-photo (get-field image
                                                                                           (hash-ref boxes
                                                                                                 (hash-ref tagged-hash
                                                                                                  (list-ref (board-world-goti w)
                                                                                                   (turn-return (board-world-turnplayer w))))))
                                                                                                     upgradeimg 120 320) boardimg)))
                                                  (send (player-object-giver-other (board-world-turnplayer w)) round-completion
                                                           )
                                                                                       (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f)
                                                  ))
     (else w)))
        (else w)))

(define (player-name-giver n)
  (if (= n 1) "player1"
      "player2"))
(define (player-object-giver n)
  (if (= n 1) player1
      player2))
(define (player-object-giver-other n)
  (if (= n 1) player2
      player1))

(define (turn-return n)
  (cond ((= n 1) 0)
        (else 1)))

(define startstate #t)


(define r-begin-x 375)
(define r-end-x 631)
(define r-begin-y 700)
(define r-end-y 900)
(define start-begin-x 375)
(define start-end-x 631)
(define start-begin-y 393)
(define start-end-y 496)
(define buy-begin-x 618)
(define buy-end-x 730)
(define buy-begin-y 521)
(define buy-end-y 555)
(define db-begin-x 542)
(define db-end-x 730)
(define db-begin-y 589)
(define db-end-y 627)
(define pass-begin-x 628)
(define pass-end-x 730)
(define pass-begin-y 657)
(define pass-end-y 693)
(define u-begin-x 618)
(define u-end-x 730)
(define u-begin-y 415)
(define u-end-y 480)
(define m-begin-x 618)
(define m-end-x 730)
(define m-begin-y 510)
(define m-end-y 550)


(define (buy-action player property) ; player property)
  (begin (send player purchase property)
         (displayln "Buy :")
         (display "Player :")
         (displayln (get-field playername player))
         (display "Balance :")
         (displayln (get-field balance player))
         (display "List of properties owned : ")
         (displayln (send player property-name-list))
         (newline)
                       (set! image (list boardimg))))
(define (m-action player property) ; player property)
  (begin (send player mortgage property)
         (displayln "Mortgage :")
         (display "Player :")
         (displayln (get-field playername player))
         (display "Balance :")
         (displayln (get-field balance player))
         (display "List of properties owned : ")
         (displayln (send player property-name-list))
         (newline)
                       (set! image (list boardimg))))
(define (u-action player property) ; player property)
  (begin (send player upgrade-property property)
         (displayln "Upgrade :")
         (display "Player :")
         (displayln (get-field playername player))
         (display "Balance :")
         (displayln (get-field balance player))
         (display "Number of houses built : ")
         (displayln (- (get-field upgrade-count property) 1))
         (newline)
                       (set! image (list boardimg))))
(define (pass-action) (begin (displayln "Pass")
                             (newline)
                           (set! image (list boardimg))))
(define (db-action) (begin board
                           (displayln "Game Ends Please close this window")
                           (newline)
                           (set! image (list gameoverimg))
                           ))

(define (mouse w x y input)
  (define player 0)
  (define property (hash-ref boxes (hash-ref tagged-hash (list-ref (board-world-goti w) (turn-return (board-world-turnplayer w))))))
  (if (= (board-world-turnplayer w) 1) (set! player player1) (set! player player2))
  (cond ((and (mouse=? input "button-down") startstate (<= x start-end-x) (>= x start-begin-x) (<= y start-end-y) (>= y start-begin-y))
         (begin  (set! startstate #f) board))
        ((and (mouse=? input "button-down") startstate (<= x r-end-x) (>= x r-begin-x) (<= y r-end-y) (>= y r-begin-y))
         (begin  (set! rulesstate #t) (set! startstate #f) board))
        ((and (mouse=? input "button-down") (not startstate) (not rulesstate) (equal? "" (get-field property-owner property)) (<= x buy-end-x) (>= x buy-begin-x) (<= y buy-end-y) (>= y buy-begin-y))
         (buy-action player property)
                                                                                    (make-board-world image
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f))
        ((and (mouse=? input "button-down") (not startstate) (not rulesstate) (<= x pass-end-x) (>= x pass-begin-x) (<= y pass-end-y) (>= y pass-begin-y))
         (pass-action)                                                             (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f))
        ((and (mouse=? input "button-down") (not startstate) (not rulesstate)
              (<= x db-end-x) (>= x db-begin-x) (<= y db-end-y) (>= y db-begin-y)) (db-action)   (set-field! bankruptcy player #t)

                                                                                   (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f))
        ((and (mouse=? input "button-down") (not startstate) (not rulesstate) (equal? (player-name-giver (board-world-turnplayer w)) (get-field property-owner property))
              (<= x u-end-x) (>= x u-begin-x) (<= y u-end-y) (>= y u-begin-y)) (u-action player property)
                                                                               (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f))
        ((and (mouse=? input "button-down") (not startstate) (not rulesstate) (equal? (player-name-giver (board-world-turnplayer w)) (get-field property-owner property))
              (<= x m-end-x) (>= x m-begin-x) (<= y m-end-y) (>= y m-begin-y)) (m-action player property)
                                                                               (make-board-world boardimg
                                                                                                       (board-world-goti w)
                                                                                                       (board-world-dice w)
                                                                                                       (board-world-turnplayer w)
                                                                                                       #f))
        (else w)))

 (define (end-game w)
   (begin (send player1 not-bankrupt)
          (send player2 not-bankrupt)
          (let [(status1 (get-field bankruptcy player1))
                (status2 (get-field bankruptcy player2))]
            (cond [(and (equal? #f status1) (equal? #t status2)) (begin
                                                                 (displayln "Game is Over")
                                                                 (display "CONGRATULATIONS : ")
                                                                 (display (get-field playername player1))
                                                                 (displayln " Wins!!")
                                                                 #t)]
                  [(and (equal? #t status1) (equal? #f status2)) (begin
                                                                 (displayln "Game is Over")
                                                                 (display "CONGRATULATIONS : ")
                                                                 (display (get-field playername player2))
                                                                 (displayln " Wins!!")
                                                                 #t)]
                  [(and (equal? #f status1) (equal? #f status2)) (begin
;;                                                                 (displayln "Game is Over")
;;                                                                 (display "CONGRATULATIONS : ")
;;                                                                 (display (get-field playername player2))
;;                                                                 (displayln " Wins!!")
                                                                  #f)]
                  [(and (equal? #t status1) (equal? #t status2)) (begin
                                                                 (displayln "Game is Over")
                                                                 (display "CONGRATULATIONS : ")
                                                                 (display (get-field playername player2))
                                                                 (display "and ")
                                                                 (display (get-field playername player1))
                                                                 (displayln " It's a tie!!")
                                                                 #t)]))))
                  
                  
                                                                 
                                                                 
                                                                 

(big-bang board
  [to-draw monopoly]
  [on-key roll-dice]
  [on-mouse mouse]
  [on-tick do-stuff 1]
  [stop-when end-game monopoly]
  )
