#lang racket
(define-syntax while
    (syntax-rules ()
      [(while bool operation1 more ... iterator1) (begin (define (iterate)
                                               (cond [bool (begin operation1
                                                                  more ...
                                                                  iterator1
                                                                  (iterate))]))
                                             (iterate))]))

(define finish_order 6)
(define games_order 3)
;(define (perms l)
;  (define (g x) (map (lambda (y) (cons x y)) (perms (remove x l))))
;  (match l
;    ['() '(())]
;    [(cons x xs) (append* (map g l))])) 
(define rollvalues '(2 3 4 5 6 7 3 4 5 6 7 8 4 5 6 7 8 9 5 6 7 8 9 10 6 7 8 9 10 11 7 8 9 10 11 12))
;(define finish (expt 10 finish_order))
;(define games (expt 10 games_order))
(define finish (expt 10 finish_order))
(define games (expt 10 games_order))
(define games_finished 0)
;(define (factorial n)
;  (define (helper n acc)
;    (if (= n 0) acc (helper (- n 1) (* acc n))))
;  (helper n 1))
;(define (shuffle l)
;  (let [(x (factorial (length l)))]
;    (list-ref (perms l) (random 0 x))))
;
;;; shuffling algo for a list
(define (shuffle l)
  (let [(v (list->vector l))]
  (define temp 0)
  (define random-index 0)
  (define i 0)
  (begin (while (< i (length l))
         (set! random-index (random 0 (length l)))
         ;(displayln i)
         (set! temp (vector-ref v i))
         (vector-set! v i (vector-ref v random-index))
         (vector-set! v random-index temp)
         (set! i (+ i 1)))
         (vector->list v))))
         
(define (member? u l)
  (not (equal? #f (member u l))))

;(define position 9)

(define (monoply)
  (begin (define squares (make-vector 40 0))
    (while (< games_finished games)
         (define master_chest '(0 40 40 40 40 10 40 40 40 40 40 40 40 40 40 40))
         (define chest (shuffle master_chest))
         (define master_chance '(0 24 11 #\U #\R 40 40 #\B 10 40 40 5 39 40 40 40))
         (define chance (shuffle master_chance))
        ; (define position 0)
         (define doubles 0)
         (define position 0)
         (define gos 0)
         (while (< gos finish)
                (define diceroll (random 0 36))
               ; (displayln diceroll)
                (if (member? diceroll '(0 7 14 21 28 35)) (set! doubles (+ doubles 1))
                    (set! doubles 0))
                (if (>= doubles 3) (set! position 10)
                   (begin
                    (set! position (modulo (+ position (list-ref rollvalues diceroll)) 40))
                (cond [(member? position '(7 22 33)) ;;chance box
                    (begin (define chance_card (list-ref chance 0))
                           (set! chance (cdr chance))
                    
                           (cond [(= (length chance) 0)
                               (begin (set! chance (shuffle master_chance)))]
                                 [(not (equal? chance_card 40))
                                  (if (number? chance_card) (set! position chance_card)
                                      (cond [(equal? chance_card #\U)
                                             (begin (define test 1)
                                             (while (not (member? position '(12 28)))
                                                    (set! position (modulo (+ position 1) 40))
                                                    (set! test 0)))]
                                            [(equal? chance_card #\R)
                                             (begin (define test2 1)
                                             (while (not (member? position '(5 15 25 35)))
                                                    (set! position (modulo (+ position 1) 40))
                                                    (set! test2 0)))]
                                            [(equal? chance_card #\B)
                                             (set! position (- position 3))]))]))]
                      [(member? position '(2 17)) ;;community chest
                       (begin (define chest_card (list-ref chest 0))
                              (set! chest (cdr chest))
                              (cond [(= (length chest) 0)
                                     (begin (set! chest (shuffle master_chest)))]
                                    [(not (= chest_card 40)) (set! position chest_card)]))]
                      [(= position 30) ;;go to jail
                            (set! position 10)])))
;                (define (sexy x) (begin (define y (vector-ref squares x))
;                                        (vector-set! squares x 0)
;                                        y))
                (vector-set! squares position (+ (vector-ref squares position) 1))
                
                (set! gos (+ gos 1)))
         (set! games_finished (+ games_finished 1)))
         (set! games_finished 0)
         
         squares))
;;result.
;;;;;;;;;;;;since running this simulation a billion times takes time we have stored the result to save time;;;;;;;;;;;;;;;;;;;
;(time (monoply))
;cpu time: 887891 real time: 896425 gc time: 6968
(define our-result-vector '#(28307028
   21336804
   19499527
   22717604
   24081304
   27354074
   23285497
   11035813
   23204177
   22858492
   61176576
   27004863
   26005271
   23991669
   24766024
   27856520
   27828766
   26161160
   29137658
   30599741
   28476660
   28157632
   13023867
   27150476
   31548027
   28703222
   27056932
   26806436
   28003084
   25957993
   1492920
   26660084
   26092925
   12039292
   24931758
   25470837
   22540433
   21102010
   21116098
   25460746))
;;;;;;;;;;;;;;;;;;;;;;;;conclusion of this simulation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define probability-distribution
  (map (lambda (x) (/ x 10000000.00)) (vector->list our-result-vector)))
         
                
                
                      
                    
                                            
                                      
                    
                    

         

