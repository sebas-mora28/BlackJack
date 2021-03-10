#lang racket



(define (init_crupier) (list "crupier" (list ) 0 #t)) ; id : decks cards 




(define (create_deck)
    (list '(2 "Clubs") '(2 "Diamonds") '(2 "Hearts") '(2 "Spades")
          '(3 "Clubs") '(3 "Diamonds") '(3 "Hearts") '(3 "Spades")
          '(4 "Clubs") '(4 "Diamonds") '(4 "Hearts") '(4 "Spades")
          '(5 "Clubs") '(5 "Diamonds") '(5 "Hearts") '(5 "Spades")
          '(6 "Clubs") '(6 "Diamonds") '(6 "Hearts") '(6 "Spades")
          '(7 "Clubs") '(7 "Diamonds") '(7 "Hearts") '(7 "Spades")
          '(8 "Clubs") '(8 "Diamonds") '(8 "Hearts") '(8 "Spades")
          '(9 "Clubs") '(9 "Diamonds") '(9 "Hearts") '(9 "Spades")
          '(10 "Clubs") '(10 "Diamonds") '(10 "Hearts") '(10 "Spades")
          '("J" "Clubs") '("J" "Diamonds") '("J" "Hearts") '("J" "Spades")
          '("Q" "Clubs") '("Q" "Diamonds") '("Q" "Hearts") '("Q" "Spades")
          '("K" "Clubs") '("K" "Diamonds") '("K" "Hearts") '("K" "Spades")
          '("A" "Clubs") '("A" "Diamonds") '("A" "Hearts") '("A" "Spades")))





(define (list_players players_names) (list_players_aux players_names 1))


(define (list_players_aux players_names num_player)
    (cond [(null? players_names) '()]

    [else (cons (list (car players_names) '() num_player #t) (list_players_aux (cdr players_names) (+ num_player 1)))]))





(define (shuffle_deck game) (cons (shuffle (car game)) (cdr game)))



 
(define (aces hand)
    (cond [(null? hand) 0]
    [(equal? (caar hand) "A") (+ 1 (aces(cdr hand)))]
    [else (aces (cdr hand))]))




(define (get_deck game) (car game))

(define (get_players game) (cadr game))

(define (get_crupier game)(caddr game))

(define (get_current_player_id game) (cadddr game))

(define (get_player_id player) (caddr player))

(define (get_player_hand player) (cadr player))

(define (get_player_name player) (cdr player))

(define (get_player_state player) (cadddr player))

(define (take_card game) (caar game))

(define (evaluate_hand hand as_value) (+ (evaluate_hand_aux hand) (* as_value (aces hand))))

(define (update_game deck players crupier current_player) (list deck players crupier current_player))

(define (get_player_by_id game player_id) (get_player_by_id_aux (get_players game) player_id))




(define (get_player_by_id_aux players player)
    (cond [(null? players) (raise "Player not found")]
    
    [(= (get_player_id (car players)) player) (car players)]

    [else (get_player_by_id_aux (cdr players player))]))



(define (evaluate_hand_aux hand)
    (cond [(null? hand) 0]
    
    [(number? (caar hand))  (+ (caar hand) (evaluate_hand_aux (cdr hand)))]    
    
    [(equal? (caar hand) "J") (+ 12 (evaluate_hand_aux (cdr hand)))]
   
    [(equal? (caar hand) "Q") (+ 13 (evaluate_hand_aux (cdr hand)))]
   
   [(equal? (caar hand) "K") (+ 14 (evaluate_hand_aux (cdr hand)))]
   
    [else (evaluate_hand_aux (cdr hand))]))



#|

 Hit 

|#

(define (hit game) 
    (cond [(= (get_current_player_id game) 0) (update_game (cdr (get_deck game)) (get_players game) (add_card game 0 (take_card)) (get_current_player_id game))]
    
    [else (update_game (cdr (get_deck game)) (add_card game (get_current_player_id game) (take_card)) (get_crupier game) (get_current_player_id game))]))
     


(define (add hand card)
    (cond [(null? hand) (list card)]
    
    [else (cons (car hand) (add (cdr hand) card))]))



(define (add_card_aux players player card)
    (cond [(null? players) '()]
    
    [(= (get_player_id (car players)) player) (cons (list (caar players) (add (cadar players) card) (caddar players) (car (cdddar players))) (add_card_aux (cdr players) player card))]
    
    [else (cons (car players) (add_card_aux (cdr players) player card))]))


(define (add_card game player card)
    (cond [(= player 0) (add_card_aux (list (get_crupier game)) player card)]

    [else (add_card_aux (get_players game) player card)]))




(define (stand game)
    (cond [(= (get_current_player_id game) (length (get_players game)))] ;Comienza a jugar el crupier
    
    [else (update_game (get_deck game) (get_players game) (get_crupier game) (+ (get_current_player_id game) 1))]))






(define (bCEj players)
    (shuffle_deck (list (create_deck) (list_players  players) (init_crupier) (car players))))


(define current_game (bCEj '("Maria" "Juan" "Pedro")))




