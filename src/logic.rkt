#lang racket



(define (init_crupier) (list "crupier" (list ) 0 "playing")) ; nombre : mazo : id : estado : puntaje


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

    [else (cons (list (car players_names) '() num_player "playing") (list_players_aux (cdr players_names) (+ num_player 1)))]))



(define (shuffle_deck game_info) (cons (shuffle (car game_info)) (cdr game_info)))

(define (deck game_info) (car game_info))

(define (players game_info) (cadr game_info))

(define (crupier game_info)(caddr game_info))

(define (current_player_id game_info) (cadddr game_info))

(define (player_id player) (caddr player))

(define (player_deck player) (cadr player))

(define (player_name player) (car player))

(define (player_state player) (cadddr player))

(define (take_card deck) (car deck))

(define (current_player game_info) (player_by_id game_info (current_player_id game_info)))

(define (update_game deck players crupier current_player) (list deck players crupier current_player))

(define (player_by_id game_info id) (player_by_id_aux (players game_info) id))


(define (player_by_id_aux players player)
    (cond [(null? players) (raise "Player not found")]
    
    [(= (player_id (car players)) player) (car players)]

    [else (player_by_id_aux (cdr players player))]))




; ---------------------------- player ---------------------



(define (change_state players id new_state)
    (cond [(null? players) '()]

    [(= (player_id (car players)) id)
        (cons (list (player_name (car players)) (player_deck (car players)) (player_id (car players)) new_state) 
        (change_state (cdr players) id new_state)) ]
        
    [else (cons (car players) (change_state (cdr players) id new_state))]))


;---------------------------- evaluate deck ----------------------------------------------------------------


(define (evaluate_deck player As_value) 
    (+ (evaluate_deck_aux (player_deck player)) (* As_value (aces (player_deck player)))))

(define (evaluate_deck_aux deck)
    (cond [(null? deck) 0]
    [(number? (caar deck))  (+ (caar deck) (evaluate_deck_aux (cdr deck)))]    
    [(equal? (caar deck) "J") (+ 10 (evaluate_deck_aux (cdr deck)))]
    [(equal? (caar deck) "Q") (+ 10 (evaluate_deck_aux (cdr deck)))]
    [(equal? (caar deck) "K") (+ 10 (evaluate_deck_aux (cdr deck)))]
    [else (evaluate_deck_aux (cdr deck))]))

 
(define (aces deck)
    (cond [(null? deck) 0]
    [(equal? (caar deck) "A") (+ 1 (aces(cdr deck)))]
    [else (aces (cdr deck))]))





; ------------------------ Hit --------------------------------------

(define (hit game_info id) 
    (cond [(= id 0) 
        (update_game (cdr (deck game_info)) 
                     (players game_info) 
                     (add_card_to_crupier (deck game_info) (crupier game_info)) 
                     (current_player_id game_info))] 
    [else 
        (update_game (cdr (deck game_info)) 
                     (add_card_to_player (deck game_info) (players game_info) id) 
                     (crupier game_info) (current_player_id game_info))]))
     


(define (add_card_to_crupier deck crupier)
    (list (player_name crupier) 
          (add (player_deck crupier)   
          (take_card deck)) 0 (player_state crupier)))


(define (add_card_to_player deck players player)

    (cond [(null? players) 
        '()]

    [(= (player_id (car players)) player) 
        (cons (list (player_name(car players)) 
              (add (cadar players) (take_card deck)) 
              (player_id (car players)) 
              (player_state (car players))) (add_card_to_player deck (cdr players) player))]
    [else 
        (cons (car players) (add_card_to_player deck (cdr players) player))]))



(define (add deck card)
    (cond [(null? deck) (list card)]
    [else (cons (car deck) (add (cdr deck) card))]))


;------------------------ stand -----------------------------------------


(define (stand game_info id )
    (cond [(= (current_player_id game_info) (length (players game_info)))
        (update_game (deck game_info) 
                     (change_state (players game_info) id "stand") 
                     (crupier game_info) 0)] ;Comienza a jugar el crupier
    
    [else 
        (update_game (deck game_info) 
                     (change_state (players game_info) id "stand") 
                     (crupier game_info) (+ (current_player_id game_info) 1))]))




;------------------ crupier functionality -------------------------------------


(define (start_crupier game_info)
    (cond [(< (evaluate_deck (crupier game_info) 11) 21)
        (cond [(< (evaluate_deck (crupier game_info) 1) 16)
            (start_crupier (hit game_info 0))]
        [else   
            (stand game_info 0)])]
    [else
        (cond [(< (evaluate_deck (crupier game_info) 1) 16) 
            (start_crupier (hit game_info 0))]
        [else
            (stand game_info 0)])]))


;----------------------- general functionality -----------------------------


(define (bCEj players)
    (set_initial_cards (shuffle_deck (list (create_deck) (list_players  players) (init_crupier) 1))))


(define (set_initial_cards game_info)
    (set_initial_cards_aux (set_initial_cards_aux game_info (length (players game_info))) (length (players game_info))))


(define (set_initial_cards_aux game_info num_players)
    (cond [(= num_players 0)
        (hit game_info 0)]
    [else
        (set_initial_cards_aux (hit game_info num_players) (- num_players 1))]))



(define (game_over? game_info)
    (= (current_player_id game_info) 0))



(define (next_player game_info new_state score) 

    (cond [(= (current_player_id game_info) (length (players game_info)))
        (update_game (deck game_info) 
                 (change_state (players game_info) (current_player_id game_info) "lost") 
                 (crupier game_info) 
                 0)]
    [else
        (update_game (deck game_info) 
                 (change_state (players game_info) (current_player_id game_info) "lost") 
                 (crupier game_info) 
                 (+ (current_player_id game_info) 1))]))







(define current_game (bCEj '("Maria" "Juan")))
(start_crupier current_game)