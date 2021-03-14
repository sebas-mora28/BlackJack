#lang racket



#|
Nombre: init_crupier
Descripción: inicializa el crupier.  
Entradas:
Salidas:
|#
(define (init_crupier) (list "crupier" (list ) 0 "playing" 0)) ; nombre : mazo : id : estado : puntaje


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

    [else (cons (list (car players_names) '() num_player "playing" 0) (list_players_aux (cdr players_names) (+ num_player 1)))]))



(define (shuffle_deck game_info) (cons (shuffle (car game_info)) (cdr game_info)))

(define (deck game_info) (car game_info))

(define (players game_info) (cadr game_info))

(define (crupier game_info)(caddr game_info))

(define (current_player_id game_info) (cadddr game_info))

(define (player_id player) (caddr player))

(define (player_deck player) (cadr player))

(define (player_name player) (car player))

(define (player_state player) (cadddr player))

(define (player_score player) (car (cddddr player)))

(define (take_card deck) (car deck))

(define (current_player game_info) (player_by_id game_info (current_player_id game_info)))

(define (update_game deck players crupier current_player) (list deck players crupier current_player))

(define (player_by_id game_info id) (player_by_id_aux (players game_info) id))


(define (player_by_id_aux players id)
    (cond [(null? players) (raise "Player not found")]
    
    [(= (player_id (car players)) id) (car players)]

    [else (player_by_id_aux (cdr players) id)]))


;-------------------------------------------------------------


(define (winners game_info)

    (cond [(and (null? players_in_game (players game_info)) (not (equal? (player_state (crupier game_info)) "stand")))
        '()]
    
    [(equal? (player_state (crupier game_info)) "stand")
        (winners_aux (add (players game_info) (crupier game_info)))]
        
    [else
        (winners_aux (cdr (players game_info)) (list (car (players game_info))))]))


(define (winners_aux possible_winners winners)
    (cond [(null? possible_winners) winners]

    [(> (player_score (car possible_winners)) (player_score (car winners)))
            (winners_aux (cdr possible_winners) (list (car possible_winners)))]

    [(= (player_score (car possible_winners)) (player_score (car winners)))
        (cond [(= (length (player_deck (car possible_winners))) (length (player_deck (car winners))))
            (winners_aux (cdr possible_winners) (add winners (car possible_winners)))]
            
        [(< (length (player_deck (car possible_winners))) (length (player_deck (car winners))))
            (winners_aux (cdr possible_winners) (add winners (car possible_winners)))]
        
        [else
            (winners_aux (cdr possible_winners) winners)])]
    [else
        (winners_aux (cdr possible_winners) winners)]))
    


(define (players_in_game players)
    (cond [(null? players) '()]
    
    [(equal? (player_state (car players) "stand")) 
        (cons (car players) (players_in_game (cdr players)))]
    
    [else (players_in_game (cdr players))]))








; ---------------------------- player ---------------------



(define (update_player_score_and_state players id new_state score)
    (cond [(null? players) '()]

    [(= (player_id (car players)) id)
        (cons (list (player_name (car players)) (player_deck (car players)) (player_id (car players)) new_state score) 
        (update_player_score_and_state (cdr players) id new_state score)) ]
        
    [else (cons (car players) (update_player_score_and_state (cdr players) id new_state score))]))


(define (update_crupier_score_and_state crupier new_state score) 
        (list (player_name crupier) (player_deck crupier) (player_id crupier) new_state score))


;---------------------------- evaluate deck ----------------------------------------------------------------



(define (evaluate_deck deck As_value)
    (cond [(null? deck) 0]
    [(number? (caar deck))  (+ (caar deck) (evaluate_deck (cdr deck) As_value))]    
    [(equal? (caar deck) "J") (+ 10 (evaluate_deck (cdr deck) As_value))]
    [(equal? (caar deck) "Q") (+ 10 (evaluate_deck (cdr deck) As_value))]
    [(equal? (caar deck) "K") (+ 10 (evaluate_deck (cdr deck) As_value))]
    [(equal? (caar deck) "A") (+ As_value (evaluate_deck (cdr deck) As_value))]
    [else (evaluate_deck (cdr deck) As_value)]))

 
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
          (take_card deck)) 0 (player_state crupier)
          (player_score crupier)))


(define (add_card_to_player deck players player)

    (cond [(null? players) 
        '()]

    [(= (player_id (car players)) player) 
        (cons (list (player_name(car players)) 
              (add (cadar players) (take_card deck)) 
              (player_id (car players)) 
              (player_state (car players))
              (player_score (car players))) (add_card_to_player deck (cdr players) player))]
    [else 
        (cons (car players) (add_card_to_player deck (cdr players) player))]))



(define (add lista elem)
    (cond [(null? lista) (list elem)]
    [else (cons (car lista) (add (cdr lista) elem))]))


;------------------------ stand -----------------------------------------


(define (stand game_info id score)
    (cond [(= (current_player_id game_info)  0) 
        (list   (player_name (crupier game_info)) 
                (player_deck (crupier game_info))
                id
                "stand"
                score)]  
    
    [(=  (current_player_id game_info) (length (players game_info)))
        (update_game (deck game_info) 
                (update_player_score_and_state (players game_info) id "stand" score) 
                (start_crupier game_info) 
                0)] ;Comienza a jugar el crupier
    
    [else 
        (update_game (deck game_info) 
                (update_player_score_and_state (players game_info) id "stand" score) 
                (crupier game_info) (+ (current_player_id game_info) 
                1))]))




;------------------ crupier functionality -------------------------------------


(define (start_crupier game_info)
    (cond   [(> (evaluate_deck (player_deck (crupier  game_info)) 11) 21)
                (cond   [(> (evaluate_deck (player_deck (crupier  game_info)) 1) 21) 
                                (update_crupier_score_and_state (crupier game_info) "lost" (evaluate_deck (player_deck (crupier  game_info)) 1))]

                        [(= (evaluate_deck (player_deck (current_player game_info)) 1) 21) 
                                (update_crupier_score_and_state (crupier game_info) "stand" 21)]

                        [else (crupier_next_move game_info (evaluate_deck (player_deck (crupier game_info)) 1))])]

            [(= (evaluate_deck (player_deck (current_player game_info)) 11) 21)
               (update_crupier_score_and_state (crupier game_info) "stand" 21)]
               
            [else (crupier_next_move game_info (evaluate_deck (player_deck (crupier game_info)) 11))]))



(define (crupier_next_move game_info score)
    (cond [(> score 17) 
        (update_crupier_score_and_state (crupier game_info) "stand" score)]
    
    [else
        (start_crupier (hit game_info 0))]))

;----------------------- general functionality -----------------------------


(define (bCEj players)
    (set_initial_cards (shuffle_deck (list (create_deck) (list_players  players) (init_crupier) 1))))
;Agrega el crupier a la lista de posibles ganadores

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

    (cond [(= (current_player_id game_info) 0) 
        (list   (player_name (crupier game_info)) 
                (player_deck (crupier game_info))
                0
                new_state
                score)]    

    [(= (current_player_id game_info) (length (players game_info)))
        (update_game (deck game_info) 
                (update_player_score_and_state(players game_info) (current_player_id game_info) new_state score) 
                (start_crupier game_info)
                0)]
    [else
        (update_game (deck game_info) 
                (update_player_score_and_state(players game_info) (current_player_id game_info) new_state score) 
                (crupier game_info) 
                (+ (current_player_id game_info) 1))]))




(define current_game (bCEj '("Maria" "Juan")))