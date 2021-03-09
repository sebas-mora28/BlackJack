#lang racket


; Name : 
; Description: 
; Inputs:  
; Outputs:
(define (init_crupier)
    (list 'crupier '())) ; id : decks cards 


; Name : 
; Description: 
; Inputs:  
; Outputs:
(define (create_deck)
    (list '(2 Clubs) '(2 Diamonds) '(2 Hearts) '(2 Spades)
          '(3 Clubs) '(3 Diamonds) '(3 Hearts) '(3 Spades)
          '(4 Clubs) '(4 Diamonds) '(4 Hearts) '(4 Spades)
          '(5 Clubs) '(5 Diamonds) '(5 Hearts) '(5 Spades)
          '(6 Clubs) '(6 Diamonds) '(6 Hearts) '(6 Spades)
          '(7 Clubs) '(7 Diamonds) '(7 Hearts) '(7 Spades)
          '(8 Clubs) '(8 Diamonds) '(8 Hearts) '(8 Spades)
          '(9 Clubs) '(9 Diamonds) '(9 Hearts) '(9 Spades)
          '(10 Clubs) '(10 Diamonds) '(10 Hearts) '(10 Spades)
          '(J Clubs) '(J Diamonds) '(J Hearts) '(J Spades)
          '(Q Clubs) '(Q Diamonds) '(Q Hearts) '(Q Spades)
          '(K Clubs) '(K Diamonds) '(K Hearts) '(K Spades)
          '(A Clubs) '(A Diamonds) '(A Hearts) '(A Spades)))



; Name : 
; Description: 
; Inputs:  
; Outputs:
(define (list_players players_names)
    (list_players_aux players_names))


(define (list_players_aux players_names)

    (cond [(null? players_names) '()]
    
    [else 
        (cons (list (car players_names) '()) (list_players_aux (cdr players_names)))]))




; Name : 
; Description: 
; Inputs:  
; Outputs
(define (mix_deck current_game)
    (cons (shuffle (car current_game)) (cdr current_game)))








; Name: 
; Description: 
; Inputs:  
; Outputs:
(define (aces hand)
    (cond [(null? hand)
        0]
    [(equal? (caar hand) 'A)
        (+ 1 (aces(cdr hand)))]
    [else
        (aces (cdr hand))]))



; Name: 
; Description: 
; Inputs:  
; Outputs:
(define (evaluate_hand hand as_value)
    (+ (evaluate_hand_aux hand) (* as_value (aces hand))))




; Name: 
; Description: 
; Inputs:  
; Outputs:
(define (evaluate_hand_aux hand)
    (cond [(null? hand) 
        0]
    [(number? (caar hand)) 
        (+ (caar hand) (evaluate_hand_aux (cdr hand)))]    
    [(equal? (caar hand) 'J)
        (+ 12 (evaluate_hand_aux (cdr hand)))]
    [(equal? (caar hand) 'Q)
        (+ 13 (evaluate_hand_aux (cdr hand)))]
    [(equal? (caar hand) 'K)
        (+ 14 (evaluate_hand_aux (cdr hand)))]
    [else 
        (evaluate_hand_aux (cdr hand))]))

(evaluate_hand '((2 Clubs) (A Diamonds) (Q Hearts) (A Spades)) 1)






; Name : bCEj
; Description: Punto de entrada del juego, inicializa el mazo de cartas, la lista de jugadores, crupier y el jugador actual 
; Inputs: player -> lista con el nombre de jugadores
; Outputs: Lista que contiene el mazo de cartas, lista de jugadores, crupier 
(define (bCEj players)
    (mix_deck (list (create_deck) (list_players  players) (init_crupier) (car players))))




