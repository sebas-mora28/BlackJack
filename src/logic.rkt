#lang racket



#|
Nombre: init_crupier

Descripción: Crea una lista que representa el crupier, esta contiene el nombre, mazo, id, estado y puntaje del crupier.
Entradas: No tiene entradas.
Salidas: lista con la información del crupier.
|#
(define (init_crupier) (list "crupier" (list ) 0 "playing" 0)) ; nombre : mazo : id : estado : puntaje


#|
Nombre: create_deck

Descripción: Devuelve el mazo conformado por 52 cartas.
Entradas: No tiene entradas.
Salidas: Una lista que contiene sublistas que representan cada una de las 52 del mazo.

|#

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



#|
Nombre: lista_players
Descripción: Forma una lista con sublistas, cada sublista representa a cada uno de los jugadores de la partida conteniendo 
             su nombre, baraja de cartas representado por una lista, id, estado actual y puntaje. 
Entradas:
            * player_names -> lista con los nombres de los jugadores de la partida dados por los usuarios 
Salidas: lista con sublistas que representan la informacion de los jugadores
|#
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


#|
Nombre: add
Descripción:  Agrega un elemento al final de una lista
Entradas:
              * lista -> lista a la cual se desea agrega el elemento 
              * elem -> elemento que se desea agregar
Salidas: lista actualizada con el nuevo elemento
|#
(define (add lista elem)
    (cond [(null? lista) (list elem)]
    [else (cons (car lista) (add (cdr lista) elem))]))



;-------------------------------------------------------------


#|
Nombre: winners
Descripción: Esta función se encarga de evaluar la situación final del juego y devolver la lista con el ganador, 
             en caso de que no existan jugadores en juego y el crupier haya perdido, entoces no existe un ganador 
             y devuelve una lista vacía, si el crupier todavía está en juego entonces agrega al crupier posibles 
             ganadores junto a los jugadores que todavía están en juego y llama la función auxiliar de winners. 
             O bien, si el crupier perdió, solamente se llama la función auxiliar del winner pasandole la lista de 
             posibles ganadores conformada por los jugadores que todavía están en juego.
Entradas: 
             * game_info -> lista con la informacion actual de la partida
Salidas: lista con los ganadores de la partida, en caso de que no existan, retorna una lista vacía
|#
(define (winners game_info)

    (cond [(and (null? (players_in_game (players game_info))) (not (equal? (player_state (crupier game_info)) "stand")))
        '()]
    
    [(equal? (player_state (crupier game_info)) "stand")
        (winners_aux (cdr (add (players_in_game game_info) (crupier game_info))) (list (car (players_in_game game_info)))]
        
    [else
        (winners_aux (cdr (players_in_game game_info)) (list (car (players_in_game game_info))))]))


#|
Nombre: winner_aux
Descripción: Esta función se encarga de conformar la lista de ganadores. La lista de ganadores viene por 
             default con el crupier o el primer elemento de la lista de los jugadores en juego. En caso 
             de que el puntaje del primer jugador de la lista de posibles ganadores sea mayor que el primer 
             jugador de la lista de ganadores, entonces se cambia la lista de ganadores. En caso de que tenga 
             el mismo puntaje, se verifica cual de los tiene menos cartas, el que tenga menos será el nuevo 
             ganador y si tiene la misma cantidad habrá un empate. 
Entradas:
            * possible_winners -> lista de posibles ganadores de la partida
            * winners -> lista de ganador o ganadores
Salidas: devuelve una lista con el ganador o ganadores.
|#
(define (winners_aux possible_winners winners)
    (cond [(null? possible_winners) winners]

    [(> (player_score (car possible_winners)) (player_score (car winners)))
            (winners_aux (cdr possible_winners) (list (car possible_winners)))]

    [(= (player_score (car possible_winners)) (player_score (car winners)))
        (cond [(= (length (player_deck (car possible_winners))) (length (player_deck (car winners))))
                (winners_aux (cdr possible_winners) (add winners (car possible_winners)))]
            
            [(< (length (player_deck (car possible_winners))) (length (player_deck (car winners))))
                (winners_aux (cdr possible_winners) (list (car possible_winners)))]
        
        [else
            (winners_aux (cdr possible_winners) winners)])]
    [else
        (winners_aux (cdr possible_winners) winners)]))
    

#|
Nombre: players_in_game
Descripción:  Esta funcion recorre la lista de ganadores buscando aquellos que todavía no hayan perdido,
              es decir, aquellos que tienen como estado "stand" y conforma una lista con estos.
Entradas:
              * players -> lista de ganadores
Salidas: lista con los jugadores que todavía no hayan perdido.
|#
(define (players_in_game players)
    (cond [(null? players) '()]
    
    [(equal? (player_state (car players)) "stand") 
        (cons (car players) (players_in_game (cdr players)))]
    
    [else (players_in_game (cdr players))]))








; ---------------------------- player ---------------------

#|
Nombre: update_player_score_and_state 
Descripción:  Esta funcion se encarga de actualizar el estado y el puntaje del jugador. Recorre la lista
              de jugadores, en el momento en que encuentra el jugador indicado, actualiza los valores.
Entradas:
              * players -> lista de jugadores
              * id -> id del jugador al que se desea actualizar los valores
              * new_state -> nuevo estado del jugador
              * score -> puntaje del jugador      
Salidas: lista de jugadores actualizada. 
|#
(define (update_player_score_and_state players id new_state score)
    (cond [(null? players) '()]

    [(= (player_id (car players)) id)
        (cons (list (player_name (car players)) (player_deck (car players)) (player_id (car players)) new_state score) 
        (update_player_score_and_state (cdr players) id new_state score)) ]
        
    [else (cons (car players) (update_player_score_and_state (cdr players) id new_state score))]))


(define (update_crupier_score_and_state crupier new_state score) 
        (list (player_name crupier) (player_deck crupier) (player_id crupier) new_state score))


;---------------------------- evaluate deck ----------------------------------------------------------------


#|
Nombre: evaluate_deck
Descripción:  Esta función se encarga de calcula el valor de las baraja de un jugador, en caso de 
              que se sea un carta con un valor númerico, se suma ese valor. Si es una carta con una
              letra se suma 10 y si es un As se suma el valor que elegió el jugador.
Entradas:
              * deck -> baraja de cartas
              * As_value -> valor del As elegido por el jugador       
Salidas: un entero indicando el valor de la baraja después de evaluar todas las cartas 
|#
(define (evaluate_deck deck As_value)
    (cond [(null? deck) 0]
    [(number? (caar deck))  (+ (caar deck) (evaluate_deck (cdr deck) As_value))]    
    [(equal? (caar deck) "J") (+ 10 (evaluate_deck (cdr deck) As_value))]
    [(equal? (caar deck) "Q") (+ 10 (evaluate_deck (cdr deck) As_value))]
    [(equal? (caar deck) "K") (+ 10 (evaluate_deck (cdr deck) As_value))]
    [(equal? (caar deck) "A") (+ As_value (evaluate_deck (cdr deck) As_value))]
    [else (evaluate_deck (cdr deck) As_value)]))

 
; ------------------------ Hit --------------------------------------


#|
Nombre: hit
Descripción:  Es la función principal de la acción hit. En caso de que el sea crupier el que va 
              a realizar la acción hit se agrega una carta a este mismo y se actuliza la información
              del juego. En caso de que sea un jugador, se agrega la carta al jugador indicado por el id
              y se actualiza la información del juego. 
Entradas:
              * game_info -> lista con la información actual de partida.
              * id -> id del jugaodr que va a realizar la accion hit        
Salidas: lista con la información actual de la partida actualizada.
|#
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
     

#|
Nombre: add_card_to_crupier
Descripción:  Esta función toma una carda del mazo y la agrega a la baraja del crupier  
Entradas:
              * deck -> baraja del crupier
              * crupier -> lista con la informacion del crupier        
Salidas: lista de la información del crupier actualizada con la nueva carta.
|#
(define (add_card_to_crupier deck crupier)
    (list (player_name crupier) 
          (add (player_deck crupier)   
          (take_card deck)) 0 (player_state crupier)
          (player_score crupier)))


#|
Nombre: add_card_to_player
Descripción:  Esta función toma una carta del mazo y la agrega a la baraja del jugador indicado por el id.
              Recorre la lista de jugadores buscando el jugador al que se desea agregar la carta, cuando lo 
              encuentra saca una carta del mazo y actualiza su baraja. Conforme recorre la lista va formando
              una nueva con los mismos elementos. 
Entradas:
              * deck -> baraja del jugador
              * players -> lista de jugadores de la partida
              * id -> id del jugador al que se desea agregar la carta 
Salidas: lista con jugadores de la partida actualizada
|#
(define (add_card_to_player deck players id)

    (cond [(null? players) 
        '()]

    [(= (player_id (car players)) id) 
        (cons (list (player_name(car players)) 
              (add (cadar players) (take_card deck)) 
              (player_id (car players)) 
              (player_state (car players))
              (player_score (car players))) (add_card_to_player deck (cdr players) id))]
    [else 
        (cons (car players) (add_card_to_player deck (cdr players) id))]))



;------------------------ stand -----------------------------------------

#|
Nombre: stand 
Descripción: Esta es la función principal de la funcionalidad stand. Actualiza el puntaje y estado del 
             jugador actual o del crupier a "stand" y pasa al siguiente jugador. En caso de que sea el 
             crupier el que realiza la acción y devuelve la lista de este. En caso de que el id sea del 
             mismo tamaño que la lista de jugadores, quiere decir que ya no existen más jugadores y el
             siguiente en jugar es el crupier. De otra forma, proceso a jugador el jugar el siguiente
             jugador disponible.
Entradas:
             * game_info -> lista con la información actual de partida.
             * id -> id del que realiza la acción stand 
             * score -> puntaje del que realiza la acción stand
Salidas:    La lista del crupier en caso de que sea este el que realiza la acción, de caso contrario devuelve
            la lista con la información actual de la partida actualizada. 
|#
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
                0)]
    
    [else 
        (update_game (deck game_info) 
                (update_player_score_and_state (players game_info) id "stand" score) 
                (crupier game_info) (+ (current_player_id game_info) 
                1))]))




;------------------ crupier functionality -------------------------------------

#|
Nombre: start_crupier
Descripción: Esta es la función principal de la lógica del crupier. Se encarga de evaluar la mejor
             opción para las jugadas del crupier. Primerament evalúa que valor debe tener el As en
             la bajara de cargas. En caso de que el valor de la baraja sea mayor a 21 ya sea con el 
             As valiendo 1 o 11, quiere decir que el crupier perdió. Una vez que se elige cual es el
             mejor valor para el As, se procede a verificar si la baraja suma 21, en este caso se cambia
             el estado del crupier a stand. Si no suma 21, se llama la función crupier_next_move. 
Entradas:
             * game_info -> lista con la información actual de partida.
Salidas: lista del crupier con la baraja, estado y puntaje actualizado.
|#
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


#|
Nombre: crupier_next_move
Descripción:  Esta función complementa la función start_crupier. Se encarga cual debe ser el siguiente 
              moviento del crupier, si la baraja suma más de 17 en ese momento, se decide a realizar 
              la acción "stand". De caso contrario, toma otra carta del mazo realizando la acción hit.
Entradas:
              * game_info -> lista con la información actual de la partida
              * score -> puntaje actual de la baraja del crupier 
Salidas: lista del crupier actulizada.
|#
(define (crupier_next_move game_info score)
    (cond [(> score 17) 
        (update_crupier_score_and_state (crupier game_info) "stand" score)]
    
    [else
        (start_crupier (hit game_info 0))]))

;----------------------- general functionality -----------------------------


#|
Nombre: set_initial_cards
Descripción:  Función principal que se encarga de colocar dos cartas en las barajas del crupier
Entradas:
Salidas: 
|#
(define (set_initial_cards game_info)
        (set_initial_cards_aux (set_initial_cards_aux game_info (length (players game_info))) (length (players game_info))))


#|
Nombre:
Descripción:  
Entradas:
Salidas: 
|#
(define (set_initial_cards_aux game_info num_players)
    (cond [(= num_players 0)
        (hit game_info 0)]
    [else
        (set_initial_cards_aux (hit game_info num_players) (- num_players 1))]))


#|
Nombre:
Descripción:  
Entradas:
Salidas: 
|#
(define (game_over? game_info)
    (= (current_player_id game_info) 0))


#|
Nombre:
Descripción:  
Entradas:
Salidas: 
|#
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



#|
Nombre: bCEj
Descripción:  Función establecida en la especificación del proyecto. Se encarga de llamar todas las funciones
              que inicializa la información de la partida y agrega las primeras dos cartas a los jugadores y 
              al crupier. 
Entradas: players-> lista con los nombres de los jugadores de la partida
Salidas: lista con la información de la partida. 
|#
(define (bCEj players)
    (set_initial_cards (shuffle_deck (list (create_deck) (list_players  players) (init_crupier) 1))))


(bCEj '("Maria" "Juan"))

(define jugadores '(("Pedro" ((6 "Spades") ("K" "Diamonds") ("A" "Hearts")) 1 "lost" 27) (" Carlos" ((2 "Hearts") (10 "Hearts") ("Q" "Clubs")) 2 "lost" 22) (" Juan" (("Q" "Spades") (3 "Diamonds") (4 "Clubs") (10 "Spades")) 3 "lost" 27)))
(println jugadores)