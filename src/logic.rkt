

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

Entradas:    * player_names -> lista con los nombres de los jugadores de la partida dados por los usuarios 

Salidas: lista con sublistas que representan la informacion de los jugadores
|#
(define (list_players players_names) (list_players_aux players_names 1))

(define (list_players_aux players_names num_player)
    (cond [(null? players_names) '()]

    [else (cons (list (car players_names) '() num_player "playing" 0) (list_players_aux (cdr players_names) (+ num_player 1)))]))



#|
Nombre: shuffle_deck

Descripción:  Se encarga de revolver el mazo de cartas mediante la función shuffle propia de Racket. Al momento 
              de recibir el mazo este se encuentra en orden a como se definió.

Entradas:     * game_info -> lista con la información actual de la partida.

Salidas: lista con la partida actual de la partida actualizada, el mazo de cartas ahora se encuentra revuelto. 
|#
(define (shuffle_deck game_info) (cons (shuffle (car game_info)) (cdr game_info)))


#|
Nombre: deck

Descripción:  Devuelve el mazo de cartas de la lista que contiene la información actual de la partida. 

Entradas:     * game_info -> lista con la información actual de la partida.

Salidas: lista que representa el mazo de cartas de la partida.   
|#
(define (deck game_info) (car game_info))


#|
Nombre: players 

Descripción:  Devuelve la lista de jugadores de la lista que contiene la información actual de la partida.  

Entradas:     * game_info -> lista con la información actual de la partida.     

Salidas: lista que contiene a cada uno de los jugadorses de la partida. 
|#
(define (players game_info) (cadr game_info))


#|
Nombre: crupier

Descripción:  Devuelve el crupier de la lista que contiene la información actual de la partida.   

Entradas:     * game_info -> lista con la información actual de la partida.   

Salidas: lista que representa el crupier. 
|#
(define (crupier game_info)(caddr game_info))


#|
Nombre: current_player_id

Descripción: Devuelve el id del jugador que se encuentra jugando en este momento. 

Entradas:    * game_info -> lista con la información actual de la partida.    

Salidas: entero que representa el id del jugador actual.
|#
(define (current_player_id game_info) (cadddr game_info))

#|
Nombre: player_id

Descripción: Devuelve el id de un jugador o crupier. 

Entradas:    * player -> lista que representa a un jugador en específico.    

Salidas: entero que representa el id del jugador. 
|#
(define (player_id player) (caddr player))

#|
Nombre: player_deck

Descripción: Devuelve la lista que representa la baraja de cartas de un jugador.

Entradas:    * player -> lista que representa a un jugador en específico.    

Salidas: lista que representa la barajas de cartas de un jugador. 
|#
(define (player_deck player) (cadr player))

#|
Nombre: player_name

Descripción:  Devuelve el nombre de un jugador 

Entradas:     * player -> lista que representa a un jugador en específico.   

Salidas: string que representa el nombre del jugador. 
|#
(define (player_name player) (car player))

#|
Nombre: player_state

Descripción:  Devuelve el estado de un jugador, si está jugando, se planteó o perdió

Entradas:     * player -> lista que representa a un jugador en específico.  

Salidas: string que representa el estado del jugador.
|#
(define (player_state player) (cadddr player))

#|
Nombre: player_score

Descripción:  Devuelve el puntaje de un jugador. 

Entradas:     * player -> lista que representa a un jugador en específico.  

Salidas: entero que representa el puntaje del jugador.
|#
(define (player_score player) (car (cddddr player)))

#|
Nombre: take_card

Descripción:  Toma una carta del mazo. Como el mazo de encuentra revuelto escoge la primera carta disponible
              en la lista. 

Entradas:     * deck -> lista que representa el mazo de cartas.

Salidas:      lista que representa una carta. 
|#
(define (take_card deck) (car deck))

#|
Nombre: current_player

Descripción:  Devuelve el jugador que se encuentra jugando en ese momento. Utiliza la función player_by_id

Entradas:     * game_info -> lista con la información actual de la partida.  

Salidas: lista que representa el jugador actual de la partida.
|#
(define (current_player game_info) (player_by_id game_info (current_player_id game_info)))

#|
Nombre: update_game

Descripción: Recibe los diferentes datos que conforman la lista con la información actual de la partida
             y crea una nueva lista los nuevos valores. 

Entradas:    * deck -> lista que representa el mazo del juego.
             * players -> lista que representa los jugadores de la partida.
             * crupier -> lista que representa el crupier.
             * current_player -> entero que representa el jugador actual de la partida. 

Salidas: lista con la información de la partida actualizada. 
|#
(define (update_game deck players crupier current_player) (list deck players crupier current_player))

#|
Nombre: player_by_id

Descripción:  Busca un jugador en la lista de jugadores por su id. 

Entradas:     * game_info -> lista con la información actual de la partida. 
              * id -> id del jugador que se desea buscar.

Salidas: lista del jugador que coincide con el id buscado.
|#
(define (player_by_id game_info id) (player_by_id_aux (players game_info) id))

#|
Nombre: player_by_id_aux

Descripción: Función auxiliar de la player_by_id. Se encarga de recorrer la lista de jugadores y 
             comparar su id con el id buscado. En el momento de encontrarlo, devuelve la lista del 
             jugador. 

Entradas:   * players -> lista de los jugadores de la partida.
            * id -> id del jugador que se desea buscar.

Salidas: lista del jugador que coincide con el id buscado. 
|#
(define (player_by_id_aux players id)
    (cond [(null? players) (raise "Player not found")]
    
    [(= (player_id (car players)) id) (car players)]

    [else (player_by_id_aux (cdr players) id)]))


#|
Nombre: add

Descripción:  Agrega un elemento al final de una lista

Entradas:     * lista -> lista a la cual se desea agrega el elemento 
              * elem -> elemento que se desea agregar

Salidas: lista actualizada con el nuevo elemento
|#
(define (add lista elem)
    (cond [(null? lista) (list elem)]
    [else (cons (car lista) (add (cdr lista) elem))]))



;-------------------------------------------------------------


#|
Nombre: game_positions

Descripción:  Esta función se encarga de devolver la tabla de posiciones del juego una vez que este haya termiando. 
              Primeramente verifica si el estado del crupier, para agregarlo a la lista de posibles ganadadores, en caso de 
              que el crupier haya perdido se agrega a la lista de perdedores. 

Entradas:     * game_info -> lista con la información de la partida.

Salidas: lista con la tabla de posiciones. 
|#
(define (game_positions game_info)
    (cond [(equal? (player_state (crupier game_info)) "stand")
            (append (ord_possible_winners (append (players_in_game (players game_info)) (list (crupier game_info)))) (ord_losers (losers (players game_info))))]
            
        [else
            (append (ord_possible_winners (players_in_game (players game_info))) (ord_losers (losers (append (players game_info) (list (crupier game_info))))))]))



#|
Nombre: ord_possible_winners

Descripción:  Esta función ordena la lista de posibles ganadores. Utiliza el método quicksort. 
              Primero se ordenan los jugadores por la cantidad de cartas y se aplica la función reverse, 
              luego, se ordenan los jugadores por su cantidad de puntos para finalmente volver a aplicar
              la función reverse.

Entradas:     * possibles_winners -> lista con los posibles ganadores de la partida

Salidas: lista actualizada con el nuevo elemento
|#
(define (ord_possible_winners possibles_winners)
    (reverse (orden_by_score (reverse (orden_by_amount_cards possibles_winners)))))



#|
Nombre: ord_losers

Descripción:  Esta función ordena. Utiliza el método quicksort. 

Entradas:     * losers -> lista con los jugadores que perdieron durante la partida. 

Salidas: lista actualizada con el nuevo elemento
|#
(define (ord_losers losers)
    (orden_by_score (orden_by_amount_cards losers)))




#|
Nombre: players_in_game

Descripción:  Esta funcion recorre la lista de ganadores buscando aquellos que todavía no hayan perdido,
              es decir, aquellos que tienen como estado "stand" y conforma una lista con estos.

Entradas:     * players -> lista de ganadores.

Salidas: lista con los jugadores que todavía no hayan perdido.
|#
(define (players_in_game players)
    (cond [(null? players) '()]
    
    [(equal? (player_state (car players)) "stand") 
        (cons (car players) (players_in_game (cdr players)))]
    
    [else (players_in_game (cdr players))]))



#|
Nombre: losers

Descripción:  Esta funcion recorre la lista de perdedores buscando aquellos que ya perdieron,
              es decir, aquellos que tienen como estado "lost" y conforma una lista con estos.

Entradas:     * players -> lista de perdedores.

Salidas: lista con los jugadores perdieron.
|#
(define (losers players)
    (cond [(null? players) '()]
    
    [(equal? (player_state (car players)) "lost") 
        (cons (car players) (losers (cdr players)))]
    
    [else (losers (cdr players))]))



#|
Nombre: less_than_score

Descripción:  Esta función busca los jugadores que tienen un puntaje menor que el pivote

Entradas:     * players -> lista jugadores, puede estar incluido el crupier.

Salidas: lista con los jugadores que tienen un puntaje menor que el pivote.
|#
(define (less_than_score pivot players)
  (cond [(null? players) '()]
  
  [(>= (player_score (car players)) (player_score pivot)) (less_than_score pivot (cdr players))]
  
  [else 
    (cons (car players) (less_than_score pivot (cdr players)))]))

#|
Nombre: less_than_cards

Descripción:  Esta función busca los jugadores que tienen una cantidad de cartas menor que el pivote

Entradas:     * players -> lista jugadores, puede estar incluido el crupier.

Salidas: lista con los jugadores que tienen una cantidad de cartas menor que el pivote.
|#
(define (less_than_cards pivot players)
  (cond [(null? players) '()]
  
  [(>= (length (player_deck (car players))) (length (player_deck pivot))) (less_than_cards pivot (cdr players))]
  
  [else 
    (cons (car players) (less_than_cards pivot (cdr players)))]))




#|
Nombre: greater_than_score

Descripción:  Esta función busca los jugadores que tienen un mayor puntaje que el pivote

Entradas:     * players -> lista jugadores, puede estar incluido el crupier.

Salidas: lista con los jugadores que tienen un mayor puntaje que el pivote.
|#
(define (greater_than_score pivot players)
  (cond [(null? players) '()]
  
  [(< (player_score (car players)) (player_score pivot)) (greater_than_score pivot (cdr players))]

  [else 
    (cons (car players) (greater_than_score pivot (cdr players)))]))


#|
Nombre: greater_than_cards

Descripción:  Esta función busca los jugadores que tienen una mayor cantidad de cartas que el pivote

Entradas:     * players -> lista jugadores, puede estar incluido el crupier.

Salidas: lista con los jugadores que tienen una mayor cantidad de cartas que el pivote.
|#
(define (greater_than_cards pivot players)
  (cond [(null? players) '()]
  
  [(< (length (player_deck (car players))) (length (player_deck pivot))) (greater_than_cards pivot (cdr players))]
  [else 
    (cons (car players) (greater_than_cards pivot (cdr players)))]))

    



#|
Nombre: orden_by_score

Descripción:  Ordena una lista con jugadores, puede incluir el crupier, según su puntaje. Utiliza el 
              método de ordenamiento quicksort.

Entradas:     * players -> lista jugadores a ordenar, puede estar incluido el crupier.

Salidas: lista con los jugadores ordenados por su puntaje.
|#
(define (orden_by_score players)
  (cond [ (null? players) '()]
  [else
    (append (orden_by_score (less_than_score (car players) (cdr players))) 
            (list (car players)) 
            (orden_by_score (greater_than_score (car players) (cdr players))))]))



#|
Nombre: orden_by_cards

Descripción:  Ordena una lista con jugadores, puede incluir el crupier, según su cantidad de cartas. Utiliza el 
              método de ordenamiento quicksort.

Entradas:     * players -> lista jugadores a ordenar, puede estar incluido el crupier.

Salidas: lista con los jugadores ordenados por su cartas.
|#
(define (orden_by_amount_cards players)
  (cond [ (null? players) '()]
  [else
    (append (orden_by_amount_cards (less_than_cards (car players) (cdr players))) 
            (list (car players)) 
            (orden_by_amount_cards (greater_than_cards (car players) (cdr players))))]))




; -------------------------------------------------

#|
Nombre: update_player_score_and_state 

Descripción:  Esta funcion se encarga de actualizar el estado y el puntaje del jugador. Recorre la lista
              de jugadores, en el momento en que encuentra el jugador indicado, actualiza los valores.

Entradas:     * players -> lista de jugadores.
              * id -> id del jugador al que se desea actualizar los valores.
              * new_state -> nuevo estado del jugador.
              * score -> puntaje del jugador. 

Salidas: lista de jugadores actualizada. 
|#
(define (update_player_score_and_state players id new_state score)
    (cond [(null? players) '()]

    [(= (player_id (car players)) id)
        (cons (list (player_name (car players)) (player_deck (car players)) (player_id (car players)) new_state score) 
        (update_player_score_and_state (cdr players) id new_state score)) ]
        
    [else 
        (cons (car players) (update_player_score_and_state (cdr players) id new_state score))]))


(define (update_crupier_score_and_state crupier new_state score) 
        (list (player_name crupier) (player_deck crupier) 0 new_state score))


;---------------------------- evaluate deck ----------------------------------------------------------------


#|
Nombre: evaluate_deck

Descripción:  Esta función se encarga de calcula el valor de las baraja de un jugador, en caso de 
              que se sea un carta con un valor númerico, se suma ese valor. Si es una carta con una
              letra se suma 10 y si es un As se suma el valor que elegió el jugador.

Entradas:     * deck -> baraja de cartas.
              * As_value -> valor del As elegido por el jugador.  

Salidas: un entero indicando el valor de la baraja después de evaluar todas las cartas.
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

Entradas:     * game_info -> lista con la información actual de partida.
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
                     (crupier game_info) 
                     (current_player_id game_info))]))
     

#|
Nombre: add_card_to_crupier

Descripción:  Esta función toma una carda del mazo y la agrega a la baraja del crupier  

Entradas:     * deck -> baraja del crupier.
              * crupier -> lista con la informacion del crupier.   

Salidas: lista de la información del crupier actualizada con la nueva carta.
|#
(define (add_card_to_crupier deck crupier)
    (list (player_name crupier) 
          (add (player_deck crupier)(take_card deck)) 
          0 
          (player_state crupier)
          (player_score crupier)))


#|
Nombre: add_card_to_player

Descripción:  Esta función toma una carta del mazo y la agrega a la baraja del jugador indicado por el id.
              Recorre la lista de jugadores buscando el jugador al que se desea agregar la carta, cuando lo 
              encuentra saca una carta del mazo y actualiza su baraja. Conforme recorre la lista va formando
              una nueva con los mismos elementos. 

Entradas:     * deck -> baraja del jugador.
              * players -> lista de jugadores de la partida.
              * id -> id del jugador al que se desea agregar la carta.

Salidas: lista con jugadores de la partida actualizada.
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

Entradas:    * game_info -> lista con la información actual de partida.
             * id -> id del que realiza la acción stand. 
             * score -> puntaje del que realiza la acción stand.

Salidas:    La lista del crupier en caso de que sea este el que realiza la acción, de caso contrario devuelve
            la lista con la información actual de la partida actualizada. 
|#
(define (stand game_info id score)
    (cond [(= id 0) 
        (update_crupier_score_and_state (crupier game_info) "stand" score)]  
    
    [(=  id (length (players game_info)))
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

Entradas:    * game_info -> lista con la información actual de partida.

Salidas: lista del crupier con la baraja, estado y puntaje actualizado.
|#

(define (start_crupier game_info)
    (cond   [(lost? (evaluate_deck (player_deck (crupier  game_info)) 11))
                (cond   [(lost? (evaluate_deck (player_deck (crupier  game_info)) 1)) 
                                (update_crupier_score_and_state (crupier game_info) "lost" (evaluate_deck (player_deck (crupier  game_info)) 1))]

                        [(equal21? (evaluate_deck (player_deck (crupier game_info)) 1)) 
                                (update_crupier_score_and_state (crupier game_info) "stand" 21)]

                        [else (crupier_next_move game_info (evaluate_deck (player_deck (crupier game_info)) 1))])]

            [(equal21? (evaluate_deck (player_deck (crupier game_info)) 11))
               (update_crupier_score_and_state (crupier game_info) "stand" 21)]
               
            [else (crupier_next_move game_info (evaluate_deck (player_deck (crupier game_info)) 11))]))


#|
Nombre: crupier_next_move

Descripción:  Esta función complementa la función start_crupier. Se encarga cual debe ser el siguiente 
              moviento del crupier, si la baraja suma más de 17 en ese momento, se decide a realizar 
              la acción "stand". De caso contrario, toma otra carta del mazo realizando la acción hit.

Entradas:     * game_info -> lista con la información actual de la partida
              * score -> puntaje actual de la baraja del crupier 

Salidas: lista del crupier actulizada.
|#
(define (crupier_next_move game_info score)
    (cond [(>= score 17) 
        (stand game_info 0 score)]
    
    [else
        (start_crupier (hit game_info 0))]))

;----------------------- general functionality -----------------------------


#|
Nombre: lost?

Descripción:  Verifica si el puntaje de un jugador supera la cantidad de 21. 

Entradas:     * player_score -> puntaje del jugador

Salidas: true en caso de que el puntaje sume más de 21, false en caso contrario.  
|#
(define (lost? player_score)
    (> player_score 21))




#|
Nombre: equal21?

Descripción:  Verifica si el puntaje de un jugador es igual a 21, es decir, tiene un blackjack. 

Entradas:     * player_score -> puntaje del jugador

Salidas: true en caso de que el puntaje sea igual a 21, false en caso contrario.  
|#
(define (equal21? player_score)
    (= player_score 21))



#|
Nombre: blackjack?

Descripción:  Verifica si el puntaje de un jugador es igual a 21 y tiene dos cartas en su baraja, es decir, tiene un blackjack. 

Entradas:     * player_score -> puntaje del jugador
              * deck -> baraja del jugador

Salidas: true en caso de que el puntaje sea igual a 21 y tenga dos cartas, false en caso contrario.  
|#
(define (blackjack? player_score deck)
    (and (= player_score 21) (= (length deck) 2)))




#|
Nombre: set_initial_cards

Descripción:  Función principal que se encarga de colocar dos cartas en las barajas del crupier y de los 
              jugadores de la partida. Esta función llama dos veces a su auxiliar para obtener el resultado 
              esperado. 

Entradas:     * game_info -> lista con la información actual de la partida

Salidas: lista con la información de la partida actualizada. 
|#
(define (set_initial_cards game_info)
        (set_initial_cards_aux (set_initial_cards_aux game_info (length (players game_info))) (length (players game_info))))


#|
Nombre: set_initial_cards_aux

Descripción:  Función auxiliar de set_initial_cards. Esta función  se encarga de llamarse recursivamente
              hasta que el id llegue a cero. En cada llamada recursiva realiza la acción hit para el id 
              actual. 

Entradas:     * game_info -> lista con la información actual de la partida
              * id -> id del jugador

Salidas: lista con la información actual de la partida actualizada.
|#
(define (set_initial_cards_aux game_info id)
    (cond [(= id 0)
        (hit game_info 0)]
    [else
        (set_initial_cards_aux (hit game_info id) (- id 1))]))


#|
Nombre: game_over? 

Descripción:  Esta función verifica si el juego ya terminó. Como condición para que el juego termine 
              el jugador actual debe ser 0. 

Entradas:     * game_info -> lista con la información actual de la partida.

Salidas: true en caso de que el juego ya haya terminado, false en caso contrario.
|#
(define (game_over? game_info)
    (= (current_player_id game_info) 0))


#|
Nombre: next_player

Descripción:  Esta función se encarga de pasar al siguiente jugador en caso de que el actual haya perdido
              y devolver la lista con la información del juego actualizada.Si el último de los jugadores 
              pierde, el siguiente en jugar sería el crupier, por lo cual se llama la función que comienza
              a aplicar la lógica de este y lo establece como jugador actual. En caso de que el jugador actual
              sea el crupier, actualiza la lista del crupier y la devuelve, a diferencia de los dos casos anteriores. 

Entradas:     * game_info -> lista con la información actual de la partida.
              * new_state -> nuevo estado del jugador o crupier.
              * socre -> puntaje del jugador o crupier.
        
Salidas:     lista con la información actual de la partida actualizada, excepto si el jugador actual es el 
             crupier, si se da este caso, se devuelve la lista del crupier actualizada 
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

Entradas:     *players-> lista con los nombres de los jugadores de la partida

Salidas: lista con la información de la partida. 
|#
(define (bCEj players)
    (set_initial_cards (shuffle_deck (list (create_deck) (list_players  players) (init_crupier) 1))))


