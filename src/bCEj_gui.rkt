#lang racket

(require racket/gui racket/include)
(include "bCEj_logic.rkt")

; Funciones globales
(define init-players '())
(define current-game '())
(define as-value 11)
(define player-score 0)
(define current-panel 0)

; Funciones para actualizar las principales variables del juego
#|
Nombre: set-players
Descripción: Actualiza la lista de jugadores al inicio del juego.
Entradas: player-list -> Lista de jugadores actuales.
Salidas: Lista actualizada de los jugadores.
|#
(define (set-players player-list)
    (set! init-players player-list))

#|
Nombre: set-current-game 
Descripción: Inicializa todos los aspectos del juego (mazo, jugadores, crupier y jugador actual).
Entradas: player-list ->  lista de jugadores al empezar el juego.
Salidas: Devuelve el mazo de cartas, la lista de jugadores, el crupier y el jugador actual.
|#
(define (set-current-game player-list)
    (set! current-game (bCEj player-list)))

#|
Nombre: set-as-value
Descripción: Almacena el valor del As (por default vale 11) con el que el jugador pretende un blackjack 
             o sumar 21.
Entradas: value -> Valor del as.
Salidas: Actualiza el valor del As seleccionado por el jugador.
|#
(define (set-as-value value)
    (set! as-value value))

#|
Nombre: place-players
Descripción: Toma la lista de jugadores y los une en un solo string separados por como.
Entradas: init-players -> Lista de jugadores al inicio del juego.
Salidas: String con los jugadores separados por coma.
|#
(define (place-players init-players)
    (substring (place-players-aux init-players) 0 (- (string-length (place-players-aux init-players)) 2)))

(define (place-players-aux init-players)
    (cond [(null? init-players) ""]
          [else (string-append (car init-players) ", " (place-players-aux (cdr init-players)))]))

; Función principal
#|
Nombre: bCEj_gui
Descripción: Función principal que verifica la lista de jugadores ingresados en la ventana de comandos
             al iniciar el programa.
Entradas: player-list -> Lista de jugadores
Salidas: Informa sobre las restricciones con respecto al número de jugadores y llama a la interfaz si
         todo está correcto.
|#
(define (bCEj_gui . player-list)
    (set-players player-list)
    (cond [(null? init-players) (display "You must enter at least one player's name")]
          [(> (length init-players) 3) (display "The number of players has been exceeded")]
          [else (interface)]))

; Interfaz gráfica
#|
Nombre: interface
Descripción: Maneja el flujo y ejecución de todos los aspectos relacionados con la interfaz del juego.
Entradas: No tiene entradas.
Salidas: Interfaz gráfica del juego.
|#
(define (interface)

; Ventana de inicio
#|
Nombre: start-window
Descripción: Crea, distribuye y controla todos los elementos de la ventana de inicio, así como el
             acceso a las demás ventanas.
Entradas: No tiene entradas.
Salidas: Primera ventana al inicia el juego.
|#
(define start-window 
    (new frame% [label "Start window"] [stretchable-width #f] [stretchable-height #f]))
    
    ; Principal contenedor de la ventana
    (define star-pane
        (new pane% [parent start-window]))

    ; Fondo principal
    (define starup-background
        (new message% [parent star-pane]
                      [label (read-bitmap "src/resources/backgrounds/start-bg.png")]))

    ; Distribución de la ventana
    (define start-panel
        (new horizontal-panel% [parent star-pane]))

    (define left-panel
        (new vertical-panel% [parent start-panel] [alignment '(left center)] [horiz-margin 10]))

    (define center-panel
        (new horizontal-panel% [parent left-panel] [alignment '(center center)]))

    (define right-panel
        (new vertical-panel% [parent start-panel] [alignment '(right top)] [horiz-margin 5] (vert-margin 5)))

    ; Menú de inicio
    (define text-field
        (new text-field% [parent center-panel]
                         [label "Players name: "]
                         [horiz-margin 5]
                         [init-value (place-players init-players)]))

    ; Botón para llamar a la ventana de juego
    (new button% [parent center-panel]
                 [label "Play"]
                 [min-width 0]
                 [min-height 0]
                 [callback (λ (b e) (on-play-button (send text-field get-value)))])

    ; Botón para llamar a la ventana de información 
    (new button% [parent right-panel]
                 [label "About"]
                 [callback (λ (b e) (about-window))])

    ; Verificar el campo de texto antes de empezar el juego
    (define (on-play-button player-list)
        (cond [(null? (string-split player-list #rx",")) (error-window "You must enter at least one player's name")]
              [(> (length (string-split player-list #rx",")) 3) (error-window "The number of players has been exceeded")]
              [else (set-current-game (string-split player-list #rx",")) (start-game)]))
          


; Ventana de información
#|
Nombre: about-window
Descripción: Crea, distribuye y controla todos los elementos de la ventana de información.
Entradas: No tiene entradas.
Salidas: Ventana de información.
|#
(define (about-window)
    (define about-dialog
        (new dialog% [label "About"] [stretchable-width #f] [stretchable-height #f]))

    ; Principal contenedor de la ventana
    (define about-pane
        (new pane% [parent about-dialog]))

    ; Distribución de la ventana
    (define about-panel
        (new vertical-panel% [parent about-pane] [alignment '(center center)]))
    
    (define label-panel
        (new horizontal-panel% [parent about-panel] [alignment '(center center)]))

    (define button-panel
        (new horizontal-panel% [parent about-panel] [alignment '(center bottom)] [vert-margin 5]))

    ; Información del juego y su desarrollo
    (new message% [parent label-panel]
                  [label (read-bitmap "src/resources/backgrounds/about-bg.png")])

    ; Botón para regresar a la ventana de inicio
    (new button%  [parent button-panel] [label "Back"] [callback (λ (b e) (send about-dialog show #f))])

    (send about-dialog show #t))

; Algunas ventanas emergentes
#|
Nombre: error-window
Descripción: Ventana que muestra si hubo un error al ingresar el nombre de los jugadores y el número
             de estos.
Entradas: msg -> Indica si se aún no se han ingresado jugadores o si se ha excedido el número de estos.
Salidas: Ventana con mensaje de error en particular.
|#
(define (error-window msg)
    (define error-dialog (new dialog% [parent start-window] [label "Error"] [stretchable-width #f] [stretchable-height #f]))
    (define pane (new pane% [parent error-dialog]))
    (new message% [parent pane]
                  [label (read-bitmap "src/resources/backgrounds/pop-up-bg.jpg")])

    (define panel (new vertical-panel% [parent pane] [alignment '(center center)] [spacing 20]))
    (new message% [parent panel] [label msg])
    (new button%  [parent panel]
                  [label "Ok"]
                  [callback (λ (b e) (send error-dialog show #f) (send start-window show #t))])

    (send error-dialog show #t))
    

#|
Nombre: lost-window
Descripción: Ventana que indica si el jugador actual ha perdido, esto es si la suma de las cartas es 
             mayor a 21.
Entradas: place -> Lugar donde se colocará la ventana.
Salidas: Ventana con mensaje que indica que la suma de cartas es mayor a 21.
|#
(define (lost-window place)
    (define lost-dialog (new dialog% [parent place] [label "You lost"] [stretchable-width #f] [stretchable-height #f]))
    (define pane (new pane% [parent lost-dialog]))
    (new message% [parent pane]
                  [label (read-bitmap "src/resources/backgrounds/pop-up-bg.jpg")])

    (define panel (new vertical-panel% [parent pane] [alignment '(center center)] [spacing 20]))
    (new message% [parent panel] 
                  [label "The sum of cards is greater than 21."])
    (new button%  [parent panel]
                  [label "Ok"]
                  [callback (λ (b e) (send lost-dialog show #f))])

    (send lost-dialog show #t))


#|
Nombre: automatic-stand-window
Descripción: Ventana que indica que el jugador actual podría ganar pues la suma de sus cartas ya es 
             igual a 21.
Entradas: place -> Lugar donde se colocará la ventana.
Salidas: Ventana con mensaje que indica que el jugador actual podría ganar.
|#
(define (automatic-stand-window place)
    (define auto-stand-dialog (new dialog% [parent place] [label "Automatic stand"] [stretchable-width #f] [stretchable-height #f]))
    (define pane (new pane% [parent auto-stand-dialog]))
    (new message% [parent pane]
                      [label (read-bitmap "src/resources/backgrounds/pop-up-bg.jpg")])

    (define panel (new vertical-panel% [parent pane] [alignment '(center center)] [spacing 20]))
    (new message% [parent panel] 
                  [label "The sum of cards equals 21, you may win."])
    (new button%  [parent panel]
                  [label "Ok"]
                  [callback (λ (b e) (send auto-stand-dialog show #f))])

    (send auto-stand-dialog show #t))


#|
Nombre: stand-window
Descripción: Ventana que le pregunta al jugador actual si está seguro o no de plantarse.
Entradas: place -> Lugar donde se colocará la ventana.
          player-stand -> Función para indicar que el jugador actual a decidido plantarse.
Salidas: Ventana de decisión para confirmar si el jugador está seguro de plantarse.
|#
(define (stand-window place player-stand)
    (define stand-dialog (new dialog% [parent place] [label "Stand"] [stretchable-width #f] [stretchable-height #f]))
    (define v-pane (new pane% [parent stand-dialog]))
    (new message% [parent v-pane]
                      [label (read-bitmap "src/resources/backgrounds/pop-up-bg.jpg")])

    (define v-panel (new vertical-panel% [parent v-pane] [alignment '(center center)]))
    (define l-panel (new horizontal-panel% [parent v-panel] [alignment '(center center)]))
    (new message% [parent v-panel] [label "Are you sure you want to stand?"])

    (define h-panel (new horizontal-panel% [parent v-panel] [alignment '(center center)]))
    (new button% [parent h-panel] [label "Yes"]
                 [callback (λ (b e) (send stand-dialog show #f) (player-stand))])
    (new button% [parent h-panel] [label "No"]
                 [callback (λ (b e) (send stand-dialog show #f))])

    (send stand-dialog show #t))


#|
Nombre: blackjack-window
Descripción: Ventana que indica si el jugador actual ha obtenido un blackjack, esto es si la suma de
             las cartas es 21 con dos de estas.
Entradas: place -> Lugar donde se colocará la ventana.
Salidas: Ventana que indica que el jugador actual ha obtenido un blackjack.
|#
(define (blackjack-window place)
    (define bj-dialog (new dialog% [label "Blackjack"] [stretchable-width #f] [stretchable-height #f]))
    (define pane (new pane% [parent bj-dialog]))
    (new message% [parent pane]
                  [label (read-bitmap "src/resources/backgrounds/blackjack-bg.png")])

    (define panel (new vertical-panel% [parent pane] [alignment '(center bottom)]))
    (define h1-panel (new horizontal-panel% [parent panel]))
    (define h2-panel (new horizontal-panel% [parent panel] [alignment '(center bottom)] [vert-margin 20]))

    (new button% [parent h2-panel] [label "Ok"]
                 [callback (λ (b e) (send bj-dialog show #f))])

    (send bj-dialog show #t))



; Ventana de juego
#|
Nombre: start-game
Descripción: Crea, distribuye y controla todos los elementos de la ventana de juego.
Entradas: No tiene entradas.
Salidas: Ventana de juego.
|#
(define (start-game)
    (send start-window show #f)

    ; Marco de la ventana
    (define game-window
        (new frame% [parent start-window] [label "BlaCEjack"] [stretchable-width #f] [stretchable-height #f]))

    ; Principales funciones de la ventana

    ; Función que agrega una carta (card) en lugar especificado (place)
    (define (add-card card place)
        (new message% [parent place]
                      [label (read-bitmap (string-append "src/resources/cards/" card))]))

    ; Función que devuelve el string de una carta de un mazo de cartas(card-list), de acuerdo
    ; con el lugar especificado (pos)
    (define (card-name card-list pos)
        (string-append (string-join (map ~a (list-ref card-list pos)) "") ".png"))

    ; Función que coloca las dos primeras cartas del primer jugador
    (define (init-interface)
        (set! current-panel down-panel)

        (play-sound "src/resources/sounds/dieShuffle.wav" #t)

        ; Primer jugador
        (add-card (card-name (player_deck (car (players current-game))) 0) down-panel)
        (add-card (card-name (player_deck (car (players current-game))) 1) down-panel))

    ; Principal contenedor de la ventana
    (define game-pane
        (new pane% [parent game-window]))

    ; Fondo de la ventana
    (define game-background
        (new message% [parent game-pane]
                      [label (read-bitmap "src/resources/backgrounds/game-bg.png")]))

    ; Distribución de la ventana
    (define game-panel
        (new horizontal-panel% [parent game-pane]))
    
    (define left-panel
        (new vertical-panel% [parent game-panel] [horiz-margin 10] [alignment '(left center)]))

    (define center-panel
        (new vertical-panel% [parent game-panel] [alignment '(center center)]))

    (define right-panel
        (new vertical-panel% [parent game-panel] [alignment '(right center)]))

    ; Centrándose en el panel izquierdo
    (define notes-panel
        (new horizontal-panel% [parent left-panel] [alignment '(left center)]))

    (new message% [parent left-panel]
                  [label (read-bitmap "src/resources/backgrounds/notes.png")])

    (define button-panel
        (new horizontal-panel% [parent left-panel] [alignment '(left bottom)] [vert-margin 20] [horiz-margin 150]))

    (new button% [parent button-panel]
                 [label "Back"]
                 [callback (λ (b e) (send game-window show #f) (send start-window show #t))])

    ; Centrándose en el panel del centro
    (define crupier-panel
        (new horizontal-panel% [parent center-panel] [alignment '(left bottom)]))

    (new message% [parent crupier-panel] [label "Player: Crupier"])

    ; Lugar donde se posicionan las cartas del crupier
    (define up-panel
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    (define player-name-panel
        (new horizontal-panel% [parent center-panel] [alignment '(left bottom)]))

    (define player-label 
        (new message% [parent player-name-panel] 
        [label (string-append "Player: " (player_name(car (players current-game))))]))

    ; Caja de radio para controlar la selección del valor de As que el jugador decida para la suma
    ; del mazo de cartas
    (define radio-box
        (new radio-box% [parent center-panel] 
                        [label "As values: "] 
                        [choices (list "A = 11" "A = 1")]
                        [style '(horizontal)]
                        [callback (λ (b e) (on-radio-box))]))

    ; Función que actualiza el valor del As de acuerdo a la selección del jugador
    (define (on-radio-box)
        (cond [(zero? (send radio-box get-selection)) (set-as-value 11)]
              [else (set-as-value 1)]))

    ; Lugar donde se posicionan las cartas del jugador actual
    (define down-panel
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    ; Primera y segunda carta del Crupier (la primera está boca abajo)
    (define card-a (new message% 
            [parent up-panel]
            [label (read-bitmap (string-append "src/resources/cards/" 
            (car (shuffle '("BackBlue.png" "BackGreen.png" "BackRed.png")))))]))

    (define card-b (new message% 
            [parent up-panel]
            [label (read-bitmap (string-append "src/resources/cards/" (card-name (cadr (crupier current-game)) 1)))]))

    (init-interface)

    ; Centrándose en el panel derecho
    (define first-panel
        (new horizontal-panel% [parent right-panel]))

    (define v-first-panel
        (new vertical-panel% [parent first-panel] [alignment '(right top)] [vert-margin 5] [horiz-margin 5]))

    (new button% [parent v-first-panel] [label "Play Again"] [callback (λ (b e)  (play-again))])

    (define second-panel
        (new horizontal-panel% [parent right-panel] [alignment '(right center)] [horiz-margin 5]))

    (new message% [parent second-panel]
                  [label (read-bitmap "src/resources/cards/mazo.png")])

    (define third-panel
        (new horizontal-panel% [parent right-panel] [alignment '(right bottom)] [spacing 5] [vert-margin 20] [horiz-margin 35]))

    (define hit-button (new button% [parent third-panel] [label "Hit"] [callback (λ (b e) (on-hit-button))]))
    (define stand-button (new button% [parent third-panel] [label "Stand"] [callback (λ (b e) (on-stand-button))]))

    ; Función para volver a empezar el juego
    (define (play-again)
        (send game-window show #f)
        (interface))

    #|
    Nombre: on-hit-button
    Descripción: Función que agrega una carta al mazo de cartas del jugador actual y de ahí determinar 
                 si perdió (player-lost), si debe plantarse (player-auto-stand) o si el juego ya 
                 terminó (game-over?).
    Entradas: No tiene entradas.
    Salidas: Adición de carta en la interfaz o cambio de jugador.
    |#
    (define (on-hit-button)
        (set! current-game (hit current-game (current_player_id current-game)))
        (set! player-score (evaluate_deck (player_deck (current_player current-game)) as-value))

        (play-sound "src/resources/sounds/cardPlace.wav" #t)
        (add-card (card-name (player_deck (current_player current-game)) 
                  (- (length (player_deck (current_player current-game))) 1)) current-panel)

        (cond [(lost? player-score) (player-lost)]
              [(equal21? player-score) (player-auto-stand)]
              [(game_over? current-game) (final-game)]))

    #|
    Nombre: player-lost
    Descripción: Cambia el estado del jugador de "playing" a "lost", muestra una ventana que le indica  
                 al jugador que perdió y la razón de esto, se llama al siguiente jugador o bien al 
                 final del juego.
    Entradas: No tiene entradas.
    Salidas: Cambio de jugador o final del juego.
    |#
    (define (player-lost)
        (set! current-game (next_player current-game "lost" player-score))
        (lost-window game-window)
        (cond [(zero? (current_player_id current-game)) (send hit-button enable #f) (send stand-button enable #f) (game_over? current-game) (final-game)]
              [else (send center-panel delete-child current-panel)
                    (send player-label set-label (string-append "Player: " (player_name (current_player current-game))))
                    (new-player (player_name (current_player current-game)))]))

    #|
    Nombre: player-auto-stand
    Descripción: Función que hace que el jugador se plante cuando la suma de cartas ya es igual a 21.
    Entradas: No tiene entradas.
    Salidas: Se muestra una ventana indicando que el jugador actual podría ganar, se llama al siguiente   
             jugador o bien al final del juego.
    |#
    (define (player-auto-stand)
        (set! current-game (stand current-game (current_player_id current-game) player-score))
        (automatic-stand-window game-window)
        (cond [(zero? (current_player_id current-game)) (send hit-button enable #f) (send stand-button enable #f) (game_over? current-game) (final-game)]
              [else (send center-panel delete-child current-panel)
                    (send player-label set-label (string-append "Player: " (player_name (current_player current-game))))
                    (new-player (player_name (current_player current-game)))]))

    #|
    Nombre: player-stand
    Descripción: Función que le pregunta al jugador si está seguro o no de plantarse.
    Entradas: No tiene entradas.
    Salidas: Posible ventana de blackjack, siguiente jugador, sonido de plantarse o bien el final del  
             juego.
    |#
    (define (player-stand)
        (set! player-score (evaluate_deck (player_deck (current_player current-game)) as-value))
        (cond [(blackjack? player-score (player_deck (current_player current-game))) 
               (blackjack-window game-window) (play-sound "src/resources/sounds/blackjack.wav" #t)])
        (set! current-game (stand current-game (current_player_id current-game) player-score))

        (play-sound "src/resources/sounds/dieThrow.wav" #t)

        (cond [(zero? (current_player_id current-game)) (send hit-button enable #f) (send stand-button enable #f)]
              [else (send center-panel delete-child current-panel)
                    (send player-label set-label (string-append "Player: " (player_name (current_player current-game))))
                    (new-player (player_name (current_player current-game)))]))

    #|
    Nombre: new-player
    Descripción: Función que coloca las dos primeras cartas de un nuevo jugador cuando el anterior 
                 haya perdido o haya decidido plantarse.s
    Entradas: player-name -> Nombre del nuevo jugador.
    Salidas: Las dos primeras cartas del nuevo jugador.
    |#
    (define (new-player player-name)
        (define player-name (new horizontal-panel% [parent center-panel] [alignment '(center center)]))
        (add-card (card-name (player_deck (current_player current-game)) 0) player-name)
        (add-card (card-name (player_deck (current_player current-game)) 1) player-name)
        (set! current-panel player-name))

    #|
    Nombre: on-stand-button
    Descripción: Función que controla la decisión del jugador de plantarse o no.
    Entradas: No tiene entradas.
    Salidas: Nuevo jugador o el final del juego
    |#
    (define (on-stand-button)
        (stand-window game-window player-stand)
        (cond [(game_over? current-game) (final-game)]))

    #|
    Nombre: final-game
    Descripción: Función que controla el final del juego, esto es cuando todos los jugadores hayan
                 jugado y empieza a jugar el crupier. muestra todas las cartas del crupier y una    
                 ventana de las posiciones de los jugadores de acuerdo a la suma de sus cartas y el 
                 valor que sea igual o esté más cercano al 21.
    Entradas: No tiene entradas.
    Salidas: Cartas del crupier y ventana con la tabla de posiciones de los jugadores y el crupier.
    |#
    (define (final-game)
        (send up-panel delete-child card-a)
        (send up-panel delete-child card-b)

        (place-crupier-cards (player_deck (crupier current-game)))
        (winner-window (game_positions current-game)))

    #|
    Nombre: place-crupier-cards
    Descripción: Coloca todas las cartas del crupier una vez que este haya empezado a jugar.
    Entradas: deck -> Mazo de cartas del crupier.
    Salidas: Cartas del crupier en la interfaz.
    |#
    (define (place-crupier-cards deck)
        (cond [(not (null? deck)) 
               (add-card (string-append (string-join (map ~a (car deck)) "") ".png") up-panel) 
               (place-crupier-cards (cdr deck))]))

    #|
    Nombre: winner-window
    Descripción: Ventana que muestra una tabla de posiciones junto con las cartas de los jugadores y
                 el crupier una vez que el juego haya terminado.
    Entradas: winner-list -> Lista ordenada de las posiciones de los jugadores y el crupier.
    Salidas: Ventana de posiciones al finalizar el juego.
    |#
    (define (winner-window winner-list)
        (define winner-dialog (new dialog% [parent game-window] [label "Winner"]))
        (define w-pane (new pane% [parent winner-dialog]))
        (new message% [parent w-pane]
                      [label (read-bitmap "src/resources/backgrounds/winner-bg.png")])

        (define v-panel (new vertical-panel% [parent w-pane] [alignment '(center center)]))
        (place-winner v-panel winner-list)
        (define h-panel (new horizontal-panel% [parent v-panel] [alignment '(center center)]))

        (new button% [parent h-panel] [label "Play Again"] [callback (λ (b e) (send winner-dialog show #f) (play-again))])
        (new button% [parent h-panel] [label "Exit"] [callback (λ (b e) (send winner-dialog show #f) (send game-window show #f))])
    
        (send winner-dialog show #t))

    #|
    Nombre: place-winner
    Descripción: Función que toma el nombre, el puntaje y las cartas de cada jugador y las coloca en
                 la ventana de ganadores en forma de tabala.
    Entradas: panel -> Lugar donde se colocará el jugador.  
              winner-list -> Lista ordenada de las posiciones de los jugadores y el crupier.
    Salidas: Posicionamiento de todos los jugadores y el crupier de forma ordenada en la ventana de         
             ganadores.
    |#
    (define (place-winner panel winner-list)
        (cond [(not (null? winner-list)) 
           (place-winner-aux panel (car winner-list)) 
           (place-winner panel (cdr winner-list))]))

    (define (place-winner-aux panel player)
        (cond [(equal? (car player) "Crupier") 
           (define (car player) (new horizontal-panel% [parent panel] [alignment '(center center)]))
           (new message% [parent (car player)] [label (string-append "\nPlayer: " (~a (player_name player)) ", score: " (~a (player_score player)) "\n")])
           (place-cards (car player) (player_deck player) 0)]
        
          [else (define (car player) (new horizontal-panel% [parent panel] [alignment '(center center)]))
           (new message% [parent (car player)] [label (string-append "\nPlayer: " (~a (player_name player)) ", score: " (~a (player_score player)) "\n")])
           (place-cards (car player) (player_deck player) 0)]))

    #|
    Nombre: place-cards
    Descripción: Función que coloca las cartas del jugador o el crupier en la ventana de ganadores
                 según corresponda.
    Entradas: panel -> Lugar donde se colocarán las cartas del jugador o del crupier.
              deck -> Mazo de cartas del jugador o del crupier.
              n-cards -> Número de cartas del mazo. 
    Salidas: Cartas de cada jugador o el crupier.
    |#
    (define (place-cards panel deck n-cards)
        (cond [(not (equal? n-cards (length deck)))
           (add-card (card-name deck n-cards) panel)
           (place-cards panel deck (+ n-cards 1))]))


    (send game-window show #t))

    (send start-window show #t)
)

(bCEj_gui "Pedro" "Carlos" "Juan")
