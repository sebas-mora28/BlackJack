#lang racket

(require racket/gui racket/include)
(include "test.rkt")

(define init-players '())
(define current-game '())
(define as-value 11)
(define player-score 0)
(define current-panel 0)

(define (set-players player-list)
    (set! init-players player-list))

(define (set-current-game player-list)
    (set! current-game (bCEj player-list)))

(define (set-as-value value)
    (set! as-value value))

(define (place-players init-players)
    (substring (place-players-aux init-players) 0 (- (string-length (place-players-aux init-players)) 2)))

(define (place-players-aux init-players)
    (cond [(null? init-players) ""]
          [else (string-append (car init-players) ", " (place-players-aux (cdr init-players)))]))

;; Main function
(define (bCEj_gui . player-list)
    (set-players player-list)
    (cond [(null? init-players) (display "You must enter at least one player's name")]
          [(> (length init-players) 3) (display "The number of players has been exceeded")]
          [else (interface)]))

; Graphic interface for blackjack game
(define (interface)

; Start window
(define start-window 
    (new frame% [label "Start window"] [min-width 600] [min-height 400]))
    
    ; Pane container for start window
    (define star-pane
        (new pane% [parent start-window]))

    ; Starup-background for start window
    (define starup-background
        (new message% [parent star-pane]
                      [label (read-bitmap "src/resources/backgrounds/start-bg.jpg")]))

    ; Start window distribution
    (define start-panel
        (new horizontal-panel% [parent star-pane]))

    (define left-panel
        (new vertical-panel% [parent start-panel] [alignment '(center center)] [horiz-margin 10]))

    (define right-panel
        (new vertical-panel% [parent start-panel] [alignment '(right top)] [horiz-margin 5] (vert-margin 5)))

    ; Starup menu
    (define text-field
        (new text-field% [parent left-panel]
                         [label "Players name: "]
                         [init-value (place-players init-players)]))

    (new button% [parent left-panel]
                 [label "Play"]
                 [callback (λ (b e) (on-play-button (send text-field get-value)))])

    (new button% [parent right-panel]
                 [label "About"]
                 [callback (λ (b e) (on-about-button))])

; Verify text-field entry before start the game
(define (on-play-button player-list)
    (cond [(null? (string-split player-list #rx",")) (error-window "You must enter at least one player's name")]
          [(> (length (string-split player-list #rx",")) 3) (error-window "The number of players has been exceeded")]
          [else (set-current-game (string-split player-list #rx",")) (start-game)]))

;(add-card (card-name (player_deck (current_player current-game)) 0) player-name)           

;-------------------------Create information window------------------------

(define (on-about-button)
    ; Dialog container to show some game development information 
    (define about-dialog
        (new dialog% [label "About"] [min-width 210] [min-height 320]))

    ; Pane container for about window
    (define about-pane
        (new pane% [parent about-dialog]))

    ; Background for about window
    (define about-background
        (new message% [parent about-pane]
                      [label (read-bitmap "src/resources/backgrounds/about-bg.jpeg")]))

    ; About window distribution
    (define about-panel
        (new vertical-panel% [parent about-pane] [alignment '(center center)]))
    
    (define label-panel
        (new horizontal-panel% [parent about-panel] [alignment '(center center)]))

    (define button-panel
        (new horizontal-panel% [parent about-panel] [alignment '(center bottom)] [vert-margin 5]))

    ; About window content
    (new message% [parent label-panel] [label "This is a window information"])

    (new button%  [parent button-panel] [label "Back"] [callback (λ (b e) (send about-dialog show #f))])

    (send about-dialog show #t))

;----------------------------Some pop-up menus-------------------------------

; Create error window
(define (error-window msg)
    (define error-dialog (new dialog% [parent start-window] [label "Error"]))
    (new message% [parent error-dialog] [label msg])
    (new button%  [parent error-dialog]
                  [label "Ok"]
                  [callback (λ (b e) (send error-dialog show #f) (send start-window show #t))])
    (send error-dialog show #t))
    
; Create lost window
(define (lost-window place)
    (define lost-dialog (new dialog% [parent place] [label "You lost"]))
    (new message% [parent lost-dialog] [label "The sum of cards is greater than 21."])
    (new button%  [parent lost-dialog]
                  [label "Ok"]
                  [callback (λ (b e) (send lost-dialog show #f))])
    (send lost-dialog show #t))

; Create automatic stand window
(define (automatic-stand-window place)
    (define auto-stand-dialog (new dialog% [parent place] [label "Automatic stand"]))
    (new message% [parent auto-stand-dialog] [label "The sum of cards equals 21, you may win."])
    (new button% [parent auto-stand-dialog]
                 [label "Ok"]
                 [callback (λ (b e) (send auto-stand-dialog show #f))])
    (send auto-stand-dialog show #t))

; Create stand window
(define (stand-window place player-stand)
    (define stand-dialog (new dialog% [parent place] [label "Stand"]))
    (define v-panel (new vertical-panel% [parent stand-dialog] [alignment '(center center)]))
    (new message% [parent v-panel] [label "Are you sure you want to stand?"])
    (define h-panel (new horizontal-panel% [parent v-panel] [alignment '(center center)]))
    (new button% [parent h-panel]
                 [label "Yes"]
                 [callback (λ (b e) (send stand-dialog show #f) (player-stand))])
    (new button% [parent h-panel]
                 [label "No"]
                 [callback (λ (b e) (send stand-dialog show #f))])
    (send stand-dialog show #t))

;--------------------------Create game window----------------------------
(define (start-game)
    (send start-window show #f)

    ; Make a frame to game window
    (define game-window
        (new frame% [parent start-window] [label "BlaCEjack"] [min-width 1200] [min-height 800]))

    ; Functions for game window
    (define (add-card card place)
        (new message% [parent place]
                      [label (read-bitmap (string-append "src/resources/cards/" card))]))

    (define (card-name card-list pos)
        (string-append (string-join (map ~a (list-ref card-list pos)) "") ".png"))

    (define (init-interface)
        (display "\nPlayers: ")
        (display (players current-game))
        (set! current-panel down-panel)

        (play-sound "src/resources/sounds/dieShuffle1.wav" #t)

        ; Crupier 
        (add-card (car (shuffle '("BackBlue.png" "BackGreen.png" "BackRed.png"))) up-panel)
        (add-card (card-name (cadr (crupier current-game)) 1) up-panel)

        ; First player
        (add-card (card-name (player_deck (car (players current-game))) 0) down-panel)
        (add-card (card-name (player_deck (car (players current-game))) 1) down-panel))

    ; Pane container for game window
    (define game-pane
        (new pane% [parent game-window]))

    ; Background for game window
    (define game-background
        (new message% [parent game-pane]
                      [label (read-bitmap "src/resources/backgrounds/game-bg.jpg")]))

    ; Game window distribution
    (define game-panel
        (new horizontal-panel% [parent game-pane]))
    
    (define left-panel
        (new vertical-panel% [parent game-panel]))

    (define center-panel
        (new vertical-panel% [parent game-panel]))

    (define right-panel
        (new vertical-panel% [parent game-panel]))

    ; Focusing on the left panel
    (define rules-panel
        (new horizontal-panel% [parent left-panel] [alignment '(center center)]))

    (new message% [parent rules-panel] [label "Rules: \n\nPlace the rules here..."])

    (define bottom-panel
        (new horizontal-panel% [parent left-panel] [alignment '(center bottom)] [vert-margin 5]))

    (new button% [parent bottom-panel]
                 [label "Back"]
                 [callback (λ (b e) (send game-window show #f)
                                    (send start-window show #t))])

    ; Focusing on the center panel
    (define crupier-panel
        (new horizontal-panel% [parent center-panel] [alignment '(left bottom)]))

    (new message% [parent crupier-panel] [label "Player: Crupier"])

    (define up-panel
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    (define player-name-panel
        (new horizontal-panel% [parent center-panel] [alignment '(left bottom)]))

    (define player-label 
        (new message% [parent player-name-panel] 
        [label (string-append "Player: " (player_name(car (players current-game))))]))

    (define radio-box
        (new radio-box% [parent center-panel] 
                        [label "As values: "] 
                        [choices (list "A = 11" "A = 1")]
                        [style '(horizontal)]
                        [callback (λ (b e) (on-radio-box))]))

    (define (on-radio-box)
        (cond [(zero? (send radio-box get-selection)) (set-as-value 11)]
              [else (set-as-value 1)]))

    (define down-panel
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    (init-interface)
    ; Agregar función que verifique si el jugador 1 inica ganando

    ; Focusing on the right panel
    (define first-panel
        (new horizontal-panel% [parent right-panel]))

    (define v-first-panel
        (new vertical-panel% [parent first-panel] [alignment '(right top)] [vert-margin 5] [horiz-margin 5]))

    (new button% [parent v-first-panel] [label "Volume"] [callback (λ (b e) (on-volume-button b e))])
    (new button% [parent v-first-panel] [label "Play Again"] [callback (λ (b e)  (on-play-again-button))])

    (define second-panel
        (new horizontal-panel% [parent right-panel] [alignment '(center center)]))

    (new message% [parent second-panel] [label "Place deck image here..."])

    (define third-panel
        (new horizontal-panel% [parent right-panel] [alignment '(center bottom)] [spacing 5] [vert-margin 5]))

    (define hit-button (new button% [parent third-panel] [label "Hit"] [callback (λ (b e) (on-hit-button b e))]))
    (define stand-button (new button% [parent third-panel] [label "Stand"] [callback (λ (b e) (on-stand-button b e))]))

    (define (on-volume-button button event)
        (display "foo"))

    (define (on-play-again-button)
        (send game-window show #f)
        (interface))

    (define (on-hit-button button event)
        (set! current-game (hit current-game (current_player_id current-game)))
        (set! player-score (evaluate_deck (player_deck (current_player current-game)) as-value))

        (play-sound "src/resources/sounds/cardPlace1.wav" #t)
        (add-card (card-name (player_deck (current_player current-game)) 
                  (- (length (player_deck (current_player current-game))) 1)) current-panel)

        (cond [(> player-score 21) (player-lost)]
              [(= player-score 21) (player-auto-stand)]
              [(game_over? current-game) (final-game)]))

    (define (player-lost)
        (set! current-game (next_player current-game "lost" player-score))
        (lost-window game-window)
        (cond [(zero? (current_player_id current-game)) (send hit-button enable #f) (send stand-button enable #f) (display "\nAhora juega el crupier") (game_over? current-game) (final-game)]
              [else (send center-panel delete-child current-panel)
                    (send player-label set-label (string-append "Player: " (player_name (current_player current-game))))
                    (new-player (player_name (current_player current-game)))]))

    (define (player-auto-stand)
        (set! current-game (stand current-game (current_player_id current-game) player-score))
        (automatic-stand-window game-window)
        (cond [(zero? (current_player_id current-game)) (send hit-button enable #f) (send stand-button enable #f) (display "\nAhora juega el crupier") (game_over? current-game) (final-game)]
              [else (send center-panel delete-child current-panel)
                    (send player-label set-label (string-append "Player: " (player_name (current_player current-game))))
                    (new-player (player_name (current_player current-game)))]))

    (define (player-stand)
        (set! player-score (evaluate_deck (player_deck (current_player current-game)) as-value))
        (set! current-game (stand current-game (current_player_id current-game) player-score))

        (play-sound "src/resources/sounds/dieThrow1.wav" #t)

        (cond [(zero? (current_player_id current-game)) (send hit-button enable #f) (send stand-button enable #f) (display "\nAhora juega el crupier")]
              [else (send center-panel delete-child current-panel)
                    (send player-label set-label (string-append "Player: " (player_name (current_player current-game))))
                    (new-player (player_name (current_player current-game)))]))

    (define (new-player player-name)
        (define player-name (new horizontal-panel% [parent center-panel] [alignment '(center center)]))
        (add-card (card-name (player_deck (current_player current-game)) 0) player-name)
        (add-card (card-name (player_deck (current_player current-game)) 1) player-name)
        (set! current-panel player-name))

    (define (on-stand-button button event)
        (stand-window game-window player-stand)
        (cond [(game_over? current-game) (final-game)]))

    (define (final-game)
        (display "\nLista final de los jugadores: ")
        (display (players current-game))
        (display "\nMazo del crupier: ")
        (display (crupier current-game))
        (display "\nEl los ganadores son:")
        (display (winners current-game))
        (winner-window (winners current-game))
        )

    ; Create winners window
    (define (winner-window winner-list)
        (define winner-dialog (new dialog% [parent game-window] [label "Winner"] [min-width 250] [min-height 150]))
        (define v-panel (new vertical-panel% [parent winner-dialog] [alignment '(center center)]))

        (cond [(null? winner-list) (new message% [parent v-panel] [label "Anyone's won"] [vert-margin 50])]            
            [else (place-winner v-panel winner-list)])

        (define h-panel (new horizontal-panel% [parent v-panel] [alignment '(center center)] [vert-margin 5]))
        (new button% [parent h-panel] [label "Play Again"] [callback (λ (b e) (send winner-dialog show #f) (on-play-again-button))])
        (new button% [parent h-panel] [label "Exit"] [callback (λ (b e) (send winner-dialog show #f) (send game-window show #f))])
    
        (send winner-dialog show #t))

    (define (place-winner panel winner-list)
        (cond [(not (null? winner-list)) 
           (place-winner-aux panel (car winner-list)) 
           (place-winner panel (cdr winner-list))]))

    (define (place-winner-aux panel player)
        (cond [(equal? (car player) "crupier") 
           (define (car player) (new horizontal-panel% [parent panel] [alignment '(center center)] [horiz-margin 20]))
           (new message% [parent (car player)] [vert-margin 5] [label (string-append "\n" (~a (player_name player)) " win, score: " (~a (player_score player)) "\n")])
           (place-cards (car player) (player_deck player) 0)]
        
          [else (define (car player) (new horizontal-panel% [parent panel] [alignment '(center center)] [horiz-margin 20]))
           (new message% [parent (car player)] [label (string-append "\n" (~a (player_name player)) " win, score: " (~a (player_score player)) "\n")])
           (place-cards (car player) (player_deck player) 0)]))

    (define (place-cards panel deck n-cards)
        (cond [(not (equal? n-cards (length deck)))
           (add-card (card-name deck n-cards) panel)
           (place-cards panel deck (+ n-cards 1))]))


    (send game-window show #t))

    (send start-window show #t)
)

(bCEj_gui "Pedro" "Carlos" "Juan")
