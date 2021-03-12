#lang racket

(require racket/gui racket/include)
(include "bCEj_logic.rkt")

(define init-players '())
(define current-game '())

(define (set-players player-list)
    (set! init-players player-list))

(define (set-current-game player-list)
    (set! current-game (bCEj player-list)))

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
          [else (interface)])
    )

;; Graphic interface for blackjack game
(define (interface)

;; Start window
(define start-window 
    (new frame% [label "Start window"]
                [min-width 600]
                [min-height 400]))
    
    ; Pane container for start window
    (define star-pane
        (new pane% [parent start-window]))

    ; Starup-background for start window
    #|(define starup-background
        (new message% [parent star-pane]
                      [label (read-bitmap "src/resources/backgrounds/Blackjack-Table.png")]))|#

    ; Star window distribution
    (define start-panel
        (new horizontal-panel% [parent star-pane]))

    (define left-panel
        (new vertical-panel% [parent start-panel] [alignment '(center center)] [horiz-margin 60]))

    (define right-panel
        (new vertical-panel% [parent start-panel] [alignment '(right top)] [horiz-margin 5] (vert-margin 5)))

    ; Starup menu
    (define text-field
        (new text-field% [parent left-panel]
                         [label "Players name: "]
                         [init-value (place-players init-players)]))

    (new button% [parent left-panel]
                 [label "Play"]
                 [callback (λ (b e) (on-play-button b e (send text-field get-value)))])

    (new button% [parent right-panel]
                 [label "About"]
                 [callback (λ (b e) (on-about-button b e))])


;; Create information window
(define (on-about-button button event)
    ; Dialog container to show some game development information 
    (define about-dialog
        (new dialog% [label "About"] [min-width 400] [min-height 400]))

    ; Pane container for about window
    (define about-pane
        (new pane% [parent about-dialog]))

    ; Background for about window
    #|(define about-background
        (new message% [parent about-pane]
                      [label (read-bitmap "foo.png")]))|#

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

;; Create error window
(define (error-window msg)
    (define error-dialog (new dialog% [parent start-window] [label "Error"]))
    (new message% [parent error-dialog] [label msg])
    (new button%  [parent error-dialog]
                  [label "Ok"]
                  [callback (λ (b e) (send error-dialog show #f) (send start-window show #t))])
    (send error-dialog show #t))

;; Verify text-field entry before start the game
(define (on-play-button button event player-list)
    (cond [(null? (string-split player-list #rx",")) (error-window "You must enter at least one player's name")]
          [(> (length (string-split player-list #rx",")) 3) (error-window "The number of players has been exceeded")]
          [else (set-current-game (string-split player-list #rx",")) (start-game)]))

;; Create game window
(define (start-game)
    (send start-window show #f)

    (display current-game)

    ; Make a frame to game window
    (define game-window
        (new frame% [parent start-window] [label "BlaCEjack"] [min-width 1000] [min-height 750]))

    ; Functions for game window
    (define (add-card card place)
        (new message% [parent place]
                      [label (read-bitmap (string-append "src/resources/cards/" card))]))

    (define (card-name card-list pos)
        (string-append (string-join (map ~a (list-ref card-list pos)) "") ".png"))

    (define (init-interface)
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
    #|(define game-background
        (new message% [parent game-pane]
                      [label (read-bitmap "foo.png")]))|#

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

    (define button-panel
        (new horizontal-panel% [parent left-panel] [alignment '(center bottom)] [vert-margin 5]))

    (new message% [parent rules-panel] [label "Rules: \n\nPlace the rules here..."])
    (new button% [parent button-panel]
                 [label "Back"]
                 [callback (λ (b e) (send game-window show #f)
                                    (send start-window show #t))])

    ; Focusing on the center panel
    (define up-panel
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    (define down-panel
        (new horizontal-panel% [parent center-panel] [alignment '(center center)]))

    (init-interface)

    ; Focusing on the right panel
    (define first-panel
        (new horizontal-panel% [parent right-panel]))

    (define v-first-panel
        (new vertical-panel% [parent first-panel] [alignment '(right top)] [vert-margin 5] [horiz-margin 5]))

    (new button% [parent v-first-panel] [label "Volume"] [callback (λ (b e) (on-volume-button b e))])
    (new button% [parent v-first-panel] [label "Play Again"] [callback (λ (b e)  (on-play-aganin-button b e))])

    (define second-panel
        (new horizontal-panel% [parent right-panel] [alignment '(center center)]))

    (new message% [parent second-panel] [label "Place deck image here..."])

    (define third-panel
        (new horizontal-panel% [parent right-panel] [alignment '(center bottom)] [spacing 5] [vert-margin 5]))

    (new button% [parent third-panel] [label "Hit"] [callback (λ (b e) (on-hit-button b e))])
    (new button% [parent third-panel] [label "Stand"] [callback (λ (b e) (on-stand-button b e))])

    
    (define (on-volume-button button event)
        (display "foo"))

    (define (on-play-aganin-button button event)
        (display "foo"))

    (define (on-hit-button button event)
        (display "foo"))

    (define (on-stand-button button event)
        (display "foo"))

    (send game-window show #t))

    (send start-window show #t)
)


(bCEj_gui "Pedro" "Carlos" "Juan")

