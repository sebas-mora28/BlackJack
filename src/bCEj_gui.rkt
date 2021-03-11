#lang racket

(require racket/gui)

(define players '())

(define (set-players player-list)
    (set! players player-list))

(define (place-players players)
    (substring (place-players-aux players) 0 (- (string-length (place-players-aux players)) 2)))

(define (place-players-aux players)
    (cond [(null? players) ""]
          [else (string-append (car players) ", " (place-players-aux (cdr players)))]))

;; Main function
(define (bCEj . player-list)
    (set-players player-list)
    (cond [(null? players) (display "You must enter at least one player's name")]
          [(> (length players) 3) (display "The number of players has been exceeded")]
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
                         [init-value (place-players players)]))

    (new button% [parent left-panel]
                 [label "Play"]
                 [callback (λ (b e) (on-play-button b e))])

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

    ; Starup-background for about window
    #|(define about-background
        (new message% [parent star-pane]
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


;; Game window
(define (on-play-button button event)
    (send start-window show #f)

    (define game-window
        (new frame% [parent start-window] [label "BlaCEjack"] [min-width 1000] [min-height 750]))
    
    (send game-window show #t))

    (send start-window show #t)
)


(bCEj "Emanuel" "Carlos")

