#lang racket/gui
(require 2htdp/image)
(require 2htdp/universe)
(require "Files/chess.rkt")
(require "Files/chess-graphics.rkt")
(require "Files/chess-ai.rkt")
(define a (bitmap "Files/Images/HomeScreen.jpg"))
(define b (bitmap "Files/Images/ColourOption.jpg"))
(define cr (bitmap "Files/Images/Credits.jpg"))
(define ins (bitmap "Files/Images/Instructions.jpg"))
(define (mouse image x y event)
  (if (string=? event "button-down") (begin (write x) (write " ") (write y) (newline) image) image))

(define c-box-single (cons (co-ordn 245 130) (co-ordn 488 147)))
(define c-box-double (cons (co-ordn 239 181) (co-ordn 498 198)))
(define c-box-ins (cons (co-ordn 253 237) (co-ordn 480 253)))
(define c-box-credits (cons (co-ordn 291 294) (co-ordn 427 309)))
(define c-box-white (cons (co-ordn 303 188) (co-ordn 392 204)))
(define c-box-black (cons (co-ordn 297 244) (co-ordn 402 262)))

(define (TheChessWorld) (big-bang (present-chessboard)
                                  (on-draw main-screen)
                                  (on-mouse mouse-event)
                                  (on-key key-presses)
                                  (name "CHESS")))
(define (quit image) exit)
(define exit #f)

(define (mouse-event1 image-old x y event)
  
  (if (string=? event "button-down")
      (let* ((c (co-ordn x y)))
        (cond [(inside-checker c c-box-single)
               (begin (set-no-ai! #f)
                      (big-bang b (on-draw d) (on-mouse mouse-event2) (stop-when quit) (close-on-stop #t)) b)]
              [(inside-checker c c-box-double) (begin (set-on-top! #f) (set-no-ai! #t) (TheChessWorld) (set! exit #t) a)]
              [(inside-checker c c-box-ins) (begin (big-bang ins (on-draw d)) a)]
              [(inside-checker c c-box-credits) (begin (big-bang cr (on-draw d)) a)]
              [else image-old]))
      image-old))

(define (mouse-event2 image-old x y event)
  
  (if (string=? event "button-down")
      (let* ((c (co-ordn x y)))
        (cond [(inside-checker c c-box-white) (begin (set-on-top! #f) (set-block 0)  (TheChessWorld) (set! exit #t) a)]
              [(inside-checker c c-box-black) (begin (set-on-top! #t) (set-block 1)  (TheChessWorld)  (set! exit #t) a)]
              [else image-old]))
      image-old))
(define (d image-old) image-old)
(big-bang a (on-draw d) (on-mouse mouse-event1) (stop-when quit) (close-on-stop #t))