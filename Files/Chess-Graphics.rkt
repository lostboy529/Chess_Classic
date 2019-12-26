#lang racket/gui
(provide (all-defined-out))
(require 2htdp/image)
(require 2htdp/universe)
(require "chess.rkt")
(require "chess-ai.rkt")
(struct co-ordn (x y) #:transparent)
(define wpawn    (bitmap "Images/pawn1.png"))
(define wknight  (bitmap "Images/knight1.png"))
(define wbishop  (bitmap "Images/bishop1.png"))
(define wrook    (bitmap "Images/rook1.png"))
(define wqueen   (bitmap "Images/queen1.png"))
(define wking    (bitmap "Images/king1.png"))
(define bpawn    (bitmap "Images/pawn2.png"))
(define bknight  (bitmap "Images/knight2.png"))
(define bbishop  (bitmap "Images/bishop2.png"))
(define brook    (bitmap "Images/rook2.png"))
(define bqueen   (bitmap "Images/queen2.png"))
(define bking    (bitmap "Images/king2.png"))
(define chessboard (bitmap "Images/chessboarda.png"))
(define w         (image-width chessboard))
(define h         (image-height chessboard))
(define bcell    (bitmap "Images/bcell.png"))
(define wcell    (bitmap "Images/wcell.png"))
(define allowed-cell (rectangle 62.5 62.5 "solid" (make-color 100 255 255 125)))
(define capture-cell (rectangle 62.5 62.5 "solid" (make-color 247 200 238 225)))
(define present-cell (rectangle 62.5 62.5 "solid" (make-color 100 255 150 170)))
(define promotion-cell (rectangle 62.5 62.5 "solid" (make-color 31 124 224 225)))
(define castle-cell (rectangle 62.5 62.5 "solid" (make-color 31 124 224 225)))
(define check-cell (bitmap "Images/king in check.png"))
(define image-list (list (list bking bqueen bbishop bknight brook bpawn)
                         (list wking wqueen wbishop wknight wrook wpawn)))
(define disable #f)
(define no-ai #f)
(define turn-blocked -1)

(define (set-block i)
  ;sets the value of turn-blocked to i where i is 1 for white, 0 for black , -1 for nothing
  (set! turn-blocked i))

(define (set-no-ai! bool)
  ;sets the value of no-ai to false if
  (set! no-ai bool))

(define now-moving #f)
(define possible-positions '(() ()))
(define moving-piece pawn-b1)

(define white-deads '())
(define black-deads '())

(define being-captured-piece pawn-b1)

(define (set-nowm!)
  ;inverts now-moving (a boolean)
  (set! now-moving (not now-moving)))

(define (cell x y)
  ;given the co-ordinates finds whether it contains a white cell or black cell
  (let* ((cell1 (cell-finder (co-ordn x y))))
    (cell-pos cell1)))

(define (cell-pos position)
  ;given the position, checks whether the cell at it is 
  (if (even? (+ (posn-x position) (posn-y position))) bcell wcell))

(define (piece-image-picker piece1)
  ;picks the image of corresponding piece
  (if piece1
      (let* ((type (piece-type piece1)))
        (list-ref (list-ref image-list (piece-colour piece1)) (- type 1)))
      #f))

(define (co-or position)
  ;gives the standard-coordinates of center of the (x,y) block
  (if on-top-white (co-ordn (- w (+ (* (posn-x position) 62.5) 303)) (- h (+ (* (- 9 (posn-y position)) 62.5) 59)))
      (co-ordn (+ (* (posn-x position) 62.5) 303) (+ (* (- 9 (posn-y position)) 62.5) 59) )))

(define (place-double image1 image2 p1 p2 base)
  ;firstly if p2 false then p2 = p1, and then places image2(if not #f) on p2 and then image1 on p1
  (let* ((x1 (co-or p1))
         (xx1 (co-ordn-x x1))
         (yx1 (co-ordn-y x1))
         (x2 (if p2 (co-or p2) x1))
         (xx2 (co-ordn-x x2))
         (yx2 (co-ordn-y x2)))
    (let* ((a (if image2 (place-image image2 xx2 yx2 base) base)))
      (if image1 (place-image image1 xx1 yx1 a) a))))

(define (filler im position)
  ;finds the piece on position and then places it on the given image
  (let* ((p (recover-piece position)))
    (if p (place-double (piece-image-picker p) (cell-pos position) position #f im)
        (place-double (cell-pos position) #f position #f im))))

(define (present-chessboard)
  ;makes the image of present chessboard (the chessboard at the time of calling this function
  (define (fill x y image)
    (cond [(< x 8) (fill (+ x 1) y (filler image (posn x y)))]
          [(< y 8) (fill 1 (+ y 1) (filler image (posn x y)))]
          [else (filler image (posn x y))]))
  (fill 1 1 chessboard))

(define (fill-full image)
  ;forms complete image of chessboard at the present position
  (check-placer (dead-placer (present-chessboard) white-deads black-deads)))

(define (dead-placer image l1 l2)
  ;places the images of dead pieces on the board, white for l1
  (let* ((a (length l1))
         (b (length l2))
         (l1-placed (foldr (lambda (x y) (begin (set! a (- a 1)) (place-image (piece-image-picker x) (co-ordn-x (co-or-dead 1 a))
                                                                              (co-ordn-y (co-or-dead 1 a)) y))) image l1)))
    
    (foldr (lambda (x y) (begin (set! b (- b 1)) (place-image (piece-image-picker x) (co-ordn-x (co-or-dead 0 b))
                                                              (co-ordn-y (co-or-dead 0 b)) y))) l1-placed l2)))

(define (check-placer image)
  ;places the king on the check-cell if it is in check
  (if (check?) (if (= turn 1) (place-double wking check-cell (piece-posn king-w) #f image)
                   (place-double bking check-cell (piece-posn king-b) #f image))
      image))

; for chessboard 310 746 310
;rook starts at 333 90
;cell dimensions 62.5*62.5

;the size of the cell
(define d 62.5)
; the half size of the cell
(define dby2 31.25)   

(define (co-or-dead colour piece-number)
  ;gives the co-ordinates of pieces which are captured
  (let*((c1 (co-ordn 875.5 (+ 577 (* piece-number -59))))
        (c2 (co-ordn 930.5 (+ 488.5 (* (- piece-number 9) -59)))))
    (if (xor (= colour 1) on-top-white)
        (if (< piece-number 9) c1 c2)
        (let* ((a (if (< piece-number 9) c1 c2)))
          (co-ordn (- w (co-ordn-x a)) (- h (co-ordn-y a)))))))

(define (border position)
  ;determining the bottom-right and the upper-left corner of the cell given its (column-number,row-number) cell number
  ;contract : number(row) number(column) -> cons of co-ordinates of bottom-left and upper-right corners respectively
  (let* ((center (co-or position))
         (center-x (co-ordn-x center))
         (center-y (co-ordn-y center)))
    (cons (co-ordn (- center-x dby2) (- center-y dby2)) (co-ordn (+ center-x dby2) (+ center-y dby2)))))

(define (between-checker x x1 x2)
  ;checks whether given x is between x1 and x2 including both
  (and (<= x x2) (>= x x1)))

(define (inside-checker co-ordinates co-ordinates-box)
  ;checks whether co-ordinates are inside the box formed by the pair of co-ordinates given as the second argument
  (and (between-checker (co-ordn-x co-ordinates) (co-ordn-x (car co-ordinates-box)) (co-ordn-x (cdr co-ordinates-box)))
       (between-checker (co-ordn-y co-ordinates) (co-ordn-y (car co-ordinates-box)) (co-ordn-y (cdr co-ordinates-box)))))

(define (cell-checker position co-ordinates)
  ;to check of the check whether the given co-ordinates lie in the given column and row or not
  (let ((borders (border position)))
    (inside-checker co-ordinates borders)))

(define (cell-finder co-ordinates)
  ;returns the position of the cell which corresponds to the given co-ordinates
  (define (cell-finder-helper column row)
    (cond [(cell-checker (posn column row) co-ordinates) (posn column row)]
          [(< column 8) (cell-finder-helper (+ column 1) row)]
          [(< row 8) (cell-finder-helper 1 (+ row 1))]
          [else (posn 0 0)]))
  (cell-finder-helper 1 1))

(define (to-move image-old moving-piece clicked-cell clicked-piece)
  ;to move the already selected piece according to the rules
  ;notice that turn has already changed before proceding for grahics and changes
  (let* ((x (co-ordn-x (co-or clicked-cell)))
         (y (co-ordn-y (co-or clicked-cell))))
    (cond [(member clicked-cell (car possible-positions))
           (if (not clicked-piece) (begin (set-nowm!) (en-passant clicked-cell moving-piece))
               (begin (set-nowm!) (capture image-old clicked-piece moving-piece)))]
          [(member clicked-cell (cadr possible-positions))
           (begin (set-nowm!) (movep image-old clicked-cell moving-piece))]
          [(equal? clicked-piece moving-piece) (begin (set-nowm!) (fill-full chessboard))]
          [(same? clicked-cell) (begin (set-nowm!) (mouse-event (fill-full chessboard) x y "button-down"))]
          [(and (< 2 (length possible-positions))
                (or (equal? clicked-cell (caddr possible-positions))
                    (equal? clicked-cell (cadddr possible-positions)))) (begin (set-nowm!) (castle image-old clicked-cell))]
          [else (begin (set-nowm!) (fill-full chessboard))])))

(define (castle image-old king-pos-cell)
  ;decides which side castling to be done
  (let* ((x (posn-x king-pos-cell))
         (y (posn-y king-pos-cell)))
    (if (= x 3) (castling-general image-old 'qside (posn-x king-pos-cell)
                                  (posn-y king-pos-cell)
                                  (recover-piece (posn 1 y))
                                  (recover-piece (posn 5 y)) 4)
        (castling-general image-old 'kside (posn-x king-pos-cell)
                          (posn-y king-pos-cell)
                          (recover-piece (posn 8 y))
                          (recover-piece (posn 5 y)) 6))))

(define (castling-general image-old symbol x y r k rf)
  ;procedures which have to be followed for castling
  (begin (set-piece-posn! r (posn rf y))
         (set-s!)
         (set-l-move! symbol)
         (set-castle-cond-2! r k #t)
         (set-piece-posn! k (posn x y))
         (fill-full chessboard)))

(define (en-passant clicked-cell moving-piece)
  ;procedures which have to be followed for en-passant
  (let* ((dying-piece-cell (posn (posn-x clicked-cell) (posn-y (piece-posn moving-piece))))
         (enp-piece (recover-piece dying-piece-cell)))
    (begin (set-l-move! (move (piece-posn moving-piece) dying-piece-cell
                              (recover-piece (piece-posn moving-piece)) enp-piece))
           (set-piece-posn! (recover-piece (piece-posn moving-piece)) clicked-cell)
           (remove-piece! enp-piece)
           (if (= turn 0) (set! white-deads (cons enp-piece white-deads))
               (set! black-deads (cons enp-piece black-deads)))
           (set-s!)
           (fill-full chessboard))))
           

(define (to-select image-old clicked-cell clicked-piece)
  ;selecting the piece which is clicked
  (cond [(same? clicked-cell) (begin (set-nowm!)
                                     (set! possible-positions (valid-moves clicked-piece))
                                     (set! moving-piece clicked-piece)
                                     (highlighter image-old))]
        [else image-old]))

(define (capture image-old to-be-captured captured-by)
  ;following the procedure for capturing a piece by another piece
  (let* ((init (piece-posn captured-by))
         (tail (piece-posn to-be-captured)))
    (begin
      (remove-piece! to-be-captured)
      (set-castle-cond! captured-by #t)
      (set-piece-posn! captured-by tail)
      (if (= turn 0) (set! white-deads (cons to-be-captured white-deads))
          (set! black-deads (cons to-be-captured black-deads)))
      (if (and (= 6 (piece-type captured-by)) (= (posn-y (piece-posn to-be-captured)) (if (= turn 1) 8 1)))
          (begin (set-piece-type! captured-by 2)
                 (set-l-move! (cons 'p (move init tail captured-by to-be-captured))))
          (set-l-move! (move init tail captured-by to-be-captured)))
      (set-c!)
      (set-s!)
      (fill-full chessboard))))

(define (movep image-old final-cell to-be-moved)
  ;following the procedure for the movement for the piece
  (let* ((init (piece-posn to-be-moved))
         (tail final-cell))
    (begin (set-castle-cond! to-be-moved #t)
           (if (= 6 (piece-type to-be-moved)) (set-c!) 1)
           (if (and (= 6 (piece-type to-be-moved)) (= (posn-y final-cell) (if (= turn 1) 8 1)))
               (begin (set-piece-type! to-be-moved 2)
                      (set-l-move! (cons 'p (move init tail to-be-moved #f))))
               (set-l-move! (move init tail to-be-moved #f)))
           (set-piece-posn! to-be-moved final-cell)
           (set-s!)
           (update-c!)
           (fill-full chessboard))))

(define (highlighter image-old)
  ;highlights all the possible positions
  (define a (ordinary-highlighter image-old))
  (cond [(and (= (length possible-positions) 4) (= 1 (piece-type moving-piece)))
         (let* ((third (caddr possible-positions))
                (fourth (cadddr possible-positions)))
           (cond [(and (null? third) (null? fourth)) a]
                 [(null? third) (place-image castle-cell
                                             (co-ordn-x (co-or (cadddr possible-positions)))
                                             (co-ordn-y (co-or (cadddr possible-positions))) a)]
                 [(null? fourth) (place-image castle-cell
                                              (co-ordn-x (co-or (caddr possible-positions)))
                                              (co-ordn-y (co-or (caddr possible-positions))) a)]
                 [else (place-image castle-cell (co-ordn-x (co-or (cadddr possible-positions)))
                                    (co-ordn-y (co-or (cadddr possible-positions)))
                                    (place-image castle-cell (co-ordn-x (co-or (caddr possible-positions)))
                                                 (co-ordn-y (co-or (caddr possible-positions))) a))]))]
         
        [else a]))

(define (ordinary-highlighter image-old)
  ;highlights the positions of possible moves and capture positions
  (let* ((capture-here (car possible-positions))
         (move-here (cadr possible-positions)))
    (capture-highlighter capture-here (move-highlighter move-here (current-highlighter image-old)))))

(define (current-highlighter image)
  ;highlights the current cell
  (place-double (piece-image-picker moving-piece) present-cell (piece-posn moving-piece) #f image))  

(define (move-highlighter l-posn image)
  ;highlights the movable position for the piece
  (foldr (lambda (x y) (place-double #f allowed-cell x #f y)) image l-posn))

(define (capture-highlighter l-posn image)
  ;highlights the capturable position of the piece
  (foldr (lambda (x y) (place-double (piece-image-picker (recover-piece x)) capture-cell x #f y)) image l-posn))

(define (mouse-event image-old x y event)
  ;checks what should be done after window has encountered a mouse click
  (if (string=? event "button-down")
      (if (or disable (= turn-blocked turn))
          image-old
          (let* ((the-cell (cell-finder (co-ordn x y)))
                 (the-piece (recover-piece the-cell)))
            (if now-moving (to-move image-old moving-piece the-cell the-piece)
                (if the-piece (to-select image-old the-cell the-piece) image-old ))))
      image-old))

(define (key-presses image-old key)
  ;reverses the last step on pressing of key z
  (cond [ (key=? key "z")
          (begin (set-s!) (cond [(null? list-of-moves) (begin (set-s!) image-old)]
                                [(equal? 'qside (car list-of-moves)) (undo-castle 'q)]
                                [(equal? 'kside (car list-of-moves)) (undo-castle 'k)]
                                [(pair? (car list-of-moves)) (undo-promotion (car list-of-moves))]
                                [else (undo-move-graphics (car list-of-moves))]))]
        [(and (not no-ai) (key=? key "\r")) (follow-move image-old (car (mini-max-first-step))) ]
        [else image-old]))

(define (follow-move image-old move-tbf)
  ;follows the moves which has to be followed
  (cond [(symbol? move-tbf) (castle image-old (if (equal? move-tbf 'k-side) (posn 7 (+ 8 (* -7 turn))) (posn 3 (+ 8 (* -7 turn)))))]
        [(is-enp? move-tbf) (en-passant (move-f-posn move-tbf) (recover-piece (move-i-posn move-tbf)))]
        [(not (move-captured-piece move-tbf)) (movep image-old (move-f-posn move-tbf) (recover-piece (move-i-posn move-tbf)))]
        [else (capture image-old (recover-piece (move-f-posn move-tbf)) (recover-piece (move-i-posn move-tbf)))]))

 

(define (undo-promotion move)
  ;undo effect of a promotion move
  (let*  ((mp (move-moved-piece (cdr move))))
    (begin (set-piece-type! mp 6) (undo-move-graphics (cdr move)))))

(define (undo-castle sym)
  ;undones the effects of castling
  (define (helper ri rf kf)
    (λ (yp) (let* ((a (recover-piece (posn rf yp)))
                   (b (recover-piece (posn kf yp))))
              (begin (set-piece-posn! a (posn ri yp))
                     (set-piece-posn! b (posn 5 yp))
                     (remove-from-lom!)
                     (set-castle-cond-2! a b #f)
                     (fill-full chessboard)))))
  (cond [(equal? sym 'q) ((helper 1 4 3) (if (= turn 1) 1 8))]
        [(equal? sym 'k) ((helper 8 6 7) (if (= turn 1) 1 8))]))
                                             
(define (undo-move-graphics move)
  ;undo effect of a normal move
  (let*  ((init (move-i-posn move))
          (tail (move-f-posn move))
          (mp (move-moved-piece move))
          (cp (move-captured-piece move)))
                  
    (begin
      (set-piece-posn! mp init)
      (remove-from-lom!)
      (set-castle-cond-2! mp #f #f)
      (if cp
          (begin (add-piece! cp)
                 (if (= turn 0) (set! white-deads (cdr white-deads))
                     (set! black-deads (cdr black-deads)))
                 (fill-full chessboard))
          (fill-full chessboard)))))

(define (main-screen image-old)
  ;decides whether checkmate or draw screen is to be projected or not
  (cond [(checkmate?) (game-end 'win image-old)]
        [(draw?) (game-end 'draw image-old)]
        [else image-old]))

(define (game-end sym image-old)
  ;gives the text showing checkmate and draw
  (let* ((a (cond [(equal? sym 'win) (if (= turn 1) (text "BLACK WINS!!" 50 "black")
                                         (text "WHITE WINS!!" 50 "black"))]
                  [else (text "DRAW!!" 50 "black")])))
    
    (begin (set! disable #t)
           (place-image a (co-ordn-x (co-or (posn 4.5 9.1)))
                        (co-ordn-y (co-or (posn 4.5 9.1)))
                        (place-image a (co-ordn-x (co-or (posn 4.5 -0.2)))
                                     (co-ordn-y (co-or (posn 4.5 -0.2))) image-old)))))

