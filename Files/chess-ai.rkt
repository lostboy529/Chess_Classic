#lang racket
(require "chess.rkt")
(define ply 4)
(provide (all-defined-out))

;next eleven variables to be changed in case board rotates
(define on-top-white #f)
(define (set-on-top! bool)
  ;sets according to position of white on top or on bottom
  (set! on-top-white bool))

(define (make-move movep)
  ;makes the move for ai (not on graphics)
  (cond [(is-enp? movep) (enp movep)]
        [(symbol? movep) (castle-move movep)]
        [(is-promoting? movep) (promote movep)]
        [else (normal-move movep)]))

;first step of minimax
(define (mini-max-first-step)
  (define (helper list-of-moves)
    (cond ((null? list-of-moves) (cons 'dummy -100000))
          (#t (begin
                (define move (car list-of-moves))
                (define a (helper (cdr list-of-moves)))
                (make-move move)
                (define value (mini-max-other-steps min (- ply 1) (cdr a)))
                (define result (if (> value (cdr a))
                                   (cons move value) a))
                (undo-move move)
                result))))
  (helper (all-valid-moves)))

;minimax using alpha-beta pruning ,uses higher order min/max
(define (mini-max-other-steps max/min ply1 alpha-beta)
  (define (helper1 list-of-moves)
    (cond ((null? list-of-moves) (if (eq? max/min max) -100000 100000))
          (#t (begin
                (define move (car list-of-moves))
                (define a (helper1 (cdr list-of-moves)))
                (define op (if (eq? max/min max) >= <=))
                (cond  ((op a alpha-beta) alpha-beta)
                       (#t (begin (make-move move)
                                  (define b (mini-max-other-steps
                                             (if (eq? max/min max) min max) (- ply1 1) a))
                                  (undo-move move)
                                  (max/min b a))))))))
 
  (if (= ply1 0)
      (board-evaluator (remainder (+ ply turn) 2))
      (helper1 (all-valid-moves))))


(define (is-enp? move-input)
  ;check whether the move in en-passant or not
  (let* ((init (move-i-posn move-input))
         (tail (move-f-posn move-input))
         (mp (move-moved-piece move-input))
         (cp (move-captured-piece move-input)))
    (and (not cp) (= 6 (piece-type mp)) (not (= (posn-x init) (posn-x tail))))))

(define (is-promoting? movep)
  ;checks whether the move is a promoting move or not
  (let* ((tail (move-f-posn movep))
         (mp (move-moved-piece movep)))
    (and (= 6 (piece-type mp)) (or (= 8 (posn-y tail)) (= 1 (posn-y tail))))))

(define (promote movep)
  ;promotes the pawn
  (begin (set-piece-type! (move-moved-piece movep) 2)
         (normal-move movep)))

(define (castle-move movep)
  ;castles
  (let* ((kp (if (= turn 0) king-b king-w))
         (rp (if (equal? movep 'kside) (if (= turn 0) rook-b2 rook-w2)
                 (if (= turn 0) rook-b1 rook-w1)))
         (y (if (= turn 0) 8 1)))
    (if (equal? movep 'kside)
        (begin (set-piece-posn! kp (posn 7 y))
               (set-piece-posn! rp (posn 6 y))
               (set-castle-cond-2! rp kp #t)
               (set-l-move! 'kside)
               (set-s!))
   	 
        (begin (set-piece-posn! kp (posn 3 y))
               (set-piece-posn! rp (posn 4 y))
               (set-castle-cond-2! rp kp #t)
               (set-l-move! 'qside)
               (set-s!))) 	))

(define (normal-move movep)
  ;sets the norma
  (begin (set-piece-posn! (move-moved-piece movep) (move-f-posn movep))
         (if (move-captured-piece movep) (remove-piece! (move-captured-piece movep)) 1)
         (set-l-move! movep)
         (set-s!)))

(define (enp movep)
  (let* ((init (move-i-posn movep))
         (tail (move-f-posn movep))
         (x1 (posn-x init))
         (x2 (posn-x tail))
         (y1 (posn-y init))
         (y2 (posn-y tail))
         (p1 (recover-piece (posn x2 y1))))
    (begin (set-piece-posn! p1 (posn x2 y2)) (normal-move movep))))
                         	 
                         	 
                                 	 

(define (undo-move movep)
  (cond [(null? list-of-moves) (set-s!)]
        [(symbol? (car list-of-moves)) (undo-castling (car list-of-moves))]
        [(is-promoting? movep) (undo-promotion-of-pawn (car list-of-moves))]
        [else (undo-normal-move (car list-of-moves))]))

(define (undo-normal-move movep)
  (begin (set-piece-posn! (move-moved-piece movep) (move-i-posn movep))
         (if (move-captured-piece movep) (add-piece! (move-captured-piece movep)) 1)
         (remove-from-lom!)
         (set-s!)))

(define (undo-castling sym)
  ;undones the effects of castling
  (define (helper ri rf kf)
    (λ (yp) (let* ((a (recover-piece (posn rf yp)))
                   (b (recover-piece (posn kf yp))))
              (begin (set-piece-posn! a (posn ri yp))
                     (set-piece-posn! b (posn 5 yp))
                     (remove-from-lom!)
                     (set-castle-cond-2! a b #f)
                     ))))
  (cond [(equal? sym 'q) ((helper 1 4 3) (if (= turn 1) 1 8))]
        [(equal? sym 'k) ((helper 8 6 7) (if (= turn 1) 1 8))]))

(define (undo-promotion-of-pawn movep)
  (begin (set-piece-type! (move-moved-piece movep))
         (set-piece-posn! (move-moved-piece movep) (move-i-posn movep))
         (if (move-captured-piece movep) (remove-piece! (move-captured-piece movep)) 1)
         (set-l-move! movep)
         (set-s!)))
                                        	 
(define (undo-enp movep)
  (let* ((init (move-i-posn movep))
         (tail (move-f-posn movep))
         (x1 (posn-x init))
         (x2 (posn-x tail))
         (y1 (posn-y init))
         (y2 (posn-y tail))
         (p1 (piece (posn x2 y1) turn 6)))
    (begin (add-piece! p1) (undo-normal-move movep))))

 
(define value-list (list 10000 900 340 325 500 100)) ;material value of king queen bishop knight rook pawn in order
(define mobility-factors (list 1 3 10 10 6 1)) ;mobility value of king queen bishop knight rook pawn in order

(define (material-value piece1)
  ;gives the material value of the piece
  (list-ref value-list (- (piece-type piece1) 1)))

(define (mobility-value piece1)
  ;gives the mobility value of the piece
  (let* ((postns (valid-moves piece1)))
    (* (list-ref mobility-factors (- (piece-type piece1) 1))
       (+ (length (car postns))
          (length (cadr postns)) (length postns) -2))))

;pawn scoring values
(define doubled-pawns -7)
(define isolated-pawn -2) ;isolated pawn is the pawn which has no other pawn on his adjacent sides
;(define isolani-pawn -3) ;queen's isolated pawn
(define passed-pawn 14) ;passed pawn is the one who has no other pawn in front of him in his file or in the adjacent fil


(define (taxicab-distance posn1 posn2)
  ;taxicab distance between two points is the sum of the absolute differences of their Cartesian coordinates
  (+ (abs (- (posn-x posn1) (posn-x posn2))) (abs (- (posn-y posn1) (posn-y posn2)))))

(define (chebychev-distance posn1 posn2)
  ;chebychev distance between two points is the max of the absolute differences of their Cartesian coordinates
  (max (abs (- (posn-x posn1) (posn-x posn2))) (abs (- (posn-y posn1) (posn-y posn2)))))

(define (board-evaluator sym)
  ;evaluates the board from the side of current turn
  (if (equal? sym 1) (evaluate-from-side-of-white)
      (* -1 (evaluate-from-side-of-white))))

(define (evaluate-from-side-of-white)
  ;evaluates difference of  points from side of white

  (define (helper l total-point)
    (if (null? l) total-point
        (let* ((current-piece (car l)))
          (helper (cdr l)
                  (+ total-point
                     (foldr (lambda (x y) (+ (x current-piece) y)) 0
                            (list-ref func-lst (- (piece-type current-piece) 1))))))))
 
  (- (helper white 0) (helper black 0)))  	 

;generates list of all valid moves for a side
(define (all-valid-moves)
  (define (helper-move l)
    (if (null? l) '()
        (let* ((v  (car l))
               (p (piece-posn v))
               (valid (if (check?) (valid-moves v)
                          (valid-moves-for-comp v)))
               (v1 (car valid))
               (v2 (cadr valid)))
          (append
           (generate-c-move-filter p v v1)
           (generate-nc-move-filter p v v2)
           (helper-move (cdr l))))))
  (define (generate-c-move-filter p v l)
    (map (lambda(fp)(move p fp v (recover-piece fp))) l))
  (define (generate-nc-move-filter p v l)
    (map (lambda(fp)(move p fp v #f)) l))
  (helper-move (if (= turn 1) white black)))

(define knight-centre-value 15)

;gives value if knight is at centre
(define (centre-knight p)
  (let* ((x (posn-x (piece-posn p)))
         (y (posn-y (piece-posn p))))
    (if (and (< x 7)(> x 2)(< y 7)(> y 2)) knight-centre-value 0)))

(define queen-initial-val -15)
;does not allow queen to move initially
(define (queen-initial p)
  (if (< (length list-of-moves) 8)
      (if (and (= 4 (posn-x (piece-posn p)))
               (or (= 1 (posn-y (piece-posn p)))
                   (= 8 (posn-y (piece-posn p))))) 0
                                                   queen-initial-val)
      0))

;gives value to more value to pawn if in enemy territory
(define pawn-values (vector 0 0 0 5 10 15 20 30))
(define (pawn-rank p)
  (let* ((y (posn-y (piece-posn p)))
         (z (if (= 1 (piece-colour p)) y (- 9 y))))
    (vector-ref pawn-values z)))

;list of functions to be used
(define func-lst (list (list material-value)
                       (list material-value  queen-initial)
                       (list material-value)
                       (list material-value centre-knight)
                       (list material-value)
                       (list material-value pawn-rank)))
