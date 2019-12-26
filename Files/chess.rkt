#lang racket
(provide (all-defined-out))
(struct posn (x y) #:transparent)       	;x is column number and y is the row number
(struct piece (posn colour type) #:mutable) ; posn is "posn"
(struct move (i-posn f-posn moved-piece captured-piece) #:transparent) ; i-posn and f-posn are "posn" structures
(define list-of-moves '())
(define turn 1) ; tells which side's move

;defines the state variables required for castling
(define w-k  #f)
(define b-k  #f)
(define w-r1 #f)
(define w-r2 #f)
(define b-r1 #f)
(define b-r2 #f)
(define (has-king-moved? turn)
  (if (= turn 1) w-k b-k))
(define (has-rook1-moved? turn)
  (if (= turn 1) w-r1 b-r1))
(define (has-rook2-moved? turn)
  (if (= turn 1) w-r2 b-r2))
(define c 0)
; pawn   = 6
; rook   = 5
; bishop = 3
; knight = 4
; king   = 1
; queen  = 2

;initializes the chessboard
(define pawn-b1   (piece (posn 1 7) 0 6))
(define pawn-b2   (piece (posn 2 7) 0 6))
(define pawn-b3   (piece (posn 3 7) 0 6))
(define pawn-b4   (piece (posn 4 7) 0 6))
(define pawn-b5   (piece (posn 5 7) 0 6))
(define pawn-b6   (piece (posn 6 7) 0 6))
(define pawn-b7   (piece (posn 7 7) 0 6))
(define pawn-b8   (piece (posn 8 7) 0 6))
(define rook-b1   (piece (posn 1 8) 0 5))
(define rook-b2   (piece (posn 8 8) 0 5))
(define bishop-b1 (piece (posn 3 8) 0 3))
(define bishop-b2 (piece (posn 6 8) 0 3))
(define knight-b1 (piece (posn 2 8) 0 4))
(define knight-b2 (piece (posn 7 8) 0 4))
(define king-b   	(piece (posn 5 8) 0 1))
(define queen-b   (piece (posn 4 8) 0 2))

(define black (list pawn-b1 pawn-b2 pawn-b3 pawn-b4
                	pawn-b5 pawn-b6 pawn-b7 pawn-b8
                	rook-b1 rook-b2 bishop-b1 bishop-b2
                	knight-b1 knight-b2 king-b queen-b))

(define pawn-w1   (piece (posn 1 2) 1 6))
(define pawn-w2   (piece (posn 2 2) 1 6))
(define pawn-w3   (piece (posn 3 2) 1 6))
(define pawn-w4   (piece (posn 4 2) 1 6))
(define pawn-w5   (piece (posn 5 2) 1 6))
(define pawn-w6   (piece (posn 6 2) 1 6))
(define pawn-w7   (piece (posn 7 2) 1 6))
(define pawn-w8   (piece (posn 8 2) 1 6))
(define rook-w1   (piece (posn 1 1) 1 5))
(define rook-w2   (piece (posn 8 1) 1 5))
(define bishop-w1 (piece (posn 3 1) 1 3))
(define bishop-w2 (piece (posn 6 1) 1 3))
(define knight-w1 (piece (posn 2 1) 1 4))
(define knight-w2 (piece (posn 7 1) 1 4))
(define king-w	(piece (posn 5 1) 1 1))
(define queen-w   (piece (posn 4 1) 1 2))

(define white (list pawn-w1 pawn-w2 pawn-w3 pawn-w4
                	pawn-w5 pawn-w6 pawn-w7 pawn-w8
                	rook-w1 rook-w2 bishop-w1 bishop-w2
                	knight-w1 knight-w2 king-w queen-w))



(define (opp? posn)
  ;checks whether the cell at posn is occupied by opponent or not.
  (let ((colour-set (if (= turn 1) black white)))
	(foldr (lambda(x y) (or (equal? (piece-posn x) posn) y)) #f colour-set)))


(define (same? posn)
  ;checks whether the cell at posn is occupied by moving side's piece or not.
  (let ((colour-set (if (= turn 1) white black)))
	(foldr (lambda(x y) (or (equal? (piece-posn x) posn) y)) #f colour-set)))

(define (add-posn p1 p2)
  (posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))


(define (recover-piece posn) ;returns the piece present at posn. If no piece is present, returns false.
  (define (helper l)
	(cond ((null? l) #f)
      	((equal? posn (piece-posn (car l))) (car l))
      	(else (helper (cdr l)))))
  (helper (append white black)))

(define (same-colour-piece-filter l) ;removes positions with same colour as moving side in a list of positions.
  (cond [(null? l) '()]
    	[(same? (car l)) (same-colour-piece-filter (cdr l))]
    	[#t (cons (car l) (same-colour-piece-filter (cdr l)))]))

(define (capturing-noncapturing l)
  ;converts the list of moves into a list containing two lists
  ;first containing capturing-moves and
  ;other containing non-capturing moves
  (define (helper l1 l2 l)
	(cond ((null? l) (list l1 l2))
      	((opp?  (car l)) (helper (cons (car l) l1) l2 (cdr l)))
      	(#t (helper l1 (cons (car l) l2) (cdr l)))))
  (helper '() '() l))

(define (make-move-list p l)  ; makes move list from posn list
  (if (null? l) '()
  	(let ((i-posn (piece-posn p)))
    	(cons (move i-posn (car l)) (make-move-list p (cdr l))))))

;checks whether a position is in the chessboard
(define (boundness-checker positions)
  (let* ((x (posn-x positions))
     	(y (posn-y positions)))
	(and (> x 0)(< x 9) (> y 0)(< y 9))))

;filters a list using a predicate
(define (filter-hof p l)
  (if (null? l) '()
  	(if (p (car l)) (cons (car l) (filter-hof p (cdr l))) (filter-hof p (cdr l)) )))

(define boundness-filter
  (lambda(l)
	(filter-hof boundness-checker l)))

;generates points in 4 quadrants given a position
(define (symmetric x y)
  (list (cons x y) (cons (- y) x) (cons (- x) (- y)) (cons y (- x))))

(define (symmetric-posn x y)
  (map (lambda(p)(posn (car p) (cdr p))) (symmetric x y)))

;valid moves without removing the pinned ones
(define (valid-moves-for-comp p)
  ((vector-ref v-moves (piece-type p)) (piece-posn p)))

;valid moves
(define (valid-moves p)
  (cond [(= (piece-type p) 1)(king-valid-moves (piece-posn p))]
    	[else (filter (piece-posn p) (valid-moves-for-comp p))]))

;valid moves for knight
(define (knight-valid-moves p-posn)
  (helper-kk
   (map (lambda(p)(add-posn p p-posn))
    	(append (symmetric-posn 1 2)
            	(symmetric-posn -1 2)))))

;valid moves for bishop
(define (bishop-valid-moves postn)
  (stopper (generate-b-cross postn)))

(define (generate-b-cross postn)
  (map (lambda(p)(cross (add-posn postn (posn (car p) (cdr p))) (car p) (cdr p)))
   	(symmetric 1 1)))

;generates list of moves going in 1 direction
;moving (a,b) everytime
(define (cross postn a b)
  (if (boundness-checker postn)
  	(cons postn (cross (add-posn postn (posn a b)) a b))
  	'()))

;takes a list of lists and in each lists and returns list of 2 lists
;1st containing capturing positions and other normal positions
(define (stopper l)
  (let* ((l1 (map (lambda(l2) (reverse (separating l2))) l))
     	(c  (capturing l1))
     	(n  (normal l1)))
	(list c n)))


(define (separating l)
  (if (null? l)  '()
  	(if (opp? (car l))
      	(list (list (car l)))
      	(if (same? (car l)) '()
          	(cons (car l) (separating (cdr l)))))))

;generates list of capturing positions
(define (capturing l)
  (if (null? l) '()
  	(if (null? (car l)) (capturing (cdr l))
      	(if (list? (caar l))
          	(cons (caaar l) (capturing (cdr l)))
          	(capturing (cdr l))))))

;generates list of normal positions
(define (normal l)
  (if (null? l)'()
  	(if (null? (car l)) (normal (cdr l))
      	(if (list? (caar l))
          	(append (cdar l) (normal (cdr l)))
          	(append (car l) (normal (cdr l)))))))

;generates list of valid rook positions
(define (rook-valid-moves postn)
  (stopper (generate-r-cross postn)))

(define (generate-r-cross postn)
  (map (lambda(p)(cross (add-posn postn (posn (car p) (cdr p))) (car p) (cdr p)))
   	(symmetric 1 0)))

;generates list of valid queen positions
(define (queen-valid-moves postn)
  (let* ((a (rook-valid-moves postn))
     	(b (bishop-valid-moves postn))
     	(c (car a))
     	(d (cadr a))
     	(e (car b))
     	(f (cadr b)))
	(list (append c e) (append d f))))

;pawn valid moves
(define (pawn-valid-moves pos)
  (let* ((l1 '())
     	(l2 '())
     	(c (if (= turn 1) 1 -1))
     	(c1 (if (= turn 1) 2 7)))
	(begin (cond ((not (or (same? (add-posn pos (posn 0 c)))
                       	(opp? (add-posn pos (posn 0 c)))))
              	(set! l2 (list (add-posn (posn 0 c) pos)))))
       	(cond ((and (= (posn-y pos) c1)
                   	(not (or (same? (add-posn pos (posn 0 c)))
                            	(opp? (add-posn pos (posn 0 c)))))
                   	(not (or (same? (add-posn pos (posn 0 (* 2 c))))
                            	(opp? (add-posn pos (posn 0 (* 2 c)))))))
              	(set! l2 (cons (add-posn (posn 0 (* 2 c)) pos) l2))))
       	(cond ((opp? (add-posn (posn 1 c) pos))
              	(set! l1 (cons (add-posn (posn 1 c) pos) l1))))
       	(cond ((opp? (add-posn (posn -1 c) pos))
              	(set! l1 (cons (add-posn (posn -1 c) pos) l1))))
       	(cond ((and (= (posn-y pos) (- 9 c1 (* 2 c)))
                   	(> (length list-of-moves) 2)
                   	(= (posn-y (move-i-posn (car list-of-moves))) (- 9 c1))
                   	(= (posn-y (move-f-posn (car list-of-moves))) (- 9 c1 (* 2 c)))
                   	(or (= (- (posn-x pos) 1) (posn-x (move-f-posn (car list-of-moves))))
                       	(= (+ (posn-x pos) 1) (posn-x (move-f-posn (car list-of-moves)))))
                   	(= 6 (piece-type (recover-piece (move-f-posn (car list-of-moves))))))
              	(set! l1 (cons (posn (posn-x (move-f-posn (car list-of-moves))) (+ c (posn-y pos))) l1))))
       	pos (list l1 l2))))



;There are a number of cases when castling is not permitted.
;  Your king has been moved earlier in the game.
;  The rook that castles has been moved earlier in the game.
;  There are pieces standing between your king and rook.
;  The king is in check.
;  The king moves through a square that is attacked by a piece of the opponent.
;  The king would be in check after castling.

(define (castling-k-side?)
  (let ((c (if (= turn 1) 1 8)))
	(if (and (castling-helper (posn 6 c))
         	(castling-helper (posn 7 c))
         	(not (has-king-moved? turn))
         	(not (has-rook2-moved? turn))

         	(safe-posn? (posn 5 c)))
    	#t #f)))

(define (castling-q-side?)
  (let ((c (if (= turn 1) 1 8)))
	(if (and (empty? (posn 2 c))
         	(castling-helper (posn 3 c))
         	(castling-helper (posn 4 c))
         	(not (has-king-moved? turn))
         	(not (has-rook1-moved? turn))
         	(safe-posn? (posn 5 c))) #t #f)))

(define  (king-valid-moves p-posn)  ; not includes castling moves for efficiency
  (append (filter p-posn (king-safe-helper p-posn)) (castling)))



;(define (king-valid-moves p-posn)
;  (let* ((a (castling)))
;	(append (king-valid p-posn) (list a))))

(define (castling)
  (let*((a (if (= turn 1) 1 8))
    	(k1 (if (castling-k-side?)
            	(posn 7 a) '()))
    	(q1 (if (castling-q-side?)
            	(posn 3 a) '())))
	(list k1 q1)))


(define (king-in-check-filter l)
  (filter-hof safe-posn? l))


;returns if king at pos is safe
(define (safe-posn? pos)
  (begin (define king (if (= turn 1) king-w king-b))
     	(define i-posn (piece-posn king))
     	(set-piece-posn! king pos)
     	(define r (and (foldr (lambda(x y)(and (piece-safe? x pos) y)) #t
                           	(list 3 4 5 6))
                    	(king-safe? pos)))
     	(set-piece-posn! king i-posn)
     	r))


(define (king-safe? pos)
  (not-there? 1 (car  (king-safe-helper pos))))

;is the piece of that type attacking a position
(define (piece-safe? n pos)
  (let* ((x (car ((vector-ref v-moves n) pos))))
	(not-there? n x)))

;is there a piece in the list
(define (not-there? n x)
  (define (f a b)
    (let* ((x (recover-piece a)))
      (cond [(eq? x #f) b]
	[else (and (not (= (piece-type (recover-piece a)) n)) b)])))
  (cond [(or(= n 3)(= n 5)) (foldr f (not-there? 2 x) x)]
    	[else (foldr f #t x)]))


(define (remove-piece! piece1)
  ;removes the piece from the list of pieces
  (if (= (piece-colour piece1) 1) (set! white (remove piece1 white))
  	(set! black (remove piece1 black))))

(define (add-piece! piece1)
  ;adds the piece in the corresponding list of pieces
  (if (= (piece-colour piece1) 1) (set! white (cons piece1 white))
  	(set! black (cons piece1 black))))

(define (set-s!)
  ;changes the turn after every move
  (set! turn (remainder (+ 1 turn) 2)))

(define (set-l-move! movec)
  ;adds the last move in the list which maintains the record of previous moves
  (set! list-of-moves (cons movec list-of-moves)))

(define (remove-from-lom!)
  ;removes first element of list of moves
  (set! list-of-moves (cdr list-of-moves)))

(define (set-c!)
  (set! c 0))

(define (update-c!)
  (set! c (+ c 1)))

(define (set-wk! bool)
  (set! w-k bool))

(define (set-bk! bool)
  (set! b-k bool))

(define (set-wr1! bool)
  (set! w-r1 bool))

(define (set-wr2! bool)
  (set! w-r2 bool))

(define (set-br1! bool)
  (set! b-r1 bool))

(define (set-br2! bool)
  (set! b-r2 bool))


(define (set-castle-cond! piece1 bool)
  (cond [(equal? piece1 king-w) (set-wk! bool)]
    	[(equal? piece1 king-b) (set-bk! bool)]
    	[(equal? piece1 rook-w1) (set-wr1! bool)]
    	[(equal? piece1 rook-w2) (set-wr2! bool)]
    	[(equal? piece1 rook-b1) (set-br1! bool)]
    	[(equal? piece1 rook-b2) (set-br2! bool)]))

(define (set-castle-cond-2! piece1 piece2 bool)
  (cond [piece1 (set-castle-cond! piece1 bool)])
  (cond [piece2 (set-castle-cond! piece2 bool)]))

;is a move safe
(define (safe-move? posn1 posn2)
  (let* ((piece (recover-piece posn1))
     	(result (begin (set-piece-posn! piece posn2)
                    	(safe-posn? (if (= turn 1) (piece-posn king-w) (piece-posn king-b))))))
	(begin (set-piece-posn! piece posn1) result)))


(define (safe-moves-filter posn1 l)
  (filter-hof (lambda(x)(safe-move? posn1 x)) l))

;is a move in which there is a capture safe
(define (safe-move-kill? posn1 posn2)
  (let* ((piece (recover-piece posn1))
     	(a (recover-piece posn2))
     	(c (if (= turn 1) -1 1))
     	(piece2 (if (piece? a) a (recover-piece (add-posn posn2 (posn 0 c)))))
     	(piece2-posn (piece-posn piece2))
     	(result (begin (set-piece-posn! piece posn2)
                    	(set-piece-posn! piece2 (posn 0 0))
                    	(safe-posn? (if (= turn 1) (piece-posn king-w) (piece-posn king-b))))))
	(begin (set-piece-posn! piece posn1)
       	(set-piece-posn! piece2 piece2-posn)result)))


(define (safe-moves-filter-kill posn1 l)
  (filter-hof (lambda(x)(safe-move-kill? posn1 x)) l))

(define (check?)
  (let* ((k-pos (if (= 1 turn) (piece-posn king-w)
                	(piece-posn king-b))))
	(not (safe-posn? k-pos))))

(define (checkmate?)
  (and (check?)
   	(null-valid-moves?)))

(define (null-valid-moves?)
  (define (helper l)
	(if (null? l) #t
    	(let* ((v (valid-moves (car l))))
      	(if (and (null? (car v)) (null? (cadr v))) (helper (cdr l))
          	#f))))
  (let* ((l (if (= turn 1) white black)))
	(helper l)))

(define (stalemate?)
  (and (null-valid-moves?)(not (check?))))

(define (draw?)
  (or (stalemate?)
  	(king-king?)
  	(king-bishop-knight?)
  	;(repeat?)
  	(>= c 50)))

(define (king-king?)
  (and (= 1 (length white))
   	(= 1 (length black))))

(define (king-bishop-knight?)
  (or (and (= 1 (length white))
       	(= 2 (length black))(bishop-knight? black))
  	(and (= 1 (length black))
       	(= 2 (length white))(bishop-knight? white))))

(define (bishop-knight? l)
  (if (null? l) #f
  	(or (= 3 (piece-type (car l))) (= 4 (piece-type (car l)))(bishop-knight? (cdr l)))))

;(define (repeat?) #f)
;  (if (> (length list-of-moves) 6)
;  	(and (= (car list-of-moves) (caddr list-of-moves)
;          	(car (cddddr list-of-moves)))
;       	(= (cadr list-of-moves) (cadddr list-of-moves)
;          	(car (cdr (cddddr list-of-moves))))) #f))

(define (filter posn1 l)
  (list (safe-moves-filter-kill posn1 (car l)) (safe-moves-filter posn1 (cadr l))))

(define v-moves (vector #f king-valid-moves
                    	queen-valid-moves
                    	bishop-valid-moves
                    	knight-valid-moves
                    	rook-valid-moves
                    	pawn-valid-moves))

(define (king-safe-helper pos)
  (helper-kk
   (map (lambda(p) (add-posn pos p))
    	(append (symmetric-posn 1 1) (symmetric-posn 1 0)))))

;set of useful higher order functions
(define (seq-op p1 p2)(lambda(l)
                    	(p1 (p2 l))))
(define (empty? p)
  (not ((or-helper same? opp?) p)))


(define filter-helper
  (seq-op same-colour-piece-filter boundness-filter))

(define helper-kk
  (seq-op capturing-noncapturing filter-helper))

(define (or-helper p1 p2) (lambda(p)
                        	(or (p1 p) (p2 p))))

(define (and-helper p1 p2) (lambda(p)
                         	(and (p1 p) (p2 p))))

(define castling-helper (and-helper empty? safe-posn?))
