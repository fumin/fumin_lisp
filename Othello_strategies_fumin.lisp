;Whenever we are guaranteed that we have enough ply moves to "look to the end,"
;we record the move of our second hand in our look-to-the-end search

;for (othello #'human (fiago 8)), and then E6 for human, spent 118 seconds, 
;as for (othello #'human (iago 8)), spent 219 seconds, or 53.88%
;In summary, the time was speeded up by 78% with zero-window, and 54% with multi-killermoves 

;for memoization, must use (defun fkey (args) (cons (first args) (copy-seq (second args))))
;(memoize 'fedge-stability :key #'fkey :test #'equal), because the pointer to board is always the same, fooling
;"equal" that it is the same key in memoization. However, not of great use because fedge-stability is not
;recursive, and that memoization works best when there's recursion.

(defparameter all-squares
  (sort (loop for i from 11 to 88 
	      when (<= 1 (mod i 10) 8) collect i)
        #'> :key #'(lambda (sq) (elt *weights* sq))))

(defstruct (node) square board value)

(defun alpha-beta-searcher2 (depth eval-fn)
  "Return a strategy that does A-B search with sorted moves."
  #'(lambda (player board)
      (multiple-value-bind (value node)
          (alpha-beta2
            player (make-node :board board
                              :value (funcall eval-fn player board))
            losing-value winning-value depth eval-fn)
        (declare (ignore value))
        (node-square node))))

(defun alpha-beta2 (player node achievable cutoff ply eval-fn)
  "A-B search, sorting moves by eval-fn"
  ;; Returns two values: achievable-value and move-to-make
  (if (= ply 0)
      (values (node-value node) node)
      (let* ((board (node-board node))
             (nodes (legal-nodes player board eval-fn)))
        (if (null nodes)
            (if (any-legal-move? (opponent player) board)
                (values (- (alpha-beta2 (opponent player)
                                        (negate-value node)
                                        (- cutoff) (- achievable)
                                        (- ply 1) eval-fn))
                        nil)
                (values (final-value player board) nil))
            (let ((best-node (first nodes)))
              (loop for move in nodes
                    for val = (- (alpha-beta2
                                   (opponent player)
                                   (negate-value move)
                                   (- cutoff) (- achievable)
                                   (- ply 1) eval-fn))
                    do (when (> val achievable)
                         (setf achievable val)
                         (setf best-node move))
                    until (>= achievable cutoff))
              (values achievable best-node))))))

(defun negate-value (node)
  "Set the value of a node to its negative."
  (setf (node-value node) (- (node-value node)))
  node)

(defun legal-nodes (player board eval-fn)
  "Return a list of legal moves, each one packed into a node."
  (let ((moves (legal-moves player board)))
    (sort (map-into
            moves
            #'(lambda (move)
                (let ((new-board (make-move move player
                                            (copy-board board))))
                  (make-node
                    :square move :board new-board
                    :value (funcall eval-fn player new-board))))
            moves)
          #'> :key #'node-value)))

(defvar *fply-boards*
  (apply #'vector (loop repeat 40 collect (initial-board))))

(defun falpha-beta3 (player board achievable cutoff ply eval-fn
                    killer-l)
  "A-B search, putting killer move first."
  (if (= ply 0)
      (funcall eval-fn player board *move-number*)
      (let ((moves (put-first-l killer-l (legal-moves player board))))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (falpha-beta3 (opponent player) board
                                (- cutoff) (- achievable)
                                (- ply 1) eval-fn nil))
                (final-value player board))
            (let ((best-move (first moves))
                  (new-board (aref *fply-boards* ply))
                  (killer2-l nil)
                  (killer2-val-l nil)
                  temp killer2-val-length)
              (loop for move in moves
                    for wacievable = (- cutoff) then (- (+ achievable 1))
                    do (multiple-value-bind (val reply)
                           (falpha-beta3
                             (opponent player)
                             (make-move move player
                                        (replace new-board board))
                             wacievable (- achievable)
                             (- ply 1) eval-fn killer2-l)
                         (setf val (- val))
                         ;;; the zero-window search
                         (when (and (> val achievable) (not (equalp (first moves) move)))
                           (multiple-value-bind (val2 reply2)
                               (falpha-beta3 
                                 (opponent player)
                                 (make-move move player
                                            (replace new-board board))
                                 (- cutoff) (- val)
                                 (- ply 1) eval-fn killer2-l)
                             (setf val (- val2))
                             (setf reply reply2)))
                         (when (> val achievable)
                           (setf achievable val)
                           (setf best-move move))
                         ;;; special case for the first move, intended to be compatible with iago
                         (when (equalp (first moves) move)
                           (push val killer2-val-l)
                           (push reply killer2-l))
                         (setf killer2-val-length (length killer2-val-l))
                         ;;; we insert "val" and "reply (the move)" if (< val killer2-val).
                         ;;; That is, if it is a killer move in the sense that its "val" is
                         ;;; smaller than someone in the killer2-val list
                         (when reply
                           (loop for killer2-val in killer2-val-l 
                                 for i from 0 to killer2-val-length
                                 until (< val killer2-val) finally 
                                 (unless (eql i killer2-val-length)
                                   (setf temp (nthcdr i killer2-val-l))
                                   (setf killer2-val-l (nconc (ldiff killer2-val-l temp) (list val) temp))
                                   (setf temp (nthcdr i killer2-l))
                                   (setf killer2-l (nconc (ldiff killer2-l temp) (list reply) temp))))))
                    until (>= achievable cutoff))
              (values achievable best-move))))))

(defun falpha-beta-searcher3 (depth eval-fn)
  "Return a strategy that does A-B search with killer moves."
  #'(lambda (player board)
      ;(princ (multiple-value-list (decode-universal-time (get-universal-time))))
      (let* ((ddepth (cond ;((< *move-number* 16) (cond ((< depth 6) depth)
                           ;                            (t 6)))
                           ((< *move-number* 46) (cond ((< depth 6) depth) ;*m*46depth16->5min
                                                       (t 6)))
                           (t (cond ((< depth 5) (* depth 2))
                                    (t 16)))))                         
             (temp (cond ((or (< *move-number* 46) (< depth 5))
                          (multiple-value-bind (value move)
                            (falpha-beta3 player board losing-value winning-value
                                          ddepth eval-fn nil)
                            (declare (ignore value))
                            move))
                         ((gethash (boardi board) *win-moves-ht*);)
                          (princ 'a-) (gethash (boardi board) *win-moves-ht*))
                         (t (multiple-value-bind (value move)
                              (falpha-beta3-2 player board losing-value winning-value
                                              ddepth eval-fn nil 0)
                              (declare (ignore value))
                              move)))))
        ;(princ (multiple-value-list (decode-universal-time (get-universal-time))))
        temp)))

(defun boardi (board)
  "compute a hash-table key for a particular board configuration"
  (let (board-id)
    (dolist (square all-squares)
      (push (aref board square) board-id))
    board-id))

(defun falpha-beta3-2 (player board achievable cutoff ply eval-fn
                       killer-l 0-1-2)
  "A-B search, putting killer move first. 
  Since we assume that by now the ply number
  large enough for us to look-to-the-end,
  for every move of our opponent we record the best
  moves that we have. i.e. the next round we are gauranteed
  to find our best-move in the hash-table."
  (if (= ply 0)
      (funcall eval-fn player board *move-number*)
      (let ((moves (put-first-l killer-l (legal-moves player board))))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (falpha-beta3-2 (opponent player) board
                                (- cutoff) (- achievable)
                                (- ply 1) eval-fn nil (- 0-1-2 1)))         
                (final-value player board))
            (let ((best-move (first moves))
                  (new-board (aref *fply-boards* ply))
                  (killer2-l nil)
                  (killer2-val-l nil)
                  temp killer2-val-length)
              (loop for move in moves
                    for wacievable = (- cutoff) then (- (+ achievable 1))
                    do (multiple-value-bind (val reply)
                           (falpha-beta3-2
                             (opponent player)
                             (make-move move player
                                        (replace new-board board))   
                             wacievable (- achievable)
                             (- ply 1) eval-fn killer2-l (+ 0-1-2 1))
                         (setf val (- val))
                         ;;; the zero-window search
                         (when (and (> val achievable) (not (equalp (first moves) move)))
                           (multiple-value-bind (val2 reply2)
                               (falpha-beta3-2 
                                 (opponent player)
                                 (make-move move player
                                            (replace new-board board))
                                 (- cutoff) (- val)
                                 (- ply 1) eval-fn killer2-l (+ 0-1-2 1))
                             (setf val (- val2))
                             (setf reply reply2)))
                         (when (> val achievable)
                           (setf achievable val)
                           (setf best-move move))
                         ;;; special case for the first move, intended to be compatible with iago
                         (when (equalp (first moves) move)
                           (push val killer2-val-l)
                           (push reply killer2-l))
                         (setf killer2-val-length (length killer2-val-l))
                         ;;; we insert "val" and "reply (the move)" if (< val killer2-val).
                         ;;; That is, if it is a killer move in the sense that its "val" is
                         ;;; smaller than someone in the killer2-val list
                         (when reply
                           (loop for killer2-val in killer2-val-l 
                                 for i from 0 to killer2-val-length
                                 until (< val killer2-val) finally 
                                 (unless (eql i killer2-val-length)
                                   (setf temp (nthcdr i killer2-val-l))
                                   (setf killer2-val-l (nconc (ldiff killer2-val-l temp) (list val) temp))
                                   (setf temp (nthcdr i killer2-l))
                                   (setf killer2-l (nconc (ldiff killer2-l temp) (list reply) temp))))))
                    until (>= achievable cutoff))
              ;;; when the second time "fiago" plays, record the move
              (when (eql 0-1-2 2)                                  
                (setf (gethash (boardi board) *win-moves-ht*) best-move))
              (values achievable best-move))))))

(defun put-first-l (killer-l moves)
  (dolist (killer killer-l)
    (setf moves (put-first killer moves)))
  moves)

(defun put-first (killer moves)
  "Move the killer move to the front of moves,
  if the killer move is in fact a legal move."
  (if (member killer moves)
      (cons killer (delete killer moves))
      moves))

(defun mobility (player board)
  "Current Mobility is the number of legal moves.
  Potential mobility is the number of blank squares
  adjacent to an opponent that are not legal moves.
  Returns current and potential mobility for player."
  (let ((opp (opponent player))
        (current 0)    ; player's current mobility
        (potential 0)) ; player's potential mobility
    (dolist (square all-squares)
      (when (eql (bref board square) empty)
        (cond ((legal-p square player board)
               (incf current))
              ((some #'(lambda (sq) (eql (bref board sq) opp))
                     (neighbors square))
               (incf potential)))))
    (values current (+ current potential))))

(defvar *edge-table* (make-array (expt 3 10))
  "Array of values to player-to-move for edge positions.")

(defconstant edge-and-x-lists
  '((22 11 12 13 14 15 16 17 18 27)
    (72 81 82 83 84 85 86 87 88 77)
    (22 11 21 31 41 51 61 71 81 72)
    (27 18 28 38 48 58 68 78 88 77))
  "The four edges (with their X-squares).")

(defun edge-index (player board squares)
  "The index counts 1 for player; 2 for opponent,
  on each square---summed as a base 3 number."
  (let ((index 0))
    (dolist (sq squares)
      (setq index (+ (* index 3)
                     (cond ((eql (bref board sq) empty) 0)
                           ((eql (bref board sq) player) 1)
                           (t 2)))))
    index))

(defun fedge-stability (player board)
  "Total edge evaluation for player to move on board."
  (loop for edge-list in edge-and-x-lists
        sum (aref *edge-table*
                  (edge-index player board edge-list))))

(defconstant top-edge (first edge-and-x-lists))

(defun init-edge-table ()
  "Initialize *edge-table*, starting from the empty board."
  ;; Initialize the static values
  (loop for n-pieces from 0 to 10 do 
        (map-edge-n-pieces
          #'(lambda (board index)
              (setf (aref *edge-table* index)
                    (static-edge-stability black board)))
          black (initial-board) n-pieces top-edge 0))
  ;; Now iterate five times trying to improve:
  (dotimes (i 5) 
    ;; Do the indexes with most pieces first
    (loop for n-pieces from 9 downto 1 do 
          (map-edge-n-pieces
            #'(lambda (board index)
                (setf (aref *edge-table* index)
                      (possible-edge-moves-value
                        black board index)))
            black (initial-board) n-pieces top-edge 0))))

(defun map-edge-n-pieces (fn player board n squares index)
  "Call fn on all edges with n pieces."
  ;; Index counts 1 for player; 2 for opponent
  (cond
    ((< (length squares) n) nil)
    ((null squares) (funcall fn board index))
    (t (let ((index3 (* 3 index))
             (sq (first squares)))
         (map-edge-n-pieces fn player board n (rest squares) index3)
         (when (and (> n 0) (eql (bref board sq) empty))
           (setf (bref board sq) player)
           (map-edge-n-pieces fn player board (- n 1) (rest squares)
                              (+ 1 index3))
           (setf (bref board sq) (opponent player))
           (map-edge-n-pieces fn player board (- n 1) (rest squares)
                              (+ 2 index3))
           (setf (bref board sq) empty))))))

(defun possible-edge-moves-value (player board index)
  "Consider all possible edge moves. 
  Combine their values into a single number."
  (combine-edge-moves
    (cons
      (list 1.0 (aref *edge-table* index)) ;; no move
      (loop for sq in top-edge             ;; possible moves
            when (eql (bref board sq) empty)
            collect (possible-edge-move player board sq)))
    player))

(defun possible-edge-move (player board sq)
  "Return a (prob val) pair for a possible edge move."
  (let ((new-board (replace (aref *fply-boards* player) board)))
    (make-move sq player new-board)
    (list (edge-move-probability player board sq)
          (- (aref *edge-table*
                   (edge-index (opponent player)
                               new-board top-edge))))))

(defun combine-edge-moves (possibilities player)
  "Combine the best moves."
  (let ((prob 1.0)
        (val 0.0)
        (fn (if (eql player black) #'> #'<)))
    (loop for pair in (sort possibilities fn :key #'second)
          while (>= prob 0.0)
          do (incf val (* prob (first pair) (second pair)))
             (decf prob (* prob (first pair))))
    (round val)))

(let ((corner/xsqs '((11 . 22) (18 . 27) (81. 72) (88 . 77))))
  (defun corner-p (sq) (assoc sq corner/xsqs))
  (defun x-square-p (sq) (rassoc sq corner/xsqs))
  (defun x-square-for (corner) (cdr (assoc corner corner/xsqs)))
  (defun corner-for (xsq) (car (rassoc xsq corner/xsqs))))

(defun edge-move-probability (player board square)
  "What's the probability that player can move to this square?"
  (cond
    ((x-square-p square) .5) ;; X-squares
    ((legal-p square player board) 1.0) ;; immediate capture
    ((corner-p square) ;; move to corner depends on X-square
     (let ((x-sq (x-square-for square)))
       (cond
         ((eql (bref board x-sq) empty) .1)
         ((eql (bref board x-sq) player) 0.001)
         (t .9))))
    (t (/ (aref
            '#2A((.1  .4 .7)
                 (.05 .3  *)
                 (.01  *  *))
            (count-edge-neighbors player board square)
            (count-edge-neighbors (opponent player) board square))
          (if (legal-p square (opponent player) board) 2 1)))))

(defun count-edge-neighbors (player board square)
  "Count the neighbors of this square occupied by player."
  (count-if #'(lambda (inc)
                (eql (bref board (+ square inc)) player))
            '(+1 -1)))

(defparameter *static-edge-table*
  '#2A(;stab  semi    un 
       (   *    0 -2000) ; X
       ( 700    *     *) ; corner
       (1200  200   -25) ; C
       (1000  200    75) ; A
       (1000  200    50) ; B
       (1000  200    50) ; B
       (1000  200    75) ; A
       (1200  200   -25) ; C
       ( 700    *     *) ; corner
       (   *    0 -2000) ; X
       ))

(defun static-edge-stability (player board)
  "Compute this edge's static stability"
  (loop for sq in top-edge
        for i from 0
        sum (cond
              ((eql (bref board sq) empty) 0)
              ((eql (bref board sq) player)
               (aref *static-edge-table* i
                     (piece-stability board sq)))
              (t (- (aref *static-edge-table* i
                          (piece-stability board sq)))))))

(let ((stable 0) (semi-stable 1) (unstable 2))
  
  (defun piece-stability (board sq)
    (cond
      ((corner-p sq) stable)
      ((x-square-p sq)
       (if (eql (bref board (corner-for sq)) empty)
           unstable semi-stable))
      (t (let* ((player (bref board sq))
                (opp (opponent player))
                (p1 (find player board :test-not #'eql
                          :start sq :end 19))
                (p2 (find player board :test-not #'eql
                          :start 11 :end sq
                          :from-end t)))
           (cond
             ;; unstable pieces can be captured immediately
             ;; by playing in the empty square
             ((or (and (eql p1 empty) (eql p2 opp))
                  (and (eql p2 empty) (eql p1 opp)))
              unstable)
             ;; Semi-stable pieces might be captured
             ((and (eql p1 opp) (eql p2 opp)
                   (find empty board :start 11 :end 19))
              semi-stable)
             ((and (eql p1 empty) (eql p2 empty))
              semi-stable)
             ;; Stable pieces can never be captured
             (t stable)))))))

(defun fIago-eval (player board move-number)
  "Combine edge-stability, current mobility and
  potential mobility to arrive at an evaluation."
  ;; The three factors are multiplied by coefficients
  ;; that vary by move number:
  (let ((c-edg (+ 312000 (* 6240 move-number)))
        (c-cur (if (< *move-number* 25)
                   (+ 50000 (* 2000 move-number))
                   (+ 75000 (* 1000 move-number))))
        (c-pot 20000))
    (multiple-value-bind (p-cur p-pot)
        (mobility player board)
      (multiple-value-bind (o-cur o-pot)
          (mobility (opponent player) board)
        ;; Combine the three factors into one sum:
        (+ (round (* c-edg (fedge-stability player board)) 32000)
           (round (* c-cur (- p-cur o-cur)) (+ p-cur o-cur 2))
           (round (* c-pot  (- p-pot o-pot)) (+ p-pot o-pot 2)))))))

(defun fIago (depth)
  "Use an approximation of Iago's evaluation function."
  (falpha-beta-searcher3 depth #'fiago-eval))

