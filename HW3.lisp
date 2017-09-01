;;;*************************************************************************
;;; Name: Neel Kotak
;;;
;;; Professor: Kim Binsted ICS 361
;;;
;;; HW3: A* algorithm for solving problems
;;;*************************************************************************

(defvar *open* NIL);open list
(defvar *close* NIL);close list
(defvar *solution-path* NIL);solution-path list at end
(defvar *farmer-start* '(1 1 1 1));start state for farmer puzzle
(defvar *jug-start* '(0 0));start state for jug puzzle
(defvar *8puzzle-easy* '(2 8 3 1 6 4 7 0 5));start state for easy 8 puzzle
(defvar *8puzzle-hard* '(1 3 4 8 2 0 5 7 6));start state for hard 8 puzzle
(defvar *8puzzle-impossible* '(7 8 0 4 6 8 3 1 2));start state for impossible 8 puzzle
(defvar *farmer-moves* '(MOVE-GOAT MOVE-ALONE MOVE-CABBAGE MOVE-WOLF));moves for farmer puzzle
(defvar *jug-moves* '(FILL5 FILL3 EMPTY3 EMPTY5 PUT5INTO3 PUT3INTO5));moves for jug puzzle
(defvar *8puzzle-moves-default* '(move-right-default move-left-default move-down-default move-up-default));moves for default heuristic 8 puzzle
(defvar *8puzzle-man-dist-moves* '(move-left-man-dist move-right-man-dist move-down-man-dist move-up-man-dist));moves for man-dist heuristic 8 puzzle
(defvar *8puzzle-custom-moves* '(move-right-custom move-left-custom move-down-custom move-up-custom));moves for custom heuristic 8 puzzle
(defvar *farmer-goal* '(0 0 0 0));farmer goal state
(defvar *jug-goal* '(4 0));jug goal State
(defvar *8puzzle-goal* '(1 2 3 8 0 4 7 6 5));8 puzzle goal State

;;;*************************************************************************
;;;
;;; Calls recursive-a-star on the problem with start state
;;; goal state, and moves as inputs
;;;
;;;*************************************************************************
(defun a-star-search (start goal moves);(calc-heuristic-farmer start)
        (setq node (make-node start NIL 0 5))
        (setq *start* node)
        (recursive-a-star node goal moves)
)

;;;*************************************************************************
;;;
;;; Performs A* algorithm recursively on the problem with node
;;; goal state, moves as parameters. prints node out each iteration
;;;
;;;*************************************************************************
(defun recursive-a-star (node goal moves)
    (format t "node: ~s~%" node)
    ;(format t "closed~s~%" *close*)
    ;(format t "open~s~%" *open*)
    (cond
        ((equal NIL node);;node is NIL, move on
            (setq *open* (cdr *open*))
            (recursive-a-star (car *open*) goal moves)
        )
        ((equal goal (get-state node));;goal state is reached, print solution-path
            (format t "Length of open list: ~D~%Length of close list: ~D~%"
            (length *open*) (length *close*))
            (return-from recursive-a-star (solution-path node))
        )
        ((or (equal (get-state node) NIL) (equal 50 (get-depth node)));;state is NIL or depth limit of 20 is reached, move on
            (setq *open* (cdr *open*));update open list
            (recursive-a-star (get-state *open*) goal moves)
        )
        (t;;none of the above, perfom moves and add lowest heuristic to open list
            (let ((node-cpy (copy-tree node)) (moves-cpy moves) (move-len (length moves)))
                (setq *open* (cdr *open*));update open list
                (loop while (> move-len 0)
                    do
                    (cond
                        ((equal (length *open*) 0)
                            ;(print (car moves-cpy))
                            (setq *open* (cons (funcall (car moves-cpy) node) *open*))
                            (cond
                                ((equal (car *open*) NIL)
                                    (setq *open* NIL)
                                )
                            )
                        )
                        (t
                            ;(print (car moves-cpy))
                            (setq *open* (insertion-sort (funcall (car moves-cpy) node-cpy) *open*))
                        )
                    )
                    (setq moves-cpy (cdr moves-cpy))
                    (setq move-len (- move-len 1))
                )
            )
            (setq *close* (cons node *close*));add node to closed list
            (recursive-a-star (car *open*) goal moves);recursive call on lowest heuristic State
        )
    )
)

;;;*************************************************************************
;;;
;;; Prints solution path for node, works backward from goal state to start State
;;; takes goal node as parameter
;;;
;;;*************************************************************************
(defun solution-path (goal-node)
    (print "Solution Path: ")
    (setq cost NIL)
    (let ((temp nil) (curr goal-node))
        (loop while(> (car(cdr(reverse curr))) 0);traverse closed list
            do
            (setq temp (car *close*))
            (COND
                ( (equal (LENGTH *close*) 1)
                  (setq *solution-path* (CONS (car *close*) *solution-path*))
                  (setq cost curr)
                  (setq curr temp)
                )
                ( (equal (car(cdr (reverse temp))) (- (car (cdr(reverse curr))) 1))
                  (COND
                    ( (equal (car temp) (car (cdr curr)))
                      (setq *solution-path* (CONS curr *solution-path*))
                      (setq curr temp)
                    )
                  )
                )
            )
            (setq *close* (cdr *close*))
            (setq temp (car *close*))
        )
    )
    (setq temp (car *solution-path*))
    (setq *solution-path* (CONS temp (CONS cost (cdr *solution-path*))))
    (setq *solution-path* (reverse *solution-path*))
    (return-from solution-path *solution-path*)
)

;;;*************************************************************************
;;;
;;; Adds node to *open*, which is a min priority queue, if it is duplicate, then
;;; doesnt add to list
;;; takes node and queue as parameter
;;;
;;;*************************************************************************
(defun insertion-sort (node q)
    ;checks if state has alrady been previously reached or is NIL
    ;if so, returns nil
    (loop for x in *open*
        do
        (cond
            ((equal (get-state node) (get-state x))
                (return-from insertion-sort q)
            )
        )
    )
    (loop for y in *close*
        do
        (cond
            ((equal (get-state node) (get-state y))
                (return-from insertion-sort q)
            )
        )
    )
    (cond
        ((equal NIL node)
            (return-from insertion-sort q))
    )
    ;if none of above, add to queue
    (if (null q)
        (cons node NIL)
        (if (<= (get-heuristic node) (get-heuristic (car q)))
            (cons node q)
            (cons (car q) (insertion-sort node (cdr q)))
        )
    )
)

;;;*************************************************************************
;;;
;;; creates a node by listing state, parent, depth, and heuristic together
;;; takes state, parent, depth, and heuristic as parameters
;;;
;;;*************************************************************************
(defun make-node (state parent depth heuristic)
    (return-from make-node (list state parent depth heuristic))
)
;;;*************************************************************************
;;;
;;; returns state of a node (1st element)
;;; takes node as parameter
;;;
;;;*************************************************************************
(defun get-state (node)
    (car node)
)

;;;*************************************************************************
;;;
;;; returns parent of a node (2nd element)
;;; takes node as parameter
;;;
;;;*************************************************************************
(defun get-parent (node)
    (car (cdr node))
)

;;;*************************************************************************
;;;
;;; returns depth of a node (3rd element)
;;; takes node as parameter
;;;
;;;*************************************************************************
(defun get-depth (node)
    (car (cdr (cdr node)))
)

;;;*************************************************************************
;;;
;;; returns depth of a node (3rd element)
;;; takes node as parameter
;;;
;;;*************************************************************************
(defun get-heuristic (node)
    (car (reverse node))
)

;;;*************************************************************************
;;;
;;; returns heuristic to farmer, goat, cabbage, wolf problem
;;; takes state as parameter
;;;
;;;*************************************************************************
(defun calc-heuristic-farmer (state)
    (let ((counter 0))
    (cond
        ((equal (car state) 1)
            (setq counter (+ counter 1)))
    )
    (cond
        ((equal (car (cdr state)) 1)
            (setq counter (+ counter 1)))
    )
    (cond
        ((equal (car (cdr (cdr state))) 1)
            (setq counter (+ counter 1)))
    )
    (cond
        ((equal (car (reverse state )) 1)
            (setq counter (+ counter 1)))
    )
    (return-from calc-heuristic-farmer counter)
))

;;;*************************************************************************
;;; Moves for Farmer, Wolf, Goat, and Cabbage problem are below
;;;
;;; Represented by 4-tuple (Cabbage-on-Right Goat-on-Right Wolf-on-Right Location-of-Farmer)
;;; 0 means false, 1 means true, and for Location-of-Farmer, L means left and R means right
;;;
;;; start: (1 1 1 1) -> EVERYONE ON LEFT SIDE
;;; goal: (0 0 0 0) -> EVERYONE ON RIGHT SIDE
;;;
;;; POSSIBLE MOVES:
;;; 1. MOVE-ALONE - FARMER MOVES ALONE; CHANGES 4TH ELEMENT IN TUPLE
;;; 2. MOVE-GOAT - FARMER MOVES WITH GOAT; CHANGES 2ND AND 4TH ELEMENT IN TUPLE
;;; 3. MOVE-CABBAGE - FARMER MOVES WITH CABBAGE; CHANGES 1ST AND 4TH ELEMENT IN TUPLE
;;; 4. MOVE-WOLF - FARMER MOVES WITH WOLF; CHANGES 3RD AND 4TH ELEMENT IN TUPLE
;;;
;;;*************************************************************************

(DEFUN MOVE-ALONE (node)
    (COND
        ((equal 0 (car (reverse (get-state node))))
            (setq new-state (CHANGE-FARMER '1 (get-state node)))
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-state node))
            (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
            )

        (T
            (setq new-state (CHANGE-FARMER 0 (get-state node)))
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-state node))
            (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
        )
    )
)

(DEFUN MOVE-CABBAGE (node)
    (COND
        ((equal 1 (car (get-state node)))
            (COND
                ((equal 0 (car (reverse (get-state node))))
                (setq new-state (CHANGE-CABBAGE 0 1 (get-state node)))
                )
                (T (setq new-state (CHANGE-CABBAGE 0 0 (get-state node)))
                )
            )
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-state node))
            (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
            )

        (T
            (COND
                ((equal 0 (car (reverse (get-state node))))
                (setq new-state (CHANGE-CABBAGE 1 1 (get-state node)))
                )
                (T (setq new-state (CHANGE-CABBAGE 1 0 (get-state node))))
            )
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-state node))
        (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
        )
    )
)

(DEFUN MOVE-GOAT (node)
    (COND
        ((equal 1 (car (cdr (get-state node))))
            (COND
                ((equal 0 (car (reverse (get-state node))))
                (setq new-state (CHANGE-GOAT 0 1 (get-state node)))
                )
                (T (setq new-state (CHANGE-GOAT 0 0 (get-state node)))
                )
            )
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-state node))
            (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
            )

        (T
            (COND
                ((equal 0 (car (reverse (get-state node))))
                (setq new-state (CHANGE-GOAT 1 1 (get-state node)))
                )
                (T (setq new-state (CHANGE-GOAT 1 0 (get-state node))))
            )
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-parent node))
        (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
        )
    )
)

(DEFUN MOVE-WOLF (node)
    (COND
        ((equal 1 (car (cdr (cdr (get-state node)))))
            (COND
                ((equal 0 (car (reverse (get-state node))))
                    (setq new-state (CHANGE-WOLF 0 1 (get-state node)))
                )
                (T
                    (setq new-state (CHANGE-WOLF 0 0 (get-state node)))
                )
            )
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-parent node))
            (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
        )

        (T
            (COND
                ((equal 0 (car (reverse (get-state node))))
                (setq new-state (CHANGE-WOLF 1 1 (get-state node)))
                )
                (T (setq new-state (CHANGE-WOLF 1 0 (get-state node))))
            )
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-state node))
        (make-node new-state newparent newdepth (calc-heuristic-farmer new-state))
        )
    )
)

(DEFUN CHANGE-CABBAGE (CABBAGE FARMER LIST);;change 1st and last element in list
    (cond
        ((not(equal (car list) (car (reverse list))));;no change if on different sides
            (return-from CHANGE-CABBAGE NIL)
        )
        (t
            (setq list (cons CABBAGE (cdr list)))
            (setq list (reverse(cons FARMER (cdr(reverse list)))))
        )
    )
)


(DEFUN CHANGE-GOAT (GOAT FARMER LIST);;change 2nd and last element in list
    (cond
        ((not(equal (car list) (car (cdr list))));;no change if on different sides
            (return-from CHANGE-GOAT NIL)
        )
        (t
            (setq list (cons (car list) (cons GOAT (cdr (cdr list)))))
            (setq list (reverse(cons FARMER (cdr(reverse list)))))))
)

(DEFUN CHANGE-WOLF (WOLF FARMER LIST);;change 3rd and last element in list
    (cond
        ((not(equal (car list) (car(cdr (cdr list)))));;no change if on different sides
            (return-from CHANGE-WOLF NIL)
        )
       (t
       (setq list (cons (car list)(cons (car (cdr LIST))(cons wolf (cdr (cdr (cdr list)))))))
       (setq list (reverse(cons FARMER (cdr(reverse list))))))
    )
)

(DEFUN CHANGE-FARMER (ITEM LIST);;change 4th element in list
    (reverse(cons item (cdr(reverse list)))))

;;;*************************************************************************
;;; Moves for jug problem are below (I DIDNT FINISH, ONLY HAVE STATES NO NODES)
;;;
;;; Represented by 2-tuple (5 gallon jug gallons 3 gallon jug amount)
;;; 1st element range 0-5
;;; 2nd element range 0-3
;;;
;;; start: (0 0) -> Both jugs empty
;;; goal: (4 0) -> Have four gallons of water on one side
;;;
;;; POSSIBLE MOVES:
;;; 1. FILL 5 GALLON JUG FULLY
;;; 2. FILL 3 GALLON JUG FULLY
;;; 3. EMPTY 5 GALLON JUG FULLY
;;; 4. EMPTY 3 GALLON JUG FULLY
;;; 5. PUT 5 GALLON JUG INTO 3 GALLON JUG
;;; 6. PUT 3 GALLON JUG INTO 5 GALLON JUG
;;;*************************************************************************
(DEFUN FILL5 (node);Fills 5 gallon jug
    (COND
        ((equal(car (get-state node)) 5)
        (return-from FILL5 NIL))
    (T
        (setq new-state (cons 5 (cdr (get-state node))))
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-state node))
        (return-from FILL5 (make-node new-state newparent newdepth (calc-heuristic-jugs new-state)))
    ))
)

(DEFUN FILL3 (node);fills 3 gallon jug
    (COND
        ((equal (car (rest (get-state node))) 3)
        (return-from FILL3 NIL))
    (T
        (setq new-state (reverse(cons 3 (cdr (reverse (get-state node))))))
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-state node))
        (return-from FILL3 (make-node new-state newparent newdepth (calc-heuristic-jugs new-state)))
    ))
)

(DEFUN EMPTY5 (node);emppties 5 gallon jug
    (COND
        ((equal(car (get-state node)) 0)
        (return-from EMPTY5 NIL))
    (T
        (setq new-state (cons 0 (cdr (get-state node))))
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-state node))
        (return-from EMPTY5 (make-node new-state newparent newdepth (calc-heuristic-jugs new-state)))
    ))
)

(DEFUN EMPTY3 (node);;empties 3 gallon jug
    (COND
        ((equal (car (cdr (get-state node))) 0)
        (return-from EMPTY3 NIL))
    (T
        (setq new-state (reverse(cons 0 (cdr (reverse (get-state node))))))
        (setq newdepth (+ (get-depth node) 1))
        (setq newparent (get-state node))
        (return-from EMPTY3 (make-node new-state newparent newdepth (calc-heuristic-jugs new-state)))
    ))
)

(DEFUN PUT5INTO3 (node);fills 3 from 5
    (setq 5g (car (get-state node)))
    (setq 3g (car (reverse (get-state node))))
    (COND
        ((OR (equal 0 5g) (equal 3 3g))
            (return-from PUT5INTO3 NIL))
        (T
            (loop while (AND  (> 5g 0) (< 3g 3)) do
                (setq 5g (- 5g 1));minus from 5 gallon
                (setq 3g (+ 3g 1));for each gallon minused, add to 3 gallon
            )
            (setq new-state (cons 5g (cdr (get-state node))))
            (setq new-state (reverse(cons 3g (cdr(reverse new-state)))))
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-state node))
            (return-from PUT5INTO3 (make-node new-state newparent newdepth (calc-heuristic-jugs new-state)))
        )
    )
)

(DEFUN PUT3INTO5 (node);fills 5 from 3
    (setq 5g (car (get-state node)))
    (setq 3g (car (cdr (get-state node))))
    (COND
        ((OR (equal 5g 5) (equal 3g 0)) (return-from PUT3INTO5 NIL))
        (T
            (loop while (AND (NOT (equal 5 5g)) (NOT (equal 0 3g))) do
                (setq 5g (+ 5g 1));minus from 5 gallon
                (setq 3g (- 3g 1));for each gallon minused, add to 3 gallon
            )
            (setq new-state (cons 5g (cdr (get-state node))))
            (setq new-state (reverse (cons 3g (cdr(reverse new-state)))))
            (setq newdepth (+ (get-depth node) 1))
            (setq newparent (get-state node))
            (return-from PUT3INTO5 (make-node new-state newparent newdepth (calc-heuristic-jugs new-state))))
        )
)


;;;*************************************************************************
;;;
;;; returns heuristic to water jug problem
;;; takes node and goal state as parameter
;;;
;;;*************************************************************************
(defun calc-heuristic-jugs (state);just difference between 4 and current state, hard coded for ease
    (cond
        ((= (car state) 5)
            (return-from calc-heuristic-jugs 1)
        )
        ((= (car state) 4)
            (return-from calc-heuristic-jugs 0)
        )
        ((= (car state) 3)
            (return-from calc-heuristic-jugs 1)
        )
        ((= (car state) 2)
            (return-from calc-heuristic-jugs 2)
        )
        ((= (car state) 1)
            (return-from calc-heuristic-jugs 3)
        )
        ((= (car state) 0)
            (return-from calc-heuristic-jugs 4)
        )
    )
)

;;;*************************************************************************
;;; Moves for 8-puzzle problem are below
;;;
;;; State representation
;;;
;;; Initial state:
;;;
;;; 2 8 3
;;; 1 6 4
;;; 7 0 5
;;;
;;; list representation: (2 8 3 1 6 4 7 0 5)
;;;
;;; Goal State:
;;;
;;; 1 2 3
;;; 8 0 4
;;; 7 6 5
;;;
;;; list representation: (1 2 3 8 0 4 7 6 5)
;;;
;;; NOTE: man-dist = manhattan heuristic
;;;       custom = custom heuristic
;;;       default = default heuristic
;;;
;;; POSSIBLE MOVES:
;;; 1. MOVE-LEFT - moves blank tile (represented by 0 in list representation) left
;;; 2. MOVE-RIGHT - moves blank tile (represented by 0 in list representation) right
;;; 3. MOVE-UP - moves blank tile (represented by 0 in list representation) up
;;; 4. MOVE-DOWN - moves blank tile (represented by 0 in list representation) down
;;;*************************************************************************
(defun move-left-default (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 0 (index 0 (get-state node)))
            (return-from move-left-default NIL)
        )
        ((equal 3 (index 0 (get-state node)))
            (return-from move-left-default NIL)
        )
        ((equal 6 (index 0 (get-state node)))
            (return-from move-left-default NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (swap (get-state newnode) (nth (- (index 0 (get-state newnode)) 1) (get-state newnode)) 0))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-left-default (make-node newstate newparent newdepth (calc-heuristic-8puzzle newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-left-man-dist (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 0 (index 0 (get-state node)))
            (return-from move-left-man-dist NIL)
        )
        ((equal 3 (index 0 (get-state node)))
            (return-from move-left-man-dist NIL)
        )
        ((equal 6 (index 0 (get-state node)))
            (return-from move-left-man-dist NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (swap (get-state newnode) (nth (- (index 0 (get-state newnode)) 1) (get-state newnode)) 0))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-left-man-dist (make-node newstate newparent newdepth (calc-man-dist-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)
(defun move-left-custom (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 0 (index 0 (get-state node)))
            (return-from move-left-custom NIL)
        )
        ((equal 3 (index 0 (get-state node)))
            (return-from move-left-custom NIL)
        )
        ((equal 6 (index 0 (get-state node)))
            (return-from move-left-custom NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (swap (get-state newnode) (nth (- (index 0 (get-state newnode)) 1) (get-state newnode)) 0))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-left-custom (make-node newstate newparent newdepth (calc-custom-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)
(defun move-right-default (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 2 (index 0 (get-state node)))
            (return-from move-right-default NIL)
        )
        ((equal 5 (index 0 (get-state node)))
            (return-from move-right-default NIL)
        )
        ((equal 8 (index 0 (get-state node)))
            (return-from move-right-default NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (+ (index 0 (get-state newnode)) 1) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-right-default (make-node newstate newparent newdepth (calc-heuristic-8puzzle newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-right-man-dist (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 2 (index 0 (get-state node)))
            (return-from move-right-man-dist NIL)
        )
        ((equal 5 (index 0 (get-state node)))
            (return-from move-right-man-dist NIL)
        )
        ((equal 8 (index 0 (get-state node)))
            (return-from move-right-man-dist NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (+ (index 0 (get-state newnode)) 1) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-right-man-dist (make-node newstate newparent newdepth (calc-man-dist-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-right-custom (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 2 (index 0 (get-state node)))
            (return-from move-right-custom NIL)
        )
        ((equal 5 (index 0 (get-state node)))
            (return-from move-right-custom NIL)
        )
        ((equal 8 (index 0 (get-state node)))
            (return-from move-right-custom NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (+ (index 0 (get-state newnode)) 1) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-right-custom (make-node newstate newparent newdepth (calc-custom-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-up-default (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 0 (index 0 (get-state node)))
            (return-from move-up-default NIL)
        )
        ((equal 1 (index 0 (get-state node)))
            (return-from move-up-default NIL)
        )
        ((equal 2 (index 0 (get-state node)))
            (return-from move-up-default NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (- (index 0 (get-state newnode)) 3) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-up-default (make-node newstate newparent newdepth (calc-heuristic-8puzzle newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-up-man-dist (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 0 (index 0 (get-state node)))
            (return-from move-up-man-dist NIL)
        )
        ((equal 1 (index 0 (get-state node)))
            (return-from move-up-man-dist NIL)
        )
        ((equal 2 (index 0 (get-state node)))
            (return-from move-up-man-dist NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (- (index 0 (get-state newnode)) 3) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-up-man-dist (make-node newstate newparent newdepth (calc-man-dist-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-up-custom (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 0 (index 0 (get-state node)))
            (return-from move-up-custom NIL)
        )
        ((equal 1 (index 0 (get-state node)))
            (return-from move-up-custom NIL)
        )
        ((equal 2 (index 0 (get-state node)))
            (return-from move-up-custom NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (- (index 0 (get-state newnode)) 3) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-up-custom (make-node newstate newparent newdepth (calc-custom-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-down-default (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 6 (index 0 (get-state node)))
            (return-from move-down-default NIL)
        )
        ((equal 7 (index 0 (get-state node)))
            (return-from move-down-default NIL)
        )
        ((equal 8 (index 0 (get-state node)))
            (return-from move-down-default NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (+ (index 0 (get-state newnode)) 3) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-down-default (make-node newstate newparent newdepth (calc-heuristic-8puzzle newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-down-man-dist (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 6 (index 0 (get-state node)))
            (return-from move-down-man-dist NIL)
        )
        ((equal 7 (index 0 (get-state node)))
            (return-from move-down-man-dist NIL)
        )
        ((equal 8 (index 0 (get-state node)))
            (return-from move-down-man-dist NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (+ (index 0 (get-state newnode)) 3) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-down-man-dist (make-node newstate newparent newdepth (calc-man-dist-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)

(defun move-down-custom (node)
    (setq newparent (copy-tree (get-state node)))
    (setq newnode (copy-tree node))
    (cond
        ((equal 6 (index 0 (get-state node)))
            (return-from move-down-custom NIL)
        )
        ((equal 7 (index 0 (get-state node)))
            (return-from move-down-custom NIL)
        )
        ((equal 8 (index 0 (get-state node)))
            (return-from move-down-custom NIL)
        )
        (t
            (let ((newstate NIL) (newdepth NIL) (newparent NIL))
                (setq newstate (copy-tree (swap (get-state newnode) (nth (+ (index 0 (get-state newnode)) 3) (get-state newnode)) 0)))
                (setq newdepth (+ (get-depth node) 1))
                (return-from move-down-custom (make-node newstate newparent newdepth (calc-custom-heuristic newstate *8puzzle-goal*)))
            )
        )
    )
)
;;;*************************************************************************
;;;
;;; returns index of inputed element in inputted list
;;; takes element and state as parameter
;;;
;;;*************************************************************************

(defun index (element state)
    (setq ind 0)
    (loop for x in state
        do
        (cond ((equal element (nth ind state))
            (return-from index ind))
        )
        (setq ind (+ ind 1))
    )
)

;;;*************************************************************************
;;;
;;; swaps 2 inputted elements inputted list
;;; takes state and 2 elements as parameter
;;;
;;;*************************************************************************
(defun swap (State element1 element2)
   (rotatef
       (car (member element1 state))
       (car (member element2 state))
       )
   (return-from swap state)
)

;;;*************************************************************************
;;;
;;; returns heuristic to 8 puzzle problem using default heuristic
;;; takes node and goal state as parameter
;;;
;;;*************************************************************************
(defun calc-heuristic-8puzzle (curr-state goal-state)
    (setq i 0)
    (setq heuristic 0)
    (loop for element in goal-state
        do
        (cond
            ((not (equal element (nth i curr-state)))
                (setq heuristic (+ heuristic 1))
            )
        )
        (setq i (+ i 1))
    )
    (return-from calc-heuristic-8puzzle heuristic)
)

;;;*************************************************************************
;;;
;;; returns man-dist Heuristic to 8 puzzle problem
;;; takes node and goal state as parameter
;;; creates matrix based of position and calculates Heuristic
;;;
;;;*************************************************************************
(defun calc-man-dist-heuristic (curr-state goal-state)
    (setq heuristic 0)
    (setq i 0)
    (setq pair1 '(0 0))
    (setq pair2 '(0 0))
    (loop for element in curr-state do
        (COND
            ( (not (equal element (nth i goal-state)))
              (setq temp (nth i curr-state))
              (setq goal-index (index temp goal-state))
              (setq pair1 (get-pair i))
              (setq pair2 (get-pair goal-index))
              (setq i (+ i 1))
            )
        )
        (setq x1 (car pair1))
        (setq y1 (car (cdr pair1)))
        (setq x2 (car pair2))
        (setq y2 (car (cdr pair2)))
        (setq z1 (abs (- x2 x1)))
        (setq z2 (abs (- y2 y1)))
        (setq z3 (+ z1 z2))
        (setq heuristic (+ z3 heuristic))
        (setq pair1 '(0 0))
        (setq pair2 '(0 0))
    )
    (return-from calc-man-dist-heuristic heuristic)
)

;;;*************************************************************************
;;;
;;; returns corresponding (x, y) pair for index
;;; will be used to calculate manhattan-heuristic, by using heuristic formula
;;; takes index as parameter
;;;
;;;*************************************************************************
(defun get-pair (index)
    (COND
        ( (equal index 0)
              (return-from get-pair '(1 1))
        )
        ( (equal index 1)
              (return-from get-pair '(1 2))
        )
        ( (equal index 2)
              (return-from get-pair '(1 3))
        )
        ( (equal index 3)
              (return-from get-pair '(2 1))
        )
        ( (equal index 4)
              (return-from get-pair '(2 2))
        )
        ( (equal index 5)
              (return-from get-pair '(2 3))
        )
        ( (equal index 6)
              (return-from get-pair '(3 1))
        )
        ( (equal index 7)
              (return-from get-pair '(3 2))
        )
        ( (equal index 8)
              (return-from get-pair '(3 3))
        )
    )
)

;;;*************************************************************************
;;;
;;; custom heuristic based off two other heuristics
;;; returns more informed heuristic between man-dist and default heuristic
;;; takes current state and goal state as parameter
;;;
;;;*************************************************************************
(defun calc-custom-heuristic (curr-state goal-state)
    (return-from calc-custom-heuristic (max (calc-heuristic-8puzzle curr-state goal-state) (calc-man-dist-heuristic curr-state goal-state)))
)
