;;; STATE SPACE SEARCH A* Search for 8-Puzzle
;;; WHAT'S PROVIDED
;;;
;;; Accompanying this file are two other files: "utilities.lisp" and
;;; "queue.lisp".  utilities.lisp must be loaded first, then queue.lisp.
;;;  queue.lisp is what you need to take a look at: it's an implementation
;;; of three kinds of queues: LIFO stacks, FIFO queues, and priority queues.


;;; THE 8-PUZZLE REPRESENTATION
;;; 
;;; 8-puzzles are simple-vectors of integers with 10 slots.
;;; Slots 0...8 are the positions in the puzzle in row-major
;;; order, filled
;;; with numbers representing the tile that's in that slot
;;; (9 is the empty space).  slot 9 additionally says where
;;; the empty space is located.  Of course slot 9 is unnecessary,
;;; but it makes the puzzle much more efficient than wandering through
;;; the array each time looking for a 9.  So for example,
;;; the following puzzle:
;;;
;;; 2 7 4
;;; 9 8 3
;;; 1 5 6
;;;
;;; ...is stored in a simple-vector with the following values:
;;;
;;; #(2 7 4 9 8 3 1 5 6 3)
;;;
;;; ...the last item (3) says that the empty space, represented
;;; as a 9, is located in slot 3 of the array.

;;; THE NUM-OUT AND MANHATTAN HEURISTICS
;;;
;;; The NUM-OUT heuristic is simply the total number of tiles out of place 
;;; (not including the blank space).
;;;
;;; The MANHATTAN heuristic is the sum, over each tile (not including the
;;; blank), of the manhattan distance of that tile from where it's supposed
;;; to be.  Manhattan distance between two points <x,y> and <x2,y2> is equal
;;; to |x-x2| + |y-y2|, that is, it's the difference along the x dimension
;;; plus the difference along the y dimension.
;;;
;;; Example:
;;;
;;; In the following puzzle:
;;;
;;; 1 8 3
;;; 9 6 5     (9 is the blank -- ignore it)
;;; 7 4 2
;;;
;;; NUM-OUT = (#8 out of place) + (#6 out of place) + (#5 out of place) + 
;;;           (#4 out of place) + (#2 out of place) = 5
;;;
;;; MANHATTAN:   TILE     X OUT    Y OUT
;;;              #1       0        0
;;;              #2       1        2
;;;              #3       0        0
;;;              #4       1        1
;;;              #5       1        0
;;;              #6       1        0
;;;              #7       0        0
;;;              #8       0        2
;;;      Total            4   +    5     =  9


(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
            (list (position 9 initial-puzzle-situation))) nil))

(defun create-random-state (num-moves)
    "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
    (let ((puzzle #(1 2 3 4 5 6 7 8 9 8)))
        (dotimes (x num-moves)
            (let ((moves (elt *valid-moves* (empty-slot puzzle))))
                (setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
        (build-state puzzle nil)))

(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 9))

(defun swap (pos1 pos2 puzzle)
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves* 
    #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
        ,@body))

(defun make-move (move puzzle)
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (when (find move moves) (swap move (empty-slot puzzle) puzzle))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setf ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 9) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))



(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
    "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
    function.  Uses a priority queue and a history list of previously-visited states.
    Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
    out the number of iterations required to discover the goal state.  Returns the 
    discovered goal state, else returns the symbol 'FAILED if the entire search 
    space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
    exceeded.  If maximum-iterations is set to nil, then there is no maximum number
    of iterations."

	(let ((queue (make-empty-queue)) (state initial-state) children (history (make-hash-table :test #'equalp :size 100000 :rehash-size 100000)) puzzle (iterations 0) found)
			(funcall enqueueing-function state queue) ;add initial state to empty queue
			(setf (gethash (puzzle-from-state state) history) nil) ;add initial puzzle to history
			
			(loop 
		
				do (incf iterations) ;increments iterations
				until (empty-queue? queue) ;if queue is empty stop
				until (and (not (equalp maximum-iterations nil)) (> iterations maximum-iterations)) ;if over max-iterations, stop		
				do (setf state (remove-front queue)) ;state = dequeue
				until (setf found (funcall goal-test state)) ;if goal is found, stop
				do (progn 
						(setf puzzle (puzzle-from-state state)) ;else get puzzle from state
						(setf children nil) ;reset children for new iteration
						(foreach-valid-move (move puzzle) (push (make-move move puzzle) children)) ;create all valid children
						(dotimes (i (length children)) ;for each child
							(let ((this (elt children i))) ;current child
								(if (not (nth-value 1 (gethash (elt children i) history))) ;is child in history? If not...
									(progn 
										(funcall enqueueing-function (build-state this state) queue) ;add child state to queue
										(setf (gethash (elt children i) history) puzzle)))))));add child to history
			(if (not found) 'failed
				(progn 
					(format t "~%Iterations: ~a~%" iterations)
					state))));if found, returns #iterations and state
						
					
					
			
				
;;; goal state for 8-puzzle
(defparameter *goal* (puzzle-from-state (make-initial-state '(1 2 3 4 5 6 7 8 9))))

;;;function that returns the f(state)
(defun manhattan-f (state)
	(let ((h (manhattan (puzzle-from-state state))))
		(+ (1- (length state)) h)));returns f(state) = g(state)+ h(state)
	
	
(defun goal-p (state)
    "Returns T if state is a goal state, else NIL.  Our goal test."
	(equalp (puzzle-from-state state) *goal*))

(defun dfs-enqueuer (state queue)
    "Enqueues in depth-first order"
		(enqueue-at-front queue state))
		
(defun bfs-g (state)
	(1- (length state)));bfs implementation of djikstra's, finds the f(state) = g(state), where all edges = 1

(defun bfs-enqueuer (state queue)
    "Enqueues in breadth-first order"
		(enqueue-by-priority queue #'bfs-g state))  


;; function that returns the manhattan distance of 8-puzzle	
(defun manhattan (puzzle)
	(let (x y x2 y2 (h 0))
		(loop for i from 1 to 8 ;iterates through each number on board
			do	(setf x (mod (position i puzzle) 3)) ;finds x position of puzzle
			do	(setf y (floor (position i puzzle) 3)) ;finds y position of puzzle
			do	(setf x2 (mod (position i *goal*) 3)) ;finds x position of goal
			do  (setf y2 (floor (position i *goal*) 3)) ; finds y position of goal
			do	(setf h (+ h (abs (+ (- x x2) (- y y2)))))) ;calculates heuristic h(state) using manhattan distance
			h)) ;returns h(puzzle)

(defun manhattan-enqueuer (state queue)
    "Enqueues by manhattan distance"

        ;; IMPLEMENT ME
	(enqueue-by-priority queue #'manhattan-f state))
				
(defun numout-f (state)
	(let ((h (numout (puzzle-from-state state))))
		(+ (1- (length state)) h)))	;returns f(state) using num-out heuristic + g(state)

;;function that determines the h(puzzle) using num-out heuristic
(defun numout (puzzle)
	(let ((out 0))
	(loop for i from 1 to 8 ;for each number on board, excluding 9
		do (if (not (= (position i puzzle) (position i *goal*))) ;if tile not in goal postion
				(incf out))) ;increment out
			out)) ;return out
			
			
(defun num-out-enqueuer (state queue)
    "Enqueues by number of tiles out of place"
	(enqueue-by-priority queue #'numout-f state))
				
;; implements greedy-best-first search using Manhattan as the heuristic			
(defun greedy-f (state)
	(manhattan (puzzle-from-state state))) 
	
;; enqueues by greedy-best-first search using manhattan heuristic	
(defun greedy-best-enqueuer (state queue)
	(enqueue-by-priority queue #'greedy-f state))
	

#|

(print-solution (general-search s #'goal-p #'manhattan-enqueuer 20000))

;;; The five test examples.

;;; Solves in 4 moves: Test1
(setf s (make-initial-state '(
9 2 3
1 4 6
7 5 8)))

;;; Solves in 8 moves: Test2
(setf s (make-initial-state '(
2 4 3
1 5 6
9 7 8)))

;;; Solves in 16 moves: Test3
(setf s (make-initial-state '(
2 3 9
5 4 8
1 6 7)))

;;; Solves in 24 moves: Test4
(setf s (make-initial-state '(
1 8 9
3 2 4
6 5 7)))


My Results: (Iterations to find optimal route)
													
		Manhattan	NUM-OUT	BFS-ENQUEUER	DFS-ENQUEUER 	GREEDY_BEST (Not required)
Test 1: 	5			5		18				failed		5
Test 2:	   18		   13		232				failed		13 (but took 10 moves)
Test 3:   178 		  381		10275			failed		1892 (but took 64 moves)
Test 4:  11293		13777		failed			failed      846(but took 98 moves)




