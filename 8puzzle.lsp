;(50 points) Implement the A* search for searching trees (in Lisp). Do not use Russell’s code or other
;code from the web. Implement a counter that counts the number of nodes expanded and prints this
;number at the end of the search. Use your code to solve the 8-puzzle problem with the heuristic
;being the number of misplaced tiles and start state ((E, 1, 3),(4, 2, 5),(7, 8, 6)). The goal state is:
;((1, 2, 3),(4, 5, 6),(7, 8, E)). Print the number of nodes expanded. You only need to show the states
;generated during the search process. Your code should detect infeasible puzzles. For an infeasible
;puzzle run your code and submit the results as well.

;state of each node in the state space
(defstruct state
	config ;2d array which represents tile configuration
	id     ;parent id and id fields to track the predecessor state nodes
	parent
	g      ;distance traversed from start state to some state
	h      ;distance from some state to the goal state
	f      ;A* Search (f=g+h)
)

(defvar *counter* 0) ;keeps track of expanded state nodes
(defvar *id* 0)  ;unique id for each state node
(setq *path_sequence* '()) ;contains sequence of state nodes which lead to the solution
(setf *heap* (make-array 0 :fill-pointer t :adjustable t))   ;keeps track of state node with minimum heuristic
(defparameter *visited* (make-hash-table))   ;keeps track of visited state nodes
(defparameter *idState_Map* (make-hash-table)) ;for tracking the predecessor nodes thorugh id member in the state node

;;initializing goal state
(setf *goal_state* (make-array '(3 3) 
   :initial-contents '((1 2 3 ) (4 5 6) (7 8 0)))
)

;Entry point for 8 puzzle problem
(defun 8_puzzle(tile)
	;converting a list to 2d array
	(setf tile_array (make-array '(3 3)))
	(loop for i from 0 to 2 do
		(loop for j from 0 to 2 do	
			(setf (aref tile_array i j) (nth j (nth i tile)))
		)
	)
	;check for infeasible puzzles
	(if (= 1 (check_invalid tile_array))
		(progn
			(format t "Infeasible Puzzle, please try another input.......~%")
			(return-from 8_puzzle)
		)
	)
	(initialize tile_array)
	(format t "Expanded nodes~%")
	(solve_puzzle)
)

;checks whether a tile configuration is feasible or infeasible by checking the number of inversions
(defun check_invalid(tile)
	(defvar ind)
	(setf ind 0)
	(defvar inv_count 0)
	(setf inv_count 0)
	(setf temp_tile (make-array '(9)))
	(loop for i from 0 to 2 do
		(loop for j from 0 to 2 do
			(setf (aref temp_tile ind)(aref tile_array i j))
			(setf ind (+ ind 1))
		)
	)
	(loop for i from 0 to 7 do
		(loop for j from (+ i 1) to 8 do	
			(if (and (and (not (= 0 (aref temp_tile i))) (not(= 0 (aref temp_tile j)))) (> (aref temp_tile i) (aref temp_tile j)))
				(setf inv_count (+ 1 inv_count))
			)
		)
	)
	(if (= 0 (mod inv_count 2))
		0
		1
	)
)

;Initialize start state
(defun initialize(tile)
	(setf *id* (+ *id* 1))
	(vector-push-extend (make-state 
            :config tile
			:id *id*
			:parent 0
			:g 0
			:h (calculate_h tile)
    		:f (calculate_h tile)
	      ) *heap*
    )
)

;Prints each tile configuration
(defun print_tile(tile_node)
	(loop for i from 0 to 2 do
		(loop for j from 0 to 2 do	
			(format t "~d| " (aref (state-config tile_node) i j))
		)
		(format t "~%")
	)
	(format t "~%~%")
)

;prints sequence of tile configurations from start state to goal state
(defun print_sequence()
	(setf k (- (length *path_sequence*) 1))
	(loop for i from 0 to (- (length *path_sequence*) 1) do
		(print_tile (nth i *path_sequence*))
	)
	(format t "Optimal number of tiles moved to reach goal state = ~d ~%" k)
)

;;Solves the puzzle by expanding a state based on lowest f and generating its successors
(defun solve_puzzle()
	;increment counter when a state node is expanded
;	(setf *counter* (+ *counter* 1))
	(setf curr_tile (vector-pop *heap*))
	(setf key (compute_key (state-config curr_tile)))
	;mark the state node as visited
	(setf (gethash key *visited*) curr_tile)
	;if goal state is reached print the number of state nodes expanded and sequence of tile moves from start state to goal state
	(if (equalp (state-config curr_tile) *goal_state*)	
		(progn
			(format t "Sequence of tile moves ~%")
			(calculate_path curr_tile)
			(print_sequence)
			(format t "Total Expanded nodes ~d~%" *counter*)
			(return-from solve_puzzle)
		)
	        (progn
			;increment counter when a state node is expanded
		        (setf *counter* (+ *counter* 1))
			;print the expanded state
			(print_tile curr_tile)
			;generate different tile states for all 4 different moves of the the empty state
			(setf (gethash (state-id curr_tile) *idState_Map*) curr_tile)
			(defvar x)
			(defvar y)
			(loop for i from 0 to 2 do
				(loop for j from 0 to 2 do
					(if (= 0 (aref (state-config curr_tile) i j))
						(progn
							(setf x i)
							(setf y j)
						)
					)
				)
			)
			(defvar tempx)  ;x coordinate for empty tile
			(defvar tempy)  ;y coordinate for empty tile
			(defvar tempconfig)  ;successor configuration after moving the empty tile	
			(loop for xx from 0 to 3 do		
				(setf tempx -1)
				(setf tempy -1)
				(setf tempconfig (make-array '(3 3)))
				(loop for i from 0 to 2 do
					(loop for j from 0 to 2 do	
						(setf (aref tempconfig i j) (aref (state-config curr_tile) i j))
					)
				)
				;checks all 4 possible moves
				(cond ((= xx 0)
						(setf tempx (- x 1))
						(setf tempy y)
					)
					((= xx 1)
						(setf tempx (+ x 1))
						(setf tempy y)
					)
					((= xx 2)
						(setf tempx x)
						(setf tempy (- y 1))
					)
					(t
						(setf tempx x)
						(setf tempy (+ y 1))
					)
				)
				;if the state is univisited then create a new state node and push into heap
				(if (and (and (and (>= tempx 0) (>= tempy 0)) (<= tempx 2)) (<= tempy 2))
					(progn
						(rotatef (aref tempconfig x y) (aref tempconfig tempx tempy))
						(setf key (compute_key tempconfig))
						
						(if (not (gethash key *visited*))
							(progn
								(setf *id* (+ *id* 1))
								(defvar g)
								(defvar h)
								(setf g (+ (state-g curr_tile) 0.1))
								(setf h (calculate_h tempconfig))
								(vector-push-extend (make-state 
												:config tempconfig
												:id *id*
												:parent (state-id curr_tile)
												:g g
												:h h
												:f (+ h g)
											) *heap*
								)
							)
						)			
					)
				)
			)
		)
	)
	;sort heap after expanding each node and generating successors
	(sort_heap 0 (- (length *heap*) 1))
	;Process recursively to pick the state node with lowest f
	(solve_puzzle)
)

;;Quick sort partition method
(defun partition(low high)
	(setf pivot (state-f (aref *heap* high)))
	(setf i (- low 1))
	(loop for j from low to (- high 1) do
		(if (>= (state-f (aref *heap* j)) pivot)
			(progn
				(setf i (+ i 1))
				(rotatef (aref *heap* i) (aref *heap* j))
			)
		)
	)
	(rotatef (aref *heap* (+ i 1)) (aref *heap* high))
	(+ i 1)
)

;Quick Sort
(defun sort_heap(low high)
	(if (< low high)
		(progn
			(setf p (partition low high))
			(sort_heap low (- p 1))
			(sort_heap (+ p 1) high)
		)
	)
)

;Compute key for hashmap when inserting into visited variable
(defun compute_key(tile)
	(setf ind 1)
	(setf sum 0)
	(loop for i from 0 to 2 do
		(loop for j from 0 to 2 do
			(setf prod 1)
			(loop for k from 0 to (- ind 1) do
				(setf prod (* prod 9))
			)
			(setf prod (* prod (aref tile i j)))
			(setf sum (+ sum prod))
			(setf ind (+ ind 1))
		)
	)
	sum
)

;calculate h (number of misplaced tiles)
(defun calculate_h(tile)
	(defvar h)
	(setf h 0)
	(loop for i from 0 to 2 do
		(loop for j from 0 to 2 do
			(if (not (= (aref *goal_state* i j) (aref tile i j)))
				(setf h (+ h 1))
			)
		)
	)
	h
)

;Populate the path sequence with all the nodes from start state to goal state recursively
(defun calculate_path(goal_node)
	(push goal_node *path_sequence*)
	(setf pId (state-parent goal_node))
	(if (not (= pId 0))
		(progn
			(calculate_path (gethash pId *idState_Map*))
		)
	)
)

;(8_puzzle '((0 1 3) (4 2 5) (7 8 6)))
;(8_puzzle '((3 0 1) (2 4 5) (7 8 6)))
;(8_puzzle '((7 0 3) (2 4 5) (1 8 6)))
;(8_puzzle '((8 1 2) (0 4 3) (7 6 5)))
;(8_puzzle '((0 1 3)(4 2 5)(7 8 6)))
;(8_puzzle '((1 2 3)(4 7 6)(8 5 0)))

;(8_puzzle '((1 0 3) (4 5 2) (6 8 7)))
;(8_puzzle '((5 7 3) (8 4 2) (0 1 6)))
