
(ql:quickload '(:array-operations :cl-clojure))

(defpackage :lonpos.lonpos2d
  (:use :clj))
(in-package :lonpos.lonpos2d)

(enable-clojure-readtable)

(defsubst vtranspose (piece)
  (make-array (reverse (array-dimensions piece))
              :initial-contents
			  (apply #'mapv #'vector
					 (coerce
					  (aops:split piece 1) 'list))))

(defsubst mirror-x (piece)
  (make-array (array-dimensions piece)
              :initial-contents
			  (mapv #'reverse (aops:split piece 1))))

(defsubst mirror-y (array)
  (make-array (array-dimensions array)
              :initial-contents
			  (reverse (aops:split array 1))))

(defsubst shape (x) (array-dimensions x))

(defparameter +place-holder+ #\_
  "Place holder, can be character or a number, used to indicate this
place (slot) is not placed.")

(-> has-pieces ((or fixnum character)) boolean)
(defun has-piece? (x)
  "Return true if value X represent it is occupied by a piece."
  (not (or (eq x 0)
		   (eq x +place-holder+))))

(defun all (piece)
  (flet ((four-states (piece)
           (let [y (mirror-y piece)]
             [piece
              (mirror-x piece)
              y
              (mirror-x y)
              ])))
    (unique
	 (append
      (four-states piece)
      (four-states (vtranspose piece)))
	 :test #'equalp)))

(defvar +masks+
  '(#2A((1 1)
		(1 0))

	#2A((1 1)
		(1 1))

	#2A((1)
		(1)
		(1)
		(1))

	#2A((1 1)
		(1 0)
		(1 0))

	#2A((1 1)
		(1 1)
		(1 0))

	#2A((1 0)
		(1 1)
		(1 0)
		(1 0))

	#2A((1 1)
		(1 0)
		(1 0)
		(1 0))

	#2A((0 1)
		(1 1)
		(1 0)
		(1 0))

	#2A((1 1 1)
		(1 0 0)
		(1 0 0))

	#2A((1 1)
		(1 0)
		(1 1))

	#2A((0 1 0)
		(1 1 1)
		(0 1 0))

	#2A((0 1 1)
		(1 1 0)
		(1 0 0))))

;; (count (filter #(= % 1) (flatten +pieces+))) = 55

(defconstructor Point
  (i fixnum)
  (j fixnum))

(defstruct/persist Piece
  mask
  i-offset)

(-> compute-i-offset (array) fixnum)
(defun compute-i-offset (piece)
  "Origin is the left top most non-zero point in mask."
  (let [[i _] (shape piece)]
	(loop for x from 0 below i
		  for val = (aref piece x 0)
		  when (has-piece? val)
			return x
		  finally (error "Should not be here ~a" piece))))

(-> init-piece (array) Piece)
(defun init-piece (mask)
  (make-piece
   :mask mask
   :i-offset (compute-i-offset mask)))

;; (defvar +pieces+ (mapcar #'init-piece +masks+))
;; (compute-i-offset (nth 7 +pieces+))

(-> init-board () array)
(defun init-board ()
  "a triangle board, top row 9 slots, second row 8, third 7, etc,
   and last row, row 1 has 1 slot."
  (make-array 9 :initial-contents
			  (loop for i from 9 downto 1
					collect (make-array
							 i :initial-element +place-holder+))))


(-> next-empty-point (array fixnum fixnum) Point)
(defun next-empty-point (board i j)
  (if (= i (- 9 j))
	  (next-empty-point board 0 (1+ j))
	  (if
	   ;; Skip if have piece
	   (has-piece? (aref (aref board i) j))
	   (next-empty-point board (1+ i) j)
	   ;; Otherwise, this point is fine
	   (Point i j))))

;; (defun next-point (board points-tree)
;;   "Find the next point with consideration of STATE. "
;;   (let [;; (Placed-piece _ points-tree)
;; 		[j is] (car points-tree)
;; 		i (car (reverse is))]
;; 	(next-empty-point board (1+ i) j)))

(-> next-point (array Point) Point)
(defun next-point (board point)
  "Find the next point with consideration of STATE. "
  (let [(Point i j) point]
	(next-empty-point board i j)
	;; (cond (= i (- 8 j))
	;; 	  (next-empty-point board 0 (1+ j))
	;; 	  (> i (- 8 j))
	;; 	  (progn
	;; 		;; Just warn, and handle this in next-state and backtrack
	;; 		(warn (str "Next-point: Placied-piece is out of board, "
	;; 				   points-tree))
	;; 		(Point (1+ i) j))
	;; 	  :otherwise (Point (1+ i) j))
	))

(defconstructor Placed-piece
  (piece piece)
  (point point)
  (points list))

(-> make-Placed-piece (Piece Point) Placed-piece)
(defun make-Placed-piece (piece point)
  (let [(Piece mask i-offset) piece]
	(when *verbose*
	  (println "PP: " piece ", " i-offset))
	(Placed-piece piece
				  point
				  ;; Using points-list now, it's easier to perform
				  ;; board piece collision check now, lot harder if
				  ;; its tree
				  (points-list mask
							   (copy-point point :i
										   (- (point-i point)
											  i-offset))))))

(defun point-x-y-in-board? (x y)
  (and (>= y 0) (< y 9)
	   (>= x 0) (< x (- 9 y))))

(defun point-in-board? (point)
  (point-x-y-in-board?
   (point-i point) (point-j point)))

(defun points-in-board? (points)
  "Return non-nil if points from `points-list' are valid points in
board."
  (every #'point-in-board? points))

(defun points-list (vec2d point)
  "Generate the points list for PIECE at POINT."
  (let [[i j] (shape vec2d)
        (Point x y) point]
    (loop :for a :below i
		  :for x+a = (+ x a)
		  ;; :do (println vec2d a subvec)
		  :append
		  (loop :for b :below j
				:for val = (aref vec2d a b)
				;; :do (println val)
				:when (has-piece? val)
				  :collect (Point x+a (+ y b))))))

(defun points-tree (vec2d point)
  "Generate the points value tree for frames function to compute frame.
This is the J.Is version."
  (let [[i j] (shape vec2d)
        (Point x y) point]
    (loop :for b below j
          :for y+b = (+ y b)
		  :collect
		  (list y+b
				(loop :for a below i
					  :for val = (aref vec2d a b)
					  ;; :do (println val)
					  :when (has-piece? val)
						:collect (+ x a))))))

;; (POINTS-TREE #2A((1 1) (1 0)) [0 2])

(defun points-frame-bottom-right (points-tree)
  "Points-tree are generated from `points-tree'."
  ;; (filter #'point-in-board?)
  (append
   ;; bottom row
   (mapcar (lambda (j.is)
			 (let [[j is] j.is]
			   (Point (1+ (car (last is))) j)))
		   points-tree)
   ;; right most column
   (let [[j is] (car (last points-tree))
		 j+1 (1+ j)]
	 (mapcar #(Point % j+1) is))))

;; (let [v (piece-mask (nth 0 +pieces+))]
;;   (points-frame-bottom-right (points-tree v [0 0])))

(defun place-at! (vec2d points val)
  "Place VAL at POINTS in VEC2D.
  Use `points-list' to generate POINTS.
  val = 0, effectively remove piece.
  val /= 0, effectively placing piece."
  (dolist (point points vec2d)
	(let [(Point i j) point]
	  ;; (dolist (j js))
	  (if (point-x-y-in-board? i j)
		  (setf (aref (aref vec2d i) j) val)
		  (warn (str "point not in board " point))))))

(defun place-at (board points val)
  "Functional version of `place-at!'."
  (let [acc (mapv #'copy-seq board)]
	(place-at! acc points val)
	acc))

(defun place-placed-piece-at (board placed-piece val)
  "Functional version of `place-at!'."
  (let [acc (mapv #'copy-seq board)
		points (placed-piece-points placed-piece)]
	(place-at! acc points val)
	acc))

;; (lonpos-board-fmt (place-at (init-board) (points-tree (nth 0 +pieces+) [0 1])))

(defun lonpos-board-fmt (board)
  (flet ((vec->string (v)
		   (apply #'str (map 'list #'str v))))
	(format cl:nil "~%~{~a~%~}"
			(loop for row across board
				  collect (vec->string row)))))

(defun lonpos->board (state)
  (lonpos-board-fmt
   (place-at (state-lonpos state)
			 (state-next-moves state)
			 #\x)))

(defconstructor State
  (lonpos array)
  (placed-pieces list)
  (pieces list)							; all rotations of pieces
  (next-moves list)						; available points to place in
  )

(defmethod print-object ((obj State) stream)
  (with-slots (placed-pieces pieces next-moves)
	  obj
	(print-unreadable-object (obj stream :type t)
	  (format stream "~a ~a~%" :placed-pieces placed-pieces)
	  (format stream "~a ~a~%" :pieces pieces)
	  (format stream "~a ~a~%" :next-moves next-moves))))

(defun init-State ()
  (let [lonpos (init-board)]
	(State lonpos
		   '()
		   (mapcar (lambda (x) (mapcar #'init-piece (all x)))
				   +masks+)
		   [(Point 0 0)])))

(defsubst state-solved? (s)
  "State is solved if no more pieces to fill and no more next moves."
  (and (typep s 'state)
	   (= (length (state-placed-pieces s)) 10)))

(defsubst state-stuck? (s)
  "State is stuck (require backtrack) is no more next moves or pieces to
place."
  (or (null (state-next-moves s))
	  (null (state-pieces s))))

(defun valid-placed-piece? (placed-pieces pp)
  "PP (placed-piece) is valid, if it doesn't overlap any other pieces
and all points are in board."
  (let [(Placed-piece _ point points) pp]
	;; (declare (ignore _frame))
	(and (point-in-board? point)
		 (points-in-board? points)
		 (not
		  (some (lambda (p)
				  (some #(find % (placed-piece-points p)
						  :test #'equalp)
						points))
				placed-pieces)))))


(defparameter *verbose*
  ;; t
  cl:nil
  )

(defun shuffle-piece-group (pieces)
  "Shuffle the first piece group to the back of the piece group."
  (let [(cons (cons _ mirrors) other-pieces) pieces]
	(if mirrors ; keep mirrors if non-nil
		(cons mirrors other-pieces)
		other-pieces))
  ;; Shuffle piece group so all the
  ;; requires pieces will be placed into
  ;; the board, but trying at different
  ;; time.
  ;; (let [(cons piece-gorup others) pieces]
  ;; 	(append others
  ;; 			(list piece-group)))
  )

(defun next-state (all-states)
  (let [(cons state states) all-states]
	(cond (state-stuck? state)
		  (progn ;; (println "State stuck " state)
			(next-state states))				  ; Backtrack
		  (state-solved? state) [:solution state] ; Solution
		  t
		  ;; Need a method to iterate through the permutation of
		  ;; pieces and points, to find the solution.
		  (let [(State lonpos placed-pieces all-pieces next-moves) state
				(cons (cons piece _) other-pieces) all-pieces
				point (car next-moves)
				placed-piece (make-Placed-piece piece point)
				;; remove this piece and point
				states (cons (copy-state
							  state
							  :pieces
							  (shuffle-piece-group all-pieces))
							 states)]
			(when *verbose*
			  (println "trying " placed-piece))
			;; TODO: I should use a predicate to find first valid
			;; placed-piece?
			(if (valid-placed-piece? placed-pieces placed-piece)
				(cons (let [board
							(place-placed-piece-at
							  lonpos
							  placed-piece
							  (1+ (length placed-pieces)))]
						(State board
							   (cons placed-piece placed-pieces)
							   other-pieces
							   ;; Left first algorithm
							   (list (next-point ;; points-frame-bottom-right
									  board
									  point))))
					  states)
				(next-state states))))))

(defun make-counter (count)
  (if (and (integerp count) (> count 0))
	  (lambda ()
		(prog1 (= count 0)
		  (decf count)))
	  (lambda ()
		cl:nil)))

(defun solve (states &key (stepn cl:nil) (stuckn cl:nil))
  (let [stepn-counter (make-counter stepn)
		stuckn-counter (make-counter stuckn)]
	(nlet recur [states states]
	  (if (funcall stepn-counter)
		  states
		  (trivia:ematch (next-state states)
			([:solution state] state)
			((and next-states (cons state _))
			 (if (state-stuck? state)
				 (if (funcall stuckn-counter)
					 next-states
					 (recur next-states))
				 (recur next-states)))
			('()						; returned to initial-state
			  :no-solution-found))))))

;; (lonpos-board-fmt (init-board))
;; (lonpos->board (car (solve (list (init-State)) :stepn 4)))
;; (lonpos->board (car (solve (list (init-State)) :stepn 16)))
;; (lonpos->board (car (solve (list (init-State)) :stuckn 400)))
;; (car (next-state (list (init-State))))

;; (solve (list (init-State)))


;; 11/19/22 It's mostly, done.
;; It had problem find the solution of a blank board.
;; Which is why the next step is to test it on actual puzzles
;; For example from the book.

;; Extension 1.
;;
;; Parse a board-fmt board as state board, i.e. given the string
;; representation of the board, break it down into (State
;; placed-pieces next-move etc)

;; Extension 2.
;;
;; Write a macro version of solve, and combine the parser from
;; Extension 1 to run macro expansion as a step by step solver!



