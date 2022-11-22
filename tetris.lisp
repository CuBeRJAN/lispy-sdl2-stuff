(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :sdl2-ttf)
(ql:quickload :bt-semaphore)


(setf *random-state* (make-random-state t))

(defconstant *screen-width* 960)
(defconstant *screen-height* 960)
(defconstant sq-size 38)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "Common Lisp SDL2 Tetris"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,renderer (sdl2:create-renderer window -1 '(:ACCELERATED :PRESENTVSYNC))))
         ,@body))))

(defmacro draw-rect (renderer x y width height)
  `(sdl2:with-rects ((fill-rect ,x ,y ,width ,height))
     (sdl2:render-draw-rect ,renderer fill-rect)))


(defconstant *w* 10)
(defconstant *h* 23)
(defconstant piece-dim 4)
(defvar grid (make-array (list *h* *w*) :initial-element "."))
(defvar piece)
(defvar piece-pos)
(defvar not-shuffeled)
(defvar piece-index)
(defvar exit-game nil)
(defvar next-piece)
(defstruct pos x y)

(defvar *pieces*
  '(
    #2A(("." "." "O" ".")
	("." "." "O" ".")
	("." "." "O" ".")
	("." "." "O" "."))
    
    #2A(("." "O" "." ".")
	("." "O" "." ".")
	("." "O" "O" ".")
	("." "." "." "."))

    #2A(("." "." "O" ".")
	("." "." "O" ".")
	("." "O" "O" ".")
	("." "." "." "."))

    #2A(("." "." "." ".")
	("." "O" "." ".")
	("O" "O" "O" ".")
	("." "." "." "."))
    
    #2A(("." "." "." ".")
	("." "O" "O" ".")
	("." "O" "O" ".")
	("." "." "." "."))
    
    #2A(("." "." "O" ".")
	("." "O" "O" ".")
	("." "O" "." ".")
	("." "." "." "."))
    
    #2A(("." "O" "." ".")
	("." "O" "O" ".")
	("." "." "O" ".")
	("." "." "." "."))
    ))

(defun random-from-list (list)
  (let ((len (length list)))
    (nth (random len) list)))

(defun print-grid (grid)
  (let ((width (array-dimension grid 1))
	(height (array-dimension grid 0)))
    (loop for i from 0 below height do
      (progn
	(loop for j from 0 below width do
	  (format t "~a" (aref grid i j)))
	(format t "~%")))))

(defun render-piece (grid)
  (let ((pw piece-dim)
	(ph piece-dim))
    (loop for i from 0 below ph do
      (loop for j from 0 below pw do
	(when (string= (aref piece i j) "O")
	  (setf (aref grid
		      (+ i (pos-y piece-pos))
		      (+ j (pos-x piece-pos)))
		(aref piece i j)))))))

(defun store-piece ()
  (render-piece grid))

(defun copy-array (array &key
			   (element-type (array-element-type array))
			   (fill-pointer (and (array-has-fill-pointer-p array)
					      (fill-pointer array)))
			   (adjustable (adjustable-array-p array)))
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))

(defun print-game ()
  (let ((render (copy-array grid)))
    (render-piece render)
    (print-grid render)))

(defun shuffle-list (list)
  (loop for i from (length list) downto 2
        do (rotatef (nth (random i) list)
                    (nth (1- i) list)))
  list)

(defun predict-piece ()
  (if (> piece-index 5)
      (if not-shuffeled
	  (progn
	    (shuffle-list *pieces*)
	    (setf not-shuffeled nil)
	    (car *pieces*))
	  (car *pieces*))
      (nth (+ piece-index 1) *pieces*)))

(defun random-piece ()
  (incf piece-index)
  (when (> piece-index 6)
    (setq piece-index 0))
  (setf not-shuffeled t)
  (setf piece (nth piece-index *pieces*)))

(defun reset-pos ()
  (setf (pos-x piece-pos) (- (/ *w* 2) (/ piece-dim 2)))
  (setf (pos-y piece-pos) 0))

(defun init-game ()
  (setf piece (random-from-list *pieces*))
  (setf piece-pos (make-pos :x (- (/ *w* 2) (/ piece-dim 2)) :y 0))
  (shuffle-list *pieces*)
  (setf not-shuffeled t)
  (setq piece-index 0)
  (setf piece (car *pieces*)))

(defun move-piece-skip ()
  (loop while (not (has-landed)) do
    (incf (pos-y piece-pos)))
  (store-piece)
  (random-piece)
  (reset-pos))

(defun move-piece ()
  (if (not (has-landed))
      (incf (pos-y piece-pos))
      (progn
	(store-piece)
	(random-piece)
	(reset-pos))))

(defun transpose-matrix (matrix)
  (let ((new-matrix (make-array (array-dimensions matrix)))
	(height (array-dimension matrix 0))
	(width  (array-dimension matrix 1)))
    (loop for i from 0 below height do
      (loop for j from 0 below width do
	(setf (aref new-matrix i j) (aref matrix j i))))
    new-matrix))

(defun reverse-rows (matrix)
  (let* ((height (array-dimension matrix 0))
	 (width  (array-dimension matrix 1))
	 (tmp nil)
	 (start 0)
	 (end (- width 1)))
    (loop for i from 0 below height do
      (progn
	(setq start 0)
	(setq end (- width 1))
	(loop while (< start end) do
	  (setf tmp (aref matrix i start))
	  (setf (aref matrix i start) (aref matrix i end))
	  (setf (aref matrix i end) tmp)
	  (incf start)
	  (decf end))))))

(defun is-valid-pos (y x)
  (when (and
	 (>= x 0)
	 (< x *w*)
	 (>= y 0)
	 (< y *h*))
    (string= (aref grid y x)
	     ".")))

(defun has-landed ()
  (check-collision :down t))

(defun clear-row (n)
  (loop for i from n above 0 do
    (loop for j from 0 below (array-dimension grid 1) do
      (setf (aref grid i j) (aref grid (- i 1) j)))))

(defun check-rows ()
  (let ((do-clear nil))
    (loop for i from 0 below (array-dimension grid 0) do
      (loop for j from 0 below (array-dimension grid 1) do
	(progn
	  (when (string/= (aref grid i j) "O")
	    (return))
	  (when (= j (- (array-dimension grid 1) 1))
	    (clear-row i)))))))

(defun check-loss ()
  (loop for i from 0 below (array-dimension grid 1) do
    (when (string= (aref grid 0 i) "O")
      (setf exit-game t))))

(defun check-collision (&key up right left down (piece piece))
  (let ((mx (cond (right 1)
		  (left -1)
		  (t 0)))
	(my (cond (down 1)
		  (up -1)
		  (t 0)))
	(landed nil))
    (loop for i from 0 below piece-dim do
      (loop for j from 0 below piece-dim do
	(when (and
	       (string= (aref piece i j) "O")
	       (not (is-valid-pos (+ i (pos-y piece-pos) my)
				  (+ j (pos-x piece-pos) mx))))
	  (setq landed t))))
    landed))

(defun rotate-piece ()
  (let ((new-piece nil))
    (setf new-piece (transpose-matrix piece))
    (reverse-rows new-piece)
    (when (not (check-collision :piece new-piece))
      (setf piece new-piece))))

(defun move (&key up down left right)
  (when (not (check-collision :up up :down down :left left :right right))
    (cond (right (incf (pos-x piece-pos)))
	  (left  (decf (pos-x piece-pos))))
    (cond (down (incf (pos-y piece-pos)))
	  (up   (decf (pos-y piece-pos))))))

(defun render-game-grid (renderer)
  (let* ((render-grid (copy-array grid))
	 (h (* sq-size (array-dimension grid 0)))
	 (w (* sq-size (array-dimension grid 1)))
	 (bxf (+ w sq-size))
	 (bxl (+ w (* sq-size (+ piece-dim 1))))
	 (byl (* sq-size piece-dim)))
    (sdl2:render-draw-line renderer 0 0 w 0)
    (sdl2:render-draw-line renderer 0 0 0 h)
    (sdl2:render-draw-line renderer 0 h w h)
    (sdl2:render-draw-line renderer w 0 w h)
    (sdl2:render-draw-line renderer bxf 0   bxl 0  )
    (sdl2:render-draw-line renderer bxf 0   bxf byl)
    (sdl2:render-draw-line renderer bxf byl bxl byl)
    (sdl2:render-draw-line renderer bxl 0   bxl byl)
    (render-piece render-grid)
    (loop for i from 0 below (array-dimension piece 0) do
      (loop for j from 0 below (array-dimension piece 1) do
	(when (string= (aref next-piece i j) "O")
	  (draw-rect renderer (+ bxf (* j sq-size)) (* i sq-size) sq-size sq-size))))
    (loop for i from 0 below (array-dimension grid 0) do
      (loop for j from 0 below (array-dimension grid 1) do
	(when (string= (aref render-grid i j) "O")
	  (draw-rect renderer (* j sq-size) (* i sq-size) sq-size sq-size))))))


(init-game)

(bt:make-thread (lambda ()
		  (loop while t do
		    (move-piece)
		    (check-rows)
		    (check-loss)
		    (setf next-piece (predict-piece))
		    (sleep 0.2)))
		:name "thread")

(with-window-renderer
    (window renderer)
  (sdl2-image:init '(:png))
  (let ()
    (sdl2:with-event-loop
	(:method :poll)
      (:quit () t)
      (:keydown (:keysym keysym)
		(case (sdl2:scancode keysym)
		  (:scancode-right (move :right t))
		  (:scancode-left (move :left t))
		  (:scancode-up (rotate-piece))
		  (:scancode-down (move-piece))
		  (:scancode-space (move-piece-skip))))
      (:idle ()
	     (when exit-game
	       (format t "~%Game over!~%")
	       (sdl2:push-quit-event))
	     (sdl2:set-render-draw-color renderer 0 0 0 255)
	     (sdl2:render-clear renderer)
	     (sdl2:set-render-draw-color renderer 255 255 255 0)
	     (render-game-grid renderer)
	     (sdl2:render-present renderer))))
  (sdl2-image:quit))
