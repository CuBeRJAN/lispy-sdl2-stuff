(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :bt-semaphore)
(ql:quickload :sdl2-ttf)


(setf *random-state* (make-random-state t))

(defconstant sq-size 46) ;; The on-screen size of a single square
(defconstant *grid-height* 20)
(defconstant *grid-width* 10)
(defconstant piece-dim 5)
(defconstant *w* *grid-width*)
(defconstant *h* (+ *grid-height* (1- piece-dim)))
(defconstant *screen-width* (+ (* (+ *w* piece-dim 1) sq-size) 3))
(defconstant *screen-height* (+ (* *grid-height* sq-size) 2))

(defvar sleep-time 0.2)
(defvar *score* 0)
(defparameter *font* nil)

(defclass tex ()
  ((renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
   (width
    :accessor tex-width
    :initform 0 )
   (height
    :accessor tex-height
    :initform 0)
   (texture
    :accessor tex-texture
    :initform nil)))

(defun free-tex (tex)
  (with-slots (texture) tex
    (sdl2:destroy-texture texture)))

(defun load-texture-from-text (renderer text)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-ttf:render-text-solid *font* text 255 255 255 0)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

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
  "Draw a rectangle with SDL2."
  `(sdl2:with-rects ((fill-rect ,x ,y ,width ,height))
     (sdl2:render-draw-rect ,renderer fill-rect)))

(defmacro fill-rect (renderer x y width height)
  "Draw a filled rectangle with SDL2."
  `(sdl2:with-rects ((fill-rect ,x ,y ,width ,height))
     (sdl2:render-fill-rect ,renderer fill-rect)
     (sdl2:set-render-draw-color renderer 200 200 200 255)
     (sdl2:render-draw-rect ,renderer fill-rect))) ;; outline

(defvar grid (make-array (list *h* *w*) :initial-element ".")) ;; 2D game grid
(defvar piece) ;; 2D grid of the current piece
(defvar piece-pos) ;; position of current piece
(defvar not-shuffled) ;; set to nil when order of pieces has been shuffled for loop
(defvar piece-index) ;; index of piece in current list of pieces
(defvar exit-game nil) ;; game will exit on next loop when set to t
(defvar next-piece nil) ;; 2D grid of the next piece
(defstruct pos x y)

;; Colors for each piece type, same as in original tetris
(defconstant colors '(
		      ("i" . (0 255 255))
		      ("l" . (255 95 31))
		      ("j" . (0 0 255))
		      ("t" . (191 64 191))
		      ("o" . (255 255 0))
		      ("z" . (255 0 0))
		      ("s" . (0 255 0))
		      ))

;; Letters are bound to colors
;; This list is shuffled around to get random piece order
(defvar *pieces*
  '(
    #2A(("." "." "i" "." ".")
	("." "." "i" "." ".")
	("." "." "i" "." ".")
	("." "." "i" "." ".")
	("." "." "." "." "."))
    
    #2A(("." "." "." "." ".")
	("." "." "l" "." ".")
	("." "." "l" "." ".")
	("." "." "l" "l" ".")
	("." "." "." "." "."))

    #2A(("." "." "." "." ".")
	("." "." "j" "." ".")
	("." "." "j" "." ".")
	("." "j" "j" "." ".")
	("." "." "." "." "."))

    #2A(("." "." "." "." ".")
	("." "." "t" "." ".")
	("." "t" "t" "t" ".")
	("." "." "." "." ".")
	("." "." "." "." "."))
    
    #2A(("." "." "." ".")
	("." "o" "o" ".")
	("." "o" "o" ".")
	("." "." "." "."))
    
    #2A(("." "." "." ".")
	("." "." "z" ".")
	("." "z" "z" ".")
	("." "z" "." ".")
	("." "." "." "."))
    
    #2A(("." "." "." ".")
	("." "s" "." ".")
	("." "s" "s" ".")
	("." "." "s" ".")
	("." "." "." "."))
    ))

(defun random-from-list (list)
  "Get random element from list."
  (let ((len (length list)))
    (nth (random len) list)))

(defun print-grid (grid)
  "Print a 2D array."
  (let ((width (array-dimension grid 1))
	(height (array-dimension grid 0)))
    (loop for i from 0 below height do
      (progn
	(loop for j from 0 below width do
	  (format t "~a" (aref grid i j)))
	(format t "~%")))))

(defun render-piece (grid)
  "Render current piece into supplied grid."
  (let ((pw (array-dimension piece 1))
	(ph (array-dimension piece 0)))
    (loop for i from 0 below ph do
      (loop for j from 0 below pw do
	(when (string/= (aref piece i j) ".")
	  (setf (aref grid
		      (+ i (pos-y piece-pos))
		      (+ j (pos-x piece-pos)))
		(aref piece i j)))))))

(defun store-piece ()
  "Permanently store current piece in the grid."
  (render-piece grid))

(defun copy-array (array &key
			   (element-type (array-element-type array))
			   (fill-pointer (and (array-has-fill-pointer-p array)
					      (fill-pointer array)))
			   (adjustable (adjustable-array-p array)))
  "Copy an array, multi-dimensional arrays supported too."
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))

(defun print-game ()
  "Print the game into command line."
  (let ((render (copy-array grid)))
    (render-piece render)
    (print-grid render)))

(defun shuffle-list (list)
  "Shuffle a list randomly."
  (loop for i from (length list) downto 2
        do (rotatef (nth (random i) list)
                    (nth (1- i) list)))
  list)

(defun predict-piece ()
  "Return next piece."
  (if (> piece-index 5)
      (if not-shuffled
	  (progn
	    (shuffle-list *pieces*)
	    (setf not-shuffled nil)
	    (car *pieces*))
	  (car *pieces*))
      (nth (+ piece-index 1) *pieces*)))

(defun random-piece ()
  "Get the next piece."
  (incf piece-index)
  (when (> piece-index 6)
    (setq piece-index 0))
  (setf not-shuffled t)
  (setf piece (nth piece-index *pieces*)))

(defun reset-pos ()
  "Reset piece position."
  (setf (pos-x piece-pos) (- (/ *w* 2) (floor piece-dim 2)))
  (setf (pos-y piece-pos) 0))

(defun init-game ()
  "Initialize game."
  (setf piece (random-from-list *pieces*))
  (setf piece-pos (make-pos :x (- (/ *w* 2) (floor piece-dim 2)) :y 0))
  (shuffle-list *pieces*)
  (setf not-shuffled t)
  (setq piece-index 0)
  (setf piece (car *pieces*)))

(defun move-piece-skip ()
  "Move piece all the way to the bottom."
  (loop while (not (has-landed)) do
    (incf (pos-y piece-pos)))
  (store-piece)
  (random-piece)
  (reset-pos))

(defun move-piece ()
  "Move piece once."
  (if (not (has-landed))
      (incf (pos-y piece-pos))
      (progn
	(store-piece)
	(random-piece)
	(reset-pos))))

(defun transpose-matrix (matrix)
  "Return transposed matrix."
  (let* ((new-matrix (make-array (list (array-dimension matrix 1)
				       (array-dimension matrix 0))))
	 (height (array-dimension matrix 0))
	 (width  (array-dimension matrix 1))
	 (new-height width)
	 (new-width height))
    (loop for i from 0 below new-height do
      (loop for j from 0 below new-width do
	(setf (aref new-matrix i j)
	      (aref matrix j i))))
    new-matrix))

(defun reverse-rows (matrix)
  "In-place reverse rows in matrix."
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
  "Check if position is not occupied or outside of game grid."
  (when (and
	 (>= x 0)
	 (< x *w*)
	 (>= y 0)
	 (< y *h*))
    (string= (aref grid y x)
	     ".")))

(defun has-landed ()
  "Check if piece has landed."
  (check-collision :down t))

(defun clear-row (n)
  "Clear supplied row."
  (decf sleep-time 0.003)
  (loop for i from n above 0 do
    (loop for j from 0 below (array-dimension grid 1) do
      (setf (aref grid i j) (aref grid (- i 1) j)))))

(defun check-rows ()
  "Find filled rows and clear them."
  (let ((do-clear nil) (cleared 0))
    (loop for i from 0 below (array-dimension grid 0) do
      (loop for j from 0 below (array-dimension grid 1) do
	(progn
	  (when (string= (aref grid i j) ".")
	    (return))
	  (when (= j (- (array-dimension grid 1) 1))
	    (clear-row i)
	    (incf cleared)))))
    (case cleared
      (1 (incf *score* 40))
      (2 (incf *score* 100))
      (3 (incf *score* 300))
      (4 (incf *score* 1200)))))

(defun check-loss ()
  "Check if the player has lost."
  (loop for i from 0 below (array-dimension grid 1) do
    (when (string/= (aref grid 2 i) ".")
      (setf exit-game t))))

(defun check-collision (&key up right left down (piece piece))
  "Check collision in specified direction for specified piece."
  (let ((mx (cond (right 1)
		  (left -1)
		  (t 0)))
	(my (cond (down 1)
		  (up -1)
		  (t 0)))
	(landed nil))
    (loop for i from 0 below (array-dimension piece 0) do
      (loop for j from 0 below (array-dimension piece 1) do
	(when (and
	       (string/= (aref piece i j) ".")
	       (not (is-valid-pos (+ i (pos-y piece-pos) my)
				  (+ j (pos-x piece-pos) mx))))
	  (setq landed t))))
    landed))

(defun rotate-piece ()
  "Rotate the current piece."
  (let ((new-piece nil))
    (setf new-piece (transpose-matrix piece))
    (reverse-rows new-piece)
    (when (not (check-collision :piece new-piece)) ;; check rotation collision
      (setf piece new-piece))))

(defun move (&key up down left right)
  "Move the current piece in specified direction."
  (when (not (check-collision :up up :down down :left left :right right))
    (cond (right (incf (pos-x piece-pos)))
	  (left  (decf (pos-x piece-pos))))
    (cond (down (incf (pos-y piece-pos)))
	  (up   (decf (pos-y piece-pos))))))

(defun set-color-by-piece (renderer letter)
  "Set renderer color according to specific grid entry."
  (let* ((color (assoc letter colors :test #'string=))
	 (r (nth 1 color))
	 (g (nth 2 color))
	 (b (nth 3 color)))
    (sdl2:set-render-draw-color renderer r g b 255)))

(defun render-game-grid (renderer)
  "Render the game grid into the SDL2 window."
  (let* ((render-grid (copy-array grid))
	 (h (+ (* sq-size (- (array-dimension grid 0) (- piece-dim 1))) 1))
	 (w (+ (* sq-size (array-dimension grid 1)) 1))
	 (bxf (+ w sq-size -1))
	 (bxl (+ w (* sq-size (+ piece-dim 1)) 1))
	 (byl (+ (* sq-size piece-dim) 1)))
    (sdl2:render-draw-line renderer 0 0 w 0) ;; Render game grid outline
    (sdl2:render-draw-line renderer 0 0 0 h)
    (sdl2:render-draw-line renderer 0 h w h)
    (sdl2:render-draw-line renderer w 0 w h)
    (sdl2:render-draw-line renderer bxf 0   bxl 0  ) ;; Next piece box outline
    (sdl2:render-draw-line renderer bxf 0   bxf byl)
    (sdl2:render-draw-line renderer bxf byl bxl byl)
    (sdl2:render-draw-line renderer bxl 0   bxl byl)
    (render-piece render-grid)
    (loop for i from 0 below (array-dimension next-piece 0) do 
      (loop for j from 0 below (array-dimension next-piece 1) do
	(when (string/= (aref next-piece i j) ".")
	  (set-color-by-piece renderer (aref next-piece i j))
	  (fill-rect renderer
		     (+ bxf 1 (* j sq-size))
		     (+ (* i sq-size) 1)
		     sq-size sq-size))))
    (loop for i from (- piece-dim 1) below (array-dimension grid 0) do
      (loop for j from 0 below (array-dimension grid 1) do
	(when (string/= (aref render-grid i j) ".")
	  (set-color-by-piece renderer (aref render-grid i j))
	  (fill-rect renderer
		     (+ (* j sq-size) 1)
		     (+ (* (- i (- piece-dim 1)) sq-size) 1)
		     sq-size sq-size))))))

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun render (tex x y &key clip angle center (flip :none))
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect clip
                         :dest-rect (sdl2:make-rect x
                                                    y
                                                    (if clip (sdl2:rect-width clip) width)
                                                    (if clip (sdl2:rect-height clip) height))
                         :angle angle
                         :center center
                         :flip (list flip))))

(defun run ()
  (setf *random-state* (make-random-state t))
  (init-game)

  (bt:make-thread (lambda ()
		    (let ((start nil)
			  (end nil))
		      (loop while t do
			(setq start (get-internal-real-time))
			(move-piece)
			(check-rows)
			(check-loss)
			(setq end (get-internal-real-time))
			(setf next-piece (predict-piece))
			(sleep (- sleep-time (/ (- end start) 1000000)))))) ;; Substract time spent on calculations
		  :name "thread")

  (with-window-renderer
      (window renderer)
    (sdl2-image:init '(:png))
    (sdl2-ttf:init)
    (setf *font* (sdl2-ttf:open-font "./square.ttf" 28))
    (let ((l-tex (load-texture-from-text renderer "0")))
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
	       (setf l-tex (load-texture-from-text renderer (write-to-string *score*)))
	       (set-color l-tex 255 255 255)
	       (render l-tex (* sq-size (+ *grid-width* (ceiling piece-dim 2))) (* (1+ piece-dim) sq-size))
	       (sdl2:set-render-draw-color renderer 255 255 255 0)
	       (render-game-grid renderer)
	       (sdl2:render-present renderer)))
	  (free-tex l-tex))
      (sdl2-ttf:quit)
      (sdl2-image:quit)))

;;(sb-ext:save-lisp-and-die #p"tetris" :toplevel #'run :executable t :compression 9)
(run)
