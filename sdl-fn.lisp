(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "Common Lisp SDL2 Tetris"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,renderer (sdl2:create-renderer window -1 '(:ACCELERATED :PRESENTVSYNC))))
         ,@body))))

(defun draw-circle (renderer cx cy radius)
  (let* ((diameter (* radius 2))
	 (x (- radius 1))
	 (y 0)
	 (tx 1)
	 (ty 1)
	 (err (- tx diameter))
	 (status 0))
    (loop while (>= x y) do
      (dolist (octant (list (cons (+ cx x) (- cy y))
			    (cons (+ cx x) (+ cy y))
			    (cons (- cx x) (- cy y))
			    (cons (- cx x) (+ cy y))
			    (cons (+ cx y) (- cy x))
			    (cons (+ cx y) (+ cy x))
			    (cons (- cx y) (- cy x))
			    (cons (- cx y) (+ cy x))))
	(eval `(sdl2:render-draw-point ,renderer ,(car octant) ,(cdr octant))))
      (when (< status 0) (setq status -1))
      (when (<= err 0)
	(incf y)
	(incf err ty)
	(incf ty 2))
      (when (> err 0)
	(decf x)
	(incf tx 2)
	(incf err (- tx diameter))))
    status))

(defun fill-circle (renderer x y radius)
  (let ((ox 0)
	(oy radius)
	(d (- radius 1))
	(status 0))
    (loop while (>= oy ox) do
      (dolist (q (list `(,(- x oy) ,(+ y ox) ,(+ x oy) ,(+ y ox))
		       `(,(- x ox) ,(+ y oy) ,(+ x ox) ,(+ y oy))
		       `(,(- x ox) ,(- y oy) ,(+ x ox) ,(- y oy))
		       `(,(- x oy) ,(- y ox) ,(+ x oy) ,(- y ox))))
	(eval
	 `(sdl2:render-draw-line ,renderer
				 ,(nth 0 q)
				 ,(nth 1 q)
				 ,(nth 2 q)
				 ,(nth 3 q))))
      (when (< status 0)
	(setq status -1)
	(return status))
      (cond
	((>= d (* 2 ox)) (decf d (+ (* 2 ox) 1)) (incf ox))
	((< d (* 2 (- radius oy))) (incf d (* 2 (- oy 1))) (decf oy))
	(t (incf d (* 2 (- oy ox 1))) (decf oy) (incf ox))))
    status))


(defmacro draw-rect (renderer x y width height)
  `(sdl2:with-rects ((fill-rect ,x ,y ,width ,height))
		    (sdl2:render-draw-rect ,renderer fill-rect)))

(defmacro fill-rect (renderer x y width height)
  `(sdl2:with-rects ((fill-rect ,x ,y ,width ,height))
		    (sdl2:render-fill-rect ,renderer fill-rect)))

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

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun load-texture-from-file (renderer filename)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                        0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

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
