(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :sdl2-ttf)
(ql:quickload :bt-semaphore)

(defparameter *font* nil)
(defconstant *screen-width* 1280)
(defconstant *screen-height* 960)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "Common Lisp SDL2 Pong"
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

(defun load-texture-from-text (renderer text)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-ttf:render-text-solid *font* text 255 255 255 0)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
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


(defstruct rect x y width height)
(defstruct circle x y radius filled)


(defparameter *dx* 3)
(defparameter *dy* 3)

(defconstant *pad-width* 25)
(defconstant *pad-height* 148)

(defparameter *left-pad*
  (let ((width *pad-width*) (height *pad-height*))
    (make-rect :x 0
	       :y (- (/ *screen-height* 2) (/ height 2))
	       :width width
	       :height height)))

(defparameter *right-pad*
  (let ((width *pad-width*) (height *pad-height*))
    (make-rect :x (- *screen-width* width)
	       :y (- (/ *screen-height* 2) (/ height 2))
	       :width width
	       :height height)))

(defparameter *ball*
  (make-rect :x (- (/ *screen-width* 2) (/ 16 2))
	     :y (- (/ *screen-height* 2) (/ 16 2))
	     :width 16
	     :height 16))

(defun detect-collision (ball pad)
  (let* ((pxf (rect-x pad))
	 (pxs (+ pxf (rect-width pad)))
	 (pyf (rect-y pad))
	 (pys (+ pyf (rect-height pad)))
	 (byf (rect-y ball))
	 (bys (+ byf (rect-height ball)))
	 (bxf (rect-x ball))
	 (bxs (+ bxf (rect-width ball))))
    (when (and (>= bys pyf) (<= byf pys))
      (if (> *dx* 0)
	  (when (and (<= (- pxf bxs) 0) (>= (- pxf bxs) -4))
	    (setf *dx* (* *dx* -1)))
  	  (when (and (<= (- bxf pxs) 0) (>= (- bxf pxs) -4))
	    (setf *dx* (* *dx* -1)))))))

(defparameter *keyr* nil)
(defparameter *keyl* nil)
(defparameter *l-score* 0)
(defparameter *r-score* 0)

(defun eval-key (key)
  (cond ((string= key "u") (when (> (rect-y *right-pad*) 0)
			     (decf (rect-y *right-pad*) 4)))
	((string= key "d") (when (< (rect-y *right-pad*) (- *screen-height*
							    (rect-height *right-pad*)))
			     (incf (rect-y *right-pad*) 4)))
	((string= key "lu") (when (> (rect-y *left-pad*) 0)
			     (decf (rect-y *left-pad*) 4)))
	((string= key "ld") (when (< (rect-y *left-pad*) (- *screen-height*
							    (rect-height *left-pad*)))
			     (incf (rect-y *left-pad*) 4)))))

(bt:make-thread (lambda ()
		  (loop with r = nil while t do
		    (incf (rect-x *ball*) *dx*)
		    (incf (rect-y *ball*) *dy*)
		    (when
			(or (>= (rect-y *ball*)
				(- *screen-height* (/ (rect-height *ball*) 2)))
			    (<= (rect-y *ball*)
				(+ (/ (rect-height *ball*) 2))))
		      (setf *dy* (* *dy* -1)))
		    (when (>= (rect-x *ball*)
			      (- *screen-width* (/ (rect-width *ball*) 2)))
		      (incf *l-score*)
		      (setf r t))
		    (when (<= (rect-x *ball*)
			      (+ (/ (rect-width *ball*) 2)))
		      (incf *r-score*)
		      (setf r t))
		    (when r
		      (setf (rect-x *ball*)
			    (- (/ *screen-width* 2) (/ (rect-width *ball*) 2)))
		      (setf (rect-y *ball*)
			    (- (/ *screen-height* 2) (/ (rect-height *ball*) 2)))
		      (setf (rect-y *left-pad*)
			    (- (/ *screen-height* 2) (/ (rect-height *left-pad*) 2)))
		      (setf (rect-y *right-pad*)
			    (- (/ *screen-height* 2) (/ (rect-height *right-pad*) 2)))
		      (sleep 1)
		      (setf r nil))
		    (detect-collision *ball* *left-pad*)
		    (detect-collision *ball* *right-pad*)
		    (eval-key *keyr*)
		    (eval-key *keyl*)
		    (sleep 0.006)))
		:name "move-thread")


(with-window-renderer
 (window renderer)
 (sdl2-image:init '(:png))
 (sdl2-ttf:init)
 (setf *font* (sdl2-ttf:open-font "./square.ttf" 28))
 (let ((l-tex (load-texture-from-text renderer "0"))
       (r-tex (load-texture-from-text renderer "0"))
       (last-l *l-score*)
       (last-r *r-score*))
   (sdl2:with-event-loop
    (:method :poll)
    (:quit () t)
    (:keydown (:keysym keysym)
	      (case (sdl2:scancode keysym)
		    (:scancode-up (setf *keyr* "u"))
		    (:scancode-down (setf *keyr* "d"))
		    (:scancode-w (setf *keyl* "lu"))
		    (:scancode-s (setf *keyl* "ld"))
		    (t (progn (setf *keyr* nil) (setf *keyl* nil)))))
    (:idle ()
	   (when (not
		  (or (sdl2:keyboard-state-p :scancode-up)
		      (sdl2:keyboard-state-p :scancode-down)))
	     (setf *keyr* nil))
	   (when (not
		  (or (sdl2:keyboard-state-p :scancode-w)
		      (sdl2:keyboard-state-p :scancode-s)))
	     (setf *keyl* nil))
	   (when (not (and (= last-l *l-score*)
			   (= last-r *r-score*)))
	     (setf l-tex (load-texture-from-text renderer (write-to-string *l-score*)))
	     (setf r-tex (load-texture-from-text renderer (write-to-string *r-score*))))
	   (setf last-l *l-score*)
	   (setf last-r *r-score*)
	   (sdl2:set-render-draw-color renderer 0 0 0 255)
	   (sdl2:render-clear renderer)
	   (sdl2:set-render-draw-color renderer 255 255 255 0)
	   (sdl2:render-draw-line renderer
				  (/ *screen-width* 2)
				  0
				  (/ *screen-width* 2)
				  *screen-height*)
	   (fill-circle renderer
		      (+ (rect-x *ball*) (/ (rect-width *ball*) 2))
		      (+ (rect-y *ball*) (/ (rect-height *ball*) 2))
		      (/ (rect-width *ball*) 2))
	   (fill-rect renderer
		      (rect-x *left-pad*)
		      (rect-y *left-pad*)
		      (rect-width *left-pad*)
		      (rect-height *left-pad*))
	   (fill-rect renderer
		      (rect-x *right-pad*)
		      (rect-y *right-pad*)
		      (rect-width *right-pad*)
		      (rect-height *right-pad*))
	   (set-color l-tex 255 255 255)
	   (set-color r-tex 255 255 255)
	   (render r-tex (+ (/ *screen-width* 2) 40) 40)
	   (render l-tex (- (/ *screen-width* 2) 40 28) 40)
	   (sdl2:render-present renderer)))
   (free-tex l-tex)
   (free-tex r-tex))
 (sdl2-ttf:quit)
 (sdl2-image:quit))
