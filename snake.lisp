;;;; snake.lisp

(in-package #:snake)

;;; "snake" goes here. Hacks and glory await!

(defparameter *data-root* "src/lisp/snake/")
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))

;;;; Game Params
(defparameter *game-width* 640)
(defparameter *game-height* 640)
(defparameter *game-state* 0) ; 0=menu, 1:ready, 2:in-game, 3:win-lose
(defparameter *arena-width* 39)
(defparameter *arena-height* 39)
(defparameter *arena* nil)
(defparameter *tile-size* 16)
(defparameter *ticks* 0)

(defparameter *snake* nil)
(defparameter *snake-body* nil)
(defparameter *snake-size* *tile-size*)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

;;;; Font Params
(defparameter *terminus-ttf* (make-instance 'SDL:ttf-font-definition
					    :size 24
					    :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))


;;;; SNAKE class

(defclass snake ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (direction :accessor direction :initarg :direction)))


;;;; CONTINUABLE macro

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;; UPDATE-SWANK function

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b)
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)))


(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		  :color (sdl:color :r r :g g :b b)))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;; COLLIDE-WALL-P function

(defun collide-wall-p (x y)
  (if (or (= x 1) (= x (- *arena-width* 1)) (= y 1) (= y (- *arena-height* 1)))
      t
      nil))


;;;; CREATE-ARENA function

(defun create-arena ()
  (setf *arena* (make-array (list *arena-height* *arena-width*))))


;;;; DRAW-ARENA function

(defun draw-arena ()
  (loop for y from 1 to *arena-height*
     do (draw-line *tile-size* (* y *tile-size*) 
		   (- *game-width* *tile-size*) (* y *tile-size*) 
		   55 55 55))

  (loop for x from 1 to *arena-width*
     do (draw-line (* x *tile-size*) *tile-size* 
		   (* x *tile-size*) (- *game-height* *tile-size*)
		   55 55 55)))


;;;; CREATE-SNAKE function

(defun create-snake ()
  (setf *snake* (make-instance 'snake :x 5 :y 19 :direction 'right))
  (setf *snake-body* '((4 19) (3 19) (2 19) (1 19))))

;;;; DRAW-SNAKE function

(defun draw-snake ()
  (sdl:draw-box-* (* *tile-size* (x *snake*)) (* *tile-size* (y *snake*)) 
		  *snake-size* *snake-size*
		  :color (sdl:color :r 0 :g 255 :b 0)
		  :stroke-color (sdl:color :r 255 :g 0 :b 0))
  (loop for elem in *snake-body*
       do (sdl:draw-box-* (* *tile-size* (first elem)) (* *tile-size* (second elem)) 
		  *snake-size* *snake-size*
		  :color (sdl:color :r 0 :g 255 :b 0)
		  :stroke-color (sdl:color :r 255 :g 125 :b 0))))


;;;; MOVE-SNAKE function

(defun set-snake-direction (direction)
  (cond ((and (eq direction 'up)
	      (not (eq (direction *snake*) 'down)))
	 (setf (direction *snake*) 'up))
	
	((and (eq direction 'down)
	      (not (eq (direction *snake*) 'up)))
	 (setf (direction *snake*) 'down))
	
	((and (eq direction 'left)
	      (not (eq (direction *snake*) 'right)))
	 (setf (direction *snake*) 'left))
	
	((and (eq direction 'right)
	      (not (eq (direction *snake*) 'left)))
	 (setf (direction *snake*) 'right))))


;;;; UPDATE-SNAKE function

(defun update-snake ()
  (setf *ticks* (incf *ticks*))
  (if (>= *ticks* 30)
      (progn (move-snake)
	     (setf *ticks* 0))))


;;;; MOVE-SNAKE function

(defun move-snake ()
  (let ((x (x *snake*))
	(y (y *snake*))
	(direction (direction *snake*)))
    (cond ((eq direction 'up) (if (collide-wall-p x (- y 1))
				  t
				  (progn (update-snake-body x y) 
					 (setf (y *snake*) (decf y)))))

	  ((eq direction 'down) (if (collide-wall-p x (+ y 1))
				    t
				    (progn (update-snake-body x y)
					   (setf (y *snake*) (incf y)))))

	  ((eq direction 'right) (if (collide-wall-p (+ x 1) y)
				     t				    
				     (progn (update-snake-body x y)
					   (setf (x *snake*) (incf x)))))

	  ((eq direction 'left) (if (collide-wall-p (- x 1) y)
				    t
				    (progn (update-snake-body x y)
					   (setf (x *snake*) (decf x))))))))


;;;; UPDATE-SNAKE-BODY function

(defun update-snake-body (x y)
  (setf *snake-body* (cons (list x y) (butlast *snake-body*))))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (draw-text "Main Menu" 20 100 255 255 255))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) ())
	(t ())))


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)
  ;(display-menu)
  (draw-arena)
  (update-snake)
  (draw-snake)
  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (create-arena)
  (create-snake))


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 3))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    ;(setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "beep.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "Snake")
    (setf (sdl:frame-rate) 60)

    ;(setup-audio)

    (unless (sdl:initialise-default-font *terminus-ttf*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))
    
    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 ;(:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-up) (set-snake-direction 'up))
	     (when (sdl:get-key-state :sdl-key-down) (set-snake-direction 'down))
	     (when (sdl:get-key-state :sdl-key-left) (set-snake-direction 'left))
	     (when (sdl:get-key-state :sdl-key-right) (set-snake-direction 'right))
	     (render)))))
