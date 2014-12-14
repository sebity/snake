;;;; snake.lisp

(in-package #:snake)

;;; "snake" goes here. Hacks and glory await!

(defparameter *data-root* "src/lisp/snake/")
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *game-width* 920)
(defparameter *game-height* 640)
(defparameter *game-state* 0) ; 0=menu, 1:in-game, 2:game-over
(defparameter *arena-width* 39)
(defparameter *arena-height* 39)
(defparameter *tile-size* 16)
(defparameter *ticks* 0)

(defparameter *snake* nil)
(defparameter *snake-body* nil)
(defparameter *snake-growth* 0)
(defparameter *lives* 3)

(defparameter *food* nil)
(defparameter *food-count* 0)
(defparameter *max-food-count* 10)
(defparameter *empty-x* 0)
(defparameter *empty-y* 0)

(defparameter *level* 1)
(defparameter *total-food-eaten* 0)
(defparameter *total-score* 0)
(defparameter *longest-snake-chain* 0)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

;;;; GFX Params
(defparameter *gfx-snake* (merge-pathnames "snake_intro.png" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf* (make-instance 'SDL:ttf-font-definition
					    :size 18
					    :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-24* (make-instance 'SDL:ttf-font-definition
					    :size 24
					    :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-32* (make-instance 'SDL:ttf-font-definition
					    :size 32
					    :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)


;;;; SNAKE class

(defclass snake ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (direction :accessor direction :initarg :direction)))


;;;; FOOD class

(defclass food ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))


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


;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b)
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)))

(defun draw-text-size (string x y r g b font-size)
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font-size))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		  :color (sdl:color :r r :g g :b b)))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;;;;;;;;;;;;;;;;;;;;;; COLLISON ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; COLLIDE-WALL-P function

(defun collide-wall-p (x y)
  (if (or (<= x 0) (>= x *arena-width*) (<= y 0) (>= y *arena-height*))
      (lose-life)))


;;;; COLLIDE-SELF-P function

(defun collide-self-p (x y)
  (if (member (list x y) *snake-body* :test #'equal)
      (lose-life)))


;;;; COLLIDE-FOOD-P function

(defun collide-food-p (x y)
  (if (and (= x (x *food*)) (= y (y *food*)))
      (progn (feed-snake)
	     (create-food))))


;;;;;;;;;;;;;;;;;;;;;;;; ARENA ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-ARENA function

(defun draw-arena ()
  (loop for y from 1 to *arena-height*
     do (draw-line *tile-size* (* y *tile-size*) 
		   (* *arena-width* *tile-size*) (* y *tile-size*) 
		   55 55 55))

  (loop for x from 1 to *arena-width*
     do (draw-line (* x *tile-size*) *tile-size* 
		   (* x *tile-size*) (* *arena-height* *tile-size*)
		   55 55 55)))


;;;;;;;;;;;;;;;;;;;;;;;; FOOD ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; FIND-EMPTY-SPACE function

(defun find-empty-space ()
  (setf *empty-x* (+ (random (- *arena-width* 1)) 1))
  (setf *empty-y* (+ (random (- *arena-height* 1)) 1))
  (if (or 
       (and (= *empty-x* (x *snake*)) (= *empty-y* (y *snake*)))
       (member (list *empty-x* *empty-y*) *snake-body* :test #'equal))
      (find-empty-space)
      t))


;;;; CREATE-FOOD function

(defun create-food ()
  (find-empty-space)
  (setf *food* (make-instance 'food :x *empty-x* :y *empty-y*)))


;;;; DRAW-FOOD function

(defun draw-food ()
  (sdl:draw-box-* (* *tile-size* (x *food*)) (* *tile-size* (y *food*)) 
		  *tile-size* *tile-size*
		  :color (sdl:color :r 255 :g 0 :b 0)))




;;;;;;;;;;;;;;;;;;;;;;;; SNAKE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-SNAKE function

(defun create-snake ()
  (setf *snake* (make-instance 'snake :x 18 :y 19 :direction 'right))
  (setf *snake-body* '((17 19) (16 19) (15 19) (14 19))))


;;;; DRAW-SNAKE function

(defun draw-snake ()
  ; Draw Head
  (sdl:draw-box-* (* *tile-size* (x *snake*)) (* *tile-size* (y *snake*)) 
		  *tile-size* *tile-size*
		  :color (sdl:color :r 0 :g 150 :b 0))
  
  ; Draw Body
  (loop for elem in *snake-body*
       do (sdl:draw-box-* (* *tile-size* (first elem)) (* *tile-size* (second elem)) 
		  *tile-size* *tile-size*
		  :color (sdl:color :r 0 :g 255 :b 0))))


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
  (if (>= *ticks* (/ 30 *level*))
      (progn (move-snake)
	     (setf *ticks* 0))))


;;;; UPDATE-SNAKE-BODY function

(defun update-snake-body (x y)
  (if (zerop *snake-growth*)
      (setf *snake-body* (cons (list x y) (butlast *snake-body*)))
      (progn (setf *snake-body* (cons (list x y) *snake-body*))
	     (setf *snake-growth* (decf *snake-growth*)))))


;;;; MOVE-SNAKE function

(defun move-snake ()
  (let ((x (x *snake*))
	(y (y *snake*))
	(direction (direction *snake*)))
    (cond ((eq direction 'up) (progn (update-snake-body x y) 
				     (setf (y *snake*) (decf y))))

	  ((eq direction 'down) (progn (update-snake-body x y)
				       (setf (y *snake*) (incf y))))

	  ((eq direction 'right) (progn (update-snake-body x y)
					   (setf (x *snake*) (incf x))))

	  ((eq direction 'left) (progn (update-snake-body x y)
				       (setf (x *snake*) (decf x)))))

    (collide-wall-p x y)
    (collide-self-p x y)
    (collide-food-p x y)))


;;;; FEED-SNAKE function

(defun feed-snake ()
  (play-sound 0)
  (setf *total-food-eaten* (incf *total-food-eaten*))
  (setf *total-score* (+ *total-score* (* 10 (length *snake-body*) *level*)))
  (setf *snake-growth* (+ *snake-growth* 10))

  (setf *food-count* (decf *food-count*))
  (if (zerop *food-count*)
      (if (= *level* 10)
	  (change-game-state)
	  (progn (setf *food-count* *max-food-count*)
		 (setf *level* (incf *level*))
		 (play-sound 1)))))



;;;; LOSE-LIFE function

(defun lose-life ()
  (setf *lives* (decf *lives*))

  (if (zerop *lives*)
      (change-game-state)
      (reset-level)))
  


;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-UI function

(defun display-ui ()
  (draw-text-size "SNAKE" 740 20 255 255 0 *ttf-font-large*)

  (draw-text "STATISTICS" 640 80 255 120 0)
  (draw-text (format nil "Level: ~a" *level*) 660 120 255 255 255)
  (draw-text (format nil "Lives: ~a" *lives*) 660 160 255 255 255)
  (draw-text (format nil "Fruit Left on Level: ~a" *food-count*) 660 200 255 255 255)
  (draw-text (format nil "Total Fruit Eaten: ~a" *total-food-eaten*) 660 240 255 255 255)
  (draw-text (format nil "Total Score: ~a" *total-score*) 660 280 255 255 255)

  (draw-text "CONTROLS" 640 380 255 120 0)
  (draw-text "Move Up: Up Arrow" 660 420 255 255 255)
  (draw-text "Move Down: Down Arrow" 660 460 255 255 255)
  (draw-text "Move Left: Left Arrow" 660 500 255 255 255)
  (draw-text "Move Right: Right Arrow" 660 540 255 255 255)
  (draw-text "Quit: Q" 660 580 255 255 255))


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (draw-text-size "SNAKE GAME" 400 20 255 255 0 *ttf-font-huge*)

  (if (and (zerop *food-count*) (= *level* 10))
      (draw-text-size "CONGRATULATIONS!!!  YOU WON!!!" 300 100 255 255 0 *ttf-font-huge*)
      (progn (draw-text-size "GAME OVER!" 400 130 255 255 255 *ttf-font-huge*)
	     (draw-text-size (format nil "YOUR SCORE IS ~a" *total-score*) 
			     340 300 255 0 0 *ttf-font-huge*)))

  (draw-text "Press SPACE to Continue..." 350 540 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-snake*) 180 20)

  (draw-text "Press SPACE to Start..." 350 540 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) (progn (reset-game)
				     (setf *game-state* 1)))
	((= *game-state* 1) (setf *game-state* 2))
	((= *game-state* 2) (setf *game-state* 0))
	(t ())))


;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)
  (cond ((= *game-state* 1)
	 (draw-arena)
	 (display-ui)
	 (draw-food)
	 (update-snake)
	 (draw-snake))

	((= *game-state* 2)
	 (display-end-game))

	(t (display-menu)))
  (sdl:update-display))


;;;; RESET-LEVEL function

(defun reset-level ()
  (setf *snake-growth* 0)
  (setf *food-count* *max-food-count*)
  (create-snake)
  (create-food))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *level* 1)
  (setf *snake-growth* 0)
  (setf *lives* 3)
  (setf *total-food-eaten* 0)
  (setf *total-score* 0)
  (setf *food-count* *max-food-count*)
  (create-snake)
  (create-food))
  

;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 2))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "bite.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "level-up.ogg" *audio-root*)))
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

    (setup-audio)

    (unless (sdl:initialise-default-font *terminus-ttf*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf*))
    (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
    (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*))

    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-r (reset-level))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-up) (set-snake-direction 'up))
	     (when (sdl:get-key-state :sdl-key-down) (set-snake-direction 'down))
	     (when (sdl:get-key-state :sdl-key-left) (set-snake-direction 'left))
	     (when (sdl:get-key-state :sdl-key-right) (set-snake-direction 'right))
	     (render)))))
