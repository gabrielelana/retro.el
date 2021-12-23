;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

;;; TODO: defconst retro-pi retro-pi/2
(defvar m/pi 3.1415926535897932)
(defvar m/pi/2 1.5707963267948966)

;;; ease
(defun linear (x)
  "..."
  x)

(defun smoothstep (x)
  "..."
  (* x x (- 3 (* 2 x))))

(defun ease-in-quad (x)
  (* x x))

(defun ease-out-quad (x)
  (* x (- 2.0 x)))

(defun ease-in-cubic (x)
  (* x x x))

(defun ease-out-cubic (x)
  (let ((x* (- x 1.0)))
    (+ 1.0 (* x* x* x*))))

(defun ease-in-out-quad (x)
  (if (< x 0.5)
      (* 2.0 x x)
    (- (* (- 4 (* 2.0 x)) x) 1.0)))

(defun ease-in-sine (x)
  (+ (* (- x) (cos (* x m/pi/2))) x))

;;; interpolate
(defun lerp (start end alpha)
  (round (+ (* start (- 1.0 alpha))
            (* end alpha))))

;;; animate
(defun tween (duration start end &optional ease interpolation)
  "..."
  (let ((elapsed 0))
    (lambda (dt)
      (setq elapsed (+ elapsed dt))
      (if (> elapsed duration)
          ;; TODO: make continuation optional
          (list nil (- elapsed duration) (tween duration start end ease interpolation))
        (funcall (or interpolation 'lerp)
                 start
                 end
                 (funcall (or ease 'smoothstep)
                          (/ elapsed duration)))))))

(defun tween-loop (tw)
  "..."
  (let ((v nil))
    (lambda (dt)
      (setq v (funcall tw dt))
      (when (listp v)
        (setq tw (nth 2 v)
              v (funcall tw (nth 1 v))))
      v)))

(defun tween-concat (twl twr)
  (let ((v nil)
        (ntwl nil)
        (ntwr nil)
        (tw twl))
    (lambda (dt)
      (setq v (funcall tw dt))
      (when (listp v)
        (if (eq tw twl)
            (setq ntwl (nth 2 v)
                  tw twr
                  v (funcall tw (nth 1 v)))
          (setq ntwr (nth 2 v)
                tw nil
                v (list nil (nth 1 v) (tween-concat ntwl ntwr)))))
      v)))

(defun tween-distinct-until-changed (tw &optional test)
  (let ((pv nil)
        (cv nil))
    (lambda (dt)
      (setq cv (funcall tw dt))
      (if (listp cv)
          cv
        (if (funcall (or test 'eq) cv pv)
            nil
          (setq pv cv)
          cv)))))

;;; TODO: tween-cons

(cl-defstruct (t-rex-sprite (:constructor t-rex-sprite--create)
                            (:copier nil))
  "T-Rex."
  (jump nil :type function)
  (update nil :type function)
  (render nil :type function))

(cl-defun t-rex-sprite-create (&key x y ground-y top-y)
  "Create T-Rex."
  (let ((sprite (retro--load-sprite "./asset/t-rex.sprite" x y))
        (jumping nil)
        (running-tween (tween-distinct-until-changed (tween-loop (tween 0.3 0 1 'linear))))
        (jumping-tween nil)
        y)
    (retro--play-sprite sprite "running")
    (t-rex-sprite--create :jump (lambda ()
                                  (setq jumping t
                                        jumping-tween (tween-concat (tween 0.3 ground-y top-y 'ease-out-cubic)
                                                                    (tween 0.3 top-y ground-y 'ease-in-cubic)))
                                  (retro--play-sprite sprite "jumping"))
                          :update (lambda (elapsed _canvas)
                                    (when jumping
                                      (setq y (funcall jumping-tween elapsed))
                                      (if (numberp y)
                                          (setf (retro-sprite-y sprite) y)
                                        (setq jumping nil
                                              jumping-tween nil)
                                        (setf (retro-sprite-y sprite) ground-y)
                                        (retro--play-sprite sprite "running")))
                                    (when (funcall running-tween elapsed)
                                      (retro--next-frame-sprite sprite)))
                          :render (lambda (canvas)
                                    (retro--plot-sprite sprite canvas)))))

(defun t-rex-demo-init (width height)
  (list 0
        (retro--load-background "./asset/t-rex-horizon.sprite" width 0 0 (- height 12 1))
        (t-rex-sprite-create :x 10
                             :y (- height 49)
                             :ground-y (- height 49)
                             :top-y (- height 110))
        (list (retro--load-tile "./asset/t-rex-cloud.sprite" (- width 50) 24))
        ;; (list (retro--load-tile "./asset/cactus-single-big.sprite" (- width 50) 24))
        ))

(defun t-rex-demo-update (elapsed game-state canvas)
  (message "[%03d] elapsed: %fs" (nth 0 game-state) elapsed)
  (retro--scroll-background (nth 1 game-state) (round (* 200.0 elapsed)))
  (funcall (t-rex-sprite-update (nth 2 game-state)) elapsed canvas)
  (setf (nth 3 game-state) (t-rex-demo-update-clouds (nth 3 game-state) (retro-canvas-height canvas) elapsed))
  (cl-incf (car game-state)))

(defun t-rex-demo-render (game-state canvas)
  (retro--plot-background (nth 1 game-state) canvas)
  (t-rex-demo-render-clouds (nth 3 game-state) canvas)
  (funcall (t-rex-sprite-render (nth 2 game-state)) canvas))

(defconst *t-rex-cloud-y* '(30 . 41))
(defconst *t-rex-cloud-gap* '(100 . 300))
(defconst *t-rex-cloud-velocity* 100.0)  ; pixels per second
(defconst *t-rex-cloud-max* 6)

(defun t-rex-demo-render-clouds (clouds canvas)
  "Render clouds."
  (dolist (cloud clouds)
    (retro--plot-tile cloud canvas)))

(defun t-rex-demo-update-clouds (clouds height elapsed)
  "Update clouds."
  (setf clouds (seq-filter (lambda (cloud) (> (+ (retro-tile-x cloud) (retro-tile-width cloud)) 0)) clouds))
  (dolist (cloud clouds)
    (setf (retro-tile-x cloud) (- (retro-tile-x cloud) (round (* *t-rex-cloud-velocity* elapsed)))))
  (if (>= (seq-length clouds) *t-rex-cloud-max*)
      clouds
    (let ((last-cloud (car clouds))
          (y (+ (car *t-rex-cloud-y*) (random (cdr *t-rex-cloud-y*))))
          (gap (+ (car *t-rex-cloud-gap*) (random (cdr *t-rex-cloud-gap*)))))
      (cons (retro--load-tile "./asset/t-rex-cloud.sprite" (+ gap (retro-tile-x last-cloud)) (- height y)) clouds))))

(defun t-rex-demo ()
  "Show a T-Rex running."
  (let ((width 600)
        (height 150))
    (retro-game-create :name "t-rex"
                       :resolution (cons width height)
                       :background-color (ht-get retro-palette-colors->index "#ffffff")
                       :bind `(("q" . retro--handle-quit)
                               ("SPC" . (lambda (game-state _)
                                          (funcall (t-rex-sprite-jump (nth 2 game-state))))))
                       :init (apply-partially 't-rex-demo-init width height)
                       :update 't-rex-demo-update
                       :render 't-rex-demo-render)))

(defun t-rex-demo-start ()
  (interactive)
  (retro--game-loop (t-rex-demo)))
