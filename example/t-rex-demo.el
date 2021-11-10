;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

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
  (render nil :type function)
  (x 0 :type number)
  (y 0 :type number))

(cl-defun t-rex-sprite-create (&key x y ground-y top-y)
  "Create T-Rex."
  (let ((running-sprite (retro--load-sprite "./asset/t-rex-running.sprite" x y))
        (jumping-sprite (retro--load-sprite "./asset/t-rex-jumping.sprite" x y))
        (current-sprite nil)
        (jumping nil)
        (running-tween (tween-distinct-until-changed (tween-loop (tween 0.3 0 1 'linear))))
        (jumping-tween nil))
    (setq current-sprite running-sprite)
    (t-rex-sprite--create :jump (lambda (_)
                                  (setq jumping t
                                        jumping-tween (tween-concat (tween 0.3 ground-y top-y 'ease-out-cubic)
                                                                    (tween 0.3 top-y ground-y 'ease-in-cubic))
                                        current-sprite jumping-sprite))
                          :update (lambda (elapsed t-rex _canvas)
                                    (when jumping
                                      (setq y (funcall jumping-tween elapsed))
                                      (when (listp y)
                                        (setq jumping nil
                                              jumping-tween nil
                                              current-sprite running-sprite
                                              y ground-y))
                                      (setf (t-rex-sprite-y t-rex) y))
                                    (when (funcall running-tween elapsed)
                                      (retro--next-frame-sprite current-sprite)))
                          :render (lambda (t-rex canvas)
                                    (setf (retro-sprite-y current-sprite) (t-rex-sprite-y t-rex))
                                    (retro--plot-sprite current-sprite canvas))
                          :x x
                          :y y
      )))

(defun t-rex-demo-init (width height)
  (list 0
        (retro--load-background "./asset/t-rex-horizon.sprite" width 0 0 (- height 12 1))
        (t-rex-sprite-create :x 10
                             :y (- height 49)
                             :ground-y (- height 49)
                             :top-y (- height 110))
        (retro--load-tile "./asset/t-rex-cloud.sprite" (- width 50) 24)
        ))

;; (defun t-rex-demo-random-clouds (clouds)) -> clouds
;; (defun t-rex-demo-random-cactus (cactus)) -> cactus
;; (defun t-rex-demo-random-pterodactyl (pterodactyls)) -> pterodactyls

(defun t-rex-demo-update (elapsed game-state canvas)
  (message "[%03d] elapsed: %fs" (nth 0 game-state) elapsed)
  (retro--scroll-background (nth 1 game-state) (round (* 200.0 elapsed)))
  (funcall (t-rex-sprite-update (nth 2 game-state)) elapsed (nth 2 game-state) canvas)
  (setf (retro-tile-x (nth 3 game-state)) (- (retro-tile-x (nth 3 game-state)) (round (* 100.0 elapsed))))
  (cl-incf (car game-state)))

(defun t-rex-demo-render (game-state canvas)
  (retro--plot-background (nth 1 game-state) canvas)
  (funcall (t-rex-sprite-render (nth 2 game-state)) (nth 2 game-state) canvas)
  (retro--plot-tile (nth 3 game-state) canvas))

(defun t-rex-demo ()
  "Show a T-Rex running."
  (let ((width 600)
        (height 150))
    (retro-game-create :name "t-rex"
                       :resolution (cons width height)
                       :background-color (ht-get retro-palette-colors->index "#ffffff")
                       :bind `(("q" . retro--handle-quit)
                               ("SPC" . (lambda (game-state _)
                                          (message "jump")
                                          (funcall (t-rex-sprite-jump (nth 2 game-state)) (nth 2 game-state)))))
                       :init (apply-partially 't-rex-demo-init width height)
                       :update 't-rex-demo-update
                       :render 't-rex-demo-render)))

(defun t-rex-demo-start ()
  (interactive)
  (retro--game-loop (t-rex-demo)))
