;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(cl-defstruct (t-rex-sprite (:constructor t-rex-sprite--create)
                            (:copier nil))
  "T-Rex."
  (running-sprite nil :type retro-sprite)
  (jumping-sprite nil :type retro-sprite)
  (current-sprite nil :type retro-sprite)
  (jump nil :type function)
  (update nil :type function)
  (render nil :type function)
  (ground-y 0 :type number)
  (top-y 0 :type number)
  (dy 0 :type number)
  (x 0 :type number)
  (y 0 :type number))

(cl-defun t-rex-sprite-create (&key x y ground-y top-y)
  "Create T-Rex."
  (let ((running-sprite (retro--load-sprite "./asset/t-rex-running.sprite" x y))
        (jumping-sprite (retro--load-sprite "./asset/t-rex-jumping.sprite" x y)))
    (t-rex-sprite--create :running-sprite running-sprite
                          :jumping-sprite jumping-sprite
                          :current-sprite running-sprite
                          :jump (lambda (t-rex) (when (= (t-rex-sprite-dy t-rex) 0)
                                                  (setf (t-rex-sprite-dy t-rex) -12
                                                        (t-rex-sprite-current-sprite t-rex) (t-rex-sprite-jumping-sprite t-rex))))
                          :update (lambda (_elapsed t-rex _canvas)
                                    (let ((dy (t-rex-sprite-dy t-rex))
                                          (y (t-rex-sprite-y t-rex)))
                                      (setq y (+ y dy))
                                      (if (< dy 0)
                                          ;; going up
                                          (when (< y (t-rex-sprite-top-y t-rex))
                                              ;; reached the top
                                              (setq y (t-rex-sprite-top-y t-rex)
                                                    dy (- dy))))
                                      (if (> dy 0)
                                          (when (> y (t-rex-sprite-ground-y t-rex))
                                              ;; reached the ground
                                              (setq y (t-rex-sprite-ground-y t-rex)
                                                    dy 0)
                                              (setf (t-rex-sprite-current-sprite t-rex) (t-rex-sprite-running-sprite t-rex))))
                                      (setf (t-rex-sprite-y t-rex) y
                                            (t-rex-sprite-dy t-rex) dy))
                                    (retro--next-frame-sprite (t-rex-sprite-current-sprite t-rex)))
                          :render (lambda (t-rex canvas)
                                    (let ((current-sprite (t-rex-sprite-current-sprite t-rex)))
                                      (setf (retro-sprite-x current-sprite) (t-rex-sprite-y t-rex))
                                      (retro--plot-sprite current-sprite canvas)))
                          :ground-y ground-y
                          :top-y top-y
                          :dy 0
                          :x x
                          :y y)))

(defun t-rex-demo ()
  "Show a T-Rex running."
  (let ((width 400)
        (height 200))
    (retro-game-create :name "t-rex"
                       :resolution (cons width height)
                       :background-color (ht-get retro-palette-colors->index "#ffffff")
                       :bind `(("q" . retro--handle-quit)
                               ("SPC" . (lambda (game-state _)
                                          (message "jump")
                                          (funcall (t-rex-sprite-jump (nth 2 game-state)) (nth 2 game-state))
                                          )))
                       :init (lambda () (list 0
                                              (retro--load-background "./asset/t-rex-horizon.sprite" width 0 0 (- height 12 1))
                                              (t-rex-sprite-create :x 10
                                                                   :y (- height 48)
                                                                   :ground-y (- height 47 2)
                                                                   :top-y (- height 150))
                                              ))
                       :update (lambda (elapsed game-state canvas)
                                 (message "[%03d] elapsed: %fs" (nth 0 game-state) elapsed)
                                 (retro--scroll-background (nth 1 game-state) 6)
                                 (funcall (t-rex-sprite-update (nth 2 game-state)) elapsed (nth 2 game-state) canvas)
                                 (cl-incf (car game-state)))
                       :render (lambda (game-state canvas)
                                 (retro--plot-background (nth 1 game-state) canvas)
                                 (funcall (t-rex-sprite-render (nth 2 game-state)) (nth 2 game-state) canvas)))))

(defun t-rex-demo-start ()
  (interactive)
  (retro--game-loop (t-rex-demo)))
