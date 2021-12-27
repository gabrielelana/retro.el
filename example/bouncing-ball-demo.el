;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defun bouncing-ball-demo ()
  "Display a bouncing ball."
  (let ((width 320)
        (height 240))
    (retro-game-create :name "bouncing-ball"
                       :resolution (cons width height)
                       :bind '(("q" . retro--handle-quit))
                       :init (lambda () (list 0 3 2 3 0 0)) ; i dx dy size x y
                       :update (lambda (elapsed game-state _canvas)
                                 (message "[%03d] elapsed: %fs" (car game-state) elapsed)
                                 (let ((i (nth 0 game-state))
                                       (dx (nth 1 game-state))
                                       (dy (nth 2 game-state))
                                       (size (nth 3 game-state))
                                       (x (nth 4 game-state))
                                       (y (nth 5 game-state)))
                                   (when (or (> (+ x dx size) (- width 1))
                                             (< (+ x dx) 0))
                                     (setq dx (* -1 dx)))
                                   (when (or (> (+ y dy size) (- height 1))
                                             (< (+ y dy) 0))
                                     (setq dy (* -1 dy)))
                                   (setf (nth 0 game-state) (1+ i)
                                         (nth 1 game-state) dx
                                         (nth 2 game-state) dy
                                         (nth 4 game-state) (+ x dx)
                                         (nth 5 game-state) (+ y dy))))
                       :render (lambda (_elapsed game-state canvas)
                                 (let ((size (nth 3 game-state))
                                       (x (nth 4 game-state))
                                       (y (nth 5 game-state)))
                                   (retro--plot-filled-rectangle x y (+ x size) (+ y size) 15 canvas))))))

(defun bouncing-ball-demo-start ()
  (interactive)
  (retro--game-loop (bouncing-ball-demo)))
