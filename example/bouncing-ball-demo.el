;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defconst BOUNCING-BALL-WIDTH 320 "Width of the screen.")
(defconst BOUNCING-BALL-HEIGHT 240 "Height of the screen.")

(defun bouncing-ball--update (elapsed game-state _canvas)
  (when (eq (% (nth 0 game-state) 100) 0)
    (message "[%03d] FPS: %f, elapsed: %fs" (nth 0 game-state) (/ 1.0 elapsed) elapsed))
  ;; (message "[%03d] elapsed: %fs" (car game-state) elapsed)
  (let* ((i (nth 0 game-state))
         (odx (nth 1 game-state))
         (ody (nth 2 game-state))
         (dx (* odx elapsed))
         (dy (* ody elapsed))
         (size (nth 3 game-state))
         (x (nth 4 game-state))
         (y (nth 5 game-state)))
    (when (or (> (+ x dx size) (- BOUNCING-BALL-WIDTH 1))
              (< (+ x dx) 0))
      (setq dx (* -1 dx)
            odx (* -1 odx)))
    (when (or (> (+ y dy size) (- BOUNCING-BALL-HEIGHT 1))
              (< (+ y dy) 0))
      (setq dy (* -1 dy)
            ody (* -1 ody)))
    (setf (nth 0 game-state) (1+ i)
          (nth 1 game-state) odx
          (nth 2 game-state) ody
          (nth 4 game-state) (+ x dx)
          (nth 5 game-state) (+ y dy))))

(defun bouncing-ball--render (_elapsed game-state canvas)
  (let ((size (nth 3 game-state))
        (x (round (nth 4 game-state)))
        (y (round (nth 5 game-state))))
    (retro--plot-filled-rectangle
     x
     y
     (+ x size)
     (+ y size)
     (retro--add-color-to-palette "#FFFFFF")
     canvas)))

(defun bouncing-ball-demo ()
  "Display a bouncing ball."
  (retro-game-create :name "bouncing-ball"
                     :resolution (cons BOUNCING-BALL-WIDTH BOUNCING-BALL-HEIGHT)
                     :background-color (retro--add-color-to-palette "#000000")
                     :bind '(("q" . retro--handle-quit))
                     :init (lambda () (list 0 100.0 100.0 10 0 0)) ; i dx/s dy/s size x y
                     :update #'bouncing-ball--update
                     :render #'bouncing-ball--render))

(defun bouncing-ball-demo-start ()
  (interactive)
  (retro--game-loop (bouncing-ball-demo)))

(provide 'bouncing-ball-demo)
;;; bouncing-ball-demo.el ends here
