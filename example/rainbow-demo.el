;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defun rainbow-demo ()
  "Display a colored square with changing color."
  (retro-game-create :name "rainbow"
                     :resolution '(320 . 240)
                     :bind '(("n" . (lambda (_ _) (message "pressed n")))
                             ("p" . (lambda (_ _) (message "pressed p")))
                             ("q" . retro--handle-quit))
                     :init (lambda () (list 0))
                     :update (lambda (elapsed game-state _canvas)
                               (message "[%03d] elapsed: %fs" (car game-state) elapsed)
                               (setf (car game-state) (1+ (car game-state))))
                     :render (lambda (_elapsed game-state canvas)
                               (retro--plot-filled-rectangle 1 1
                                                             (- (retro-canvas-width canvas) 2)
                                                             (- (retro-canvas-height canvas) 2)
                                                             (1+ (% (car game-state) 15))
                                                             canvas))))

(defun rainbow-demo-start ()
  (interactive)
  (retro--game-loop (rainbow-demo)))
