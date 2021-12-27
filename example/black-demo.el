;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defun black-demo ()
  "Display a black screen."
  (retro-game-create :name "black"
                     :resolution '(320 . 240)
                     :bind '(("q" . retro--handle-quit))
                     :init (lambda () (list 0))
                     :update (lambda (elapsed game-state _canvas)
                               (message "[%03d] elapsed: %fs" (car game-state) elapsed)
                               (setf (car game-state) (1+ (car game-state))))
                     :render (lambda (_elapsed _game-state _canvas) nil)))

(defun black-demo-start ()
  (interactive)
  (retro--game-loop (black-demo)))
