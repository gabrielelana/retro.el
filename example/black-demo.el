;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defun black-demo ()
  "Display a black screen."
  (retro-game-create :name "black"
                     :resolution '(320 . 240)
                     :bind '(("q" . retro--handle-quit))
                     :init (lambda () (list 0))
                     :update (lambda (elapsed game-state _canvas)
                               (when (eq (% (nth 0 game-state) 100) 0)
                                 (message "[%03d] FPS: %f, elapsed: %fs" (nth 0 game-state) (/ 1.0 elapsed) elapsed))
                               ;; (message "[%03d] elapsed: %fs" (car game-state) elapsed)
                               (setf (car game-state) (1+ (car game-state))))
                     :render (lambda (_elapsed _game-state _canvas) nil)))

(defun black-demo-start ()
  (interactive)
  (retro--game-loop (black-demo)))
