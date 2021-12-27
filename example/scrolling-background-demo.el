;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defun scrolling-background-demo ()
  "Show a background scrolling on the right."
  (let ((width 100)
        (height 178))
    (retro-game-create :name "scrolling-background"
                       :resolution (cons width height)
                       :background-color (ht-get retro-palette-colors->index "#000000")
                       :bind `(("q" . retro--handle-quit))
                       :init (lambda () (list 0 (retro--load-background "./asset/flappy-bird-background.sprite" width 0 0 0)))  ; i background
                       :update (lambda (elapsed game-state _canvas)
                                 (message "[%03d] elapsed: %fs" (nth 0 game-state) elapsed)
                                 (retro--scroll-background (nth 1 game-state) 3)
                                 (cl-incf (car game-state)))
                       :render (lambda (_elapsed game-state canvas)
                                 (retro--plot-background (nth 1 game-state) canvas)))))

(defun scrolling-background-demo-start ()
  (interactive)
  (retro--game-loop (scrolling-background-demo)))
