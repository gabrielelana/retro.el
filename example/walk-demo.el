(require 'retro (expand-file-name "./../retro.el"))

(defun walk-demo ()
  "Move an animated sprite left and right."
  (let ((width 320)
        (height 240))
    (retro-game-create :name "walk"
                       :resolution (cons width height)
                       :background-color (ht-get retro-palette-colors->index "#ffffff")
                       :bind `((("<left>" "h") . ,(lambda (game-state _)
                                                    (retro--move-sprite (nth 1 game-state) -2 0 0 0 (1- width) (1- height))
                                                    (retro--next-frame-sprite (nth 1 game-state))
                                                    (when (nth 2 game-state)
                                                      (retro--flip-h-sprite (nth 1 game-state))
                                                      (setf (nth 2 game-state) nil))
                                                    ))
                               (("<right>" "l") . ,(lambda (game-state _)
                                                     (retro--move-sprite (nth 1 game-state) 2 0 0 0 (1- width) (1- height))
                                                     (retro--next-frame-sprite (nth 1 game-state))
                                                     (when (not (nth 2 game-state))
                                                       (retro--flip-h-sprite (nth 1 game-state))
                                                       (setf (nth 2 game-state) t))
                                                     ))
                               ("q" . retro--handle-quit))
                       :init (lambda () (list 0 (retro--load-sprite "pink_walk.sprite" 10 160) t))
                       :update (lambda (_elapsed game-state _canvas)
                                 ;; (message "[%03d] (%d,%d) elapsed: %fs" (nth 0 game-state) (nth 1 game-state) (nth 2 game-state) elapsed)
                                 (cl-incf (car game-state)))
                       :render (lambda (game-state canvas)
                                 (retro--plot-sprite (nth 1 game-state) canvas)))))
