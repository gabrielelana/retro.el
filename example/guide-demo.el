(require 'retro (expand-file-name "./../retro.el"))

(defun guide-demo ()
  "Guide a single pixel by pressing arrow keys."
  (let ((width 50)
        (height 50))
    (retro-game-create :name "guide"
                       :resolution (cons width height)
                       :background-color (ht-get retro-palette-colors->index "#000000")
                       :bind `((("<up>" "k") . ,(lambda (game-state _) (setf (nth 2 game-state) (max (1- (nth 2 game-state)) 0))))
                               (("<down>" "j") . ,(lambda (game-state _) (setf (nth 2 game-state) (min (1+ (nth 2 game-state)) (1- height)))))
                               (("<left>" "h") . ,(lambda (game-state _) (setf (nth 1 game-state) (max (1- (nth 1 game-state)) 0))))
                               (("<right>" "l") . ,(lambda (game-state _) (setf (nth 1 game-state) (min (1+ (nth 1 game-state)) (1- width)))))
                               ("q" . retro--handle-quit))
                       :init (lambda () (list 0 0 0))  ; i x y
                       :update (lambda (_elapsed game-state _canvas)
                                 ;; (message "[%03d] (%d,%d) elapsed: %fs" (nth 0 game-state) (nth 1 game-state) (nth 2 game-state) elapsed)
                                 (cl-incf (car game-state)))
                       :render (lambda (game-state canvas)
                                 (retro--plot-pixel (nth 1 game-state)
                                                    (nth 2 game-state)
                                                    (ht-get retro-palette-colors->index "#ffffff")
                                                    (retro-canvas-pixels canvas)
                                                    width)))))
