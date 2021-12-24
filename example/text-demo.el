;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defconst FONT-PATH (expand-file-name "./asset/gravity.font"))

(defun text-demo ()
  "Display text on blank canvas."
  (retro-game-create :name "text"
                     :resolution '(320 . 240)
                     :bind '(("q" . retro--handle-quit))
                     :init (lambda ()
                             (let* ((font-white (retro--load-font FONT-PATH))
                                    (font-red (retro--change-colors-font font-white '(("#ffffff" . "#ff0000")
                                                                                      ("#abaeb8" . "#d10000")
                                                                                      ("#747888" . "#a30000")
                                                                                      ("#4a4f65" . "#750000")))))
                               (list 0 font-white font-red)))
                     :update (lambda (elapsed game-state canvas)
                               (retro--plot-string (nth 1 game-state)
                                                   (format "FPS: %d" (round (/ 1.0 elapsed)))
                                                   10 30 2
                                                   canvas)
                               (retro--plot-string (nth 1 game-state)
                                                   (format "FRAME: %05d" (car game-state))
                                                   10 10 2
                                                   canvas)
                               (retro--plot-string (nth 1 game-state) "HELLO WORLD" 100 100 2 canvas)
                               (retro--plot-char (nth 2 game-state) "F" 10 100 canvas)
                               (retro--plot-char (nth 2 game-state) "O" 10 120 canvas)
                               (retro--plot-char (nth 2 game-state) "O" 10 140 canvas)
                               (retro--plot-char (nth 2 game-state) "6" 10 160 canvas)
                               (retro--plot-char (nth 2 game-state) "9" 10 180 canvas)
                               (setf (car game-state) (1+ (car game-state))))
                     :render (lambda (_game-state _canvas) nil)
                     :background-color (retro--add-color-to-palette "#000000")))

(defun text-demo-start ()
  (interactive)
  (retro--game-loop (text-demo)))
