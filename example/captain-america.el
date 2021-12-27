;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))

(defconst CAPTAIN-AMERICA-WIDTH 320)
(defconst CAPTAIN-AMERICA-HEIGHT 240)
(defconst CAPTAIN-AMERICA-SPRITE-DXY 4)
(defconst CAPTAIN-AMERICA-SPRITE (expand-file-name "./asset/captain-america.sprite"))

(defun captain-america-left (game-state _)
  (let* ((sprite (nth 1 game-state))
         (sx (retro-sprite-x sprite)))
    (setf (retro-sprite-x sprite) (- sx CAPTAIN-AMERICA-SPRITE-DXY))
    (if (eq (nth 2 game-state) :left)
        (retro--next-frame-sprite (nth 1 game-state))
      (setf (nth 2 game-state) :left)
      (retro--play-sprite sprite "walk-left"))))

(defun captain-america-right (game-state _)
  (let* ((sprite (nth 1 game-state))
         (sx (retro-sprite-x sprite)))
    (setf (retro-sprite-x sprite) (+ sx CAPTAIN-AMERICA-SPRITE-DXY))
    (if (eq (nth 2 game-state) :right)
        (retro--next-frame-sprite (nth 1 game-state))
      (setf (nth 2 game-state) :right)
      (retro--play-sprite sprite "walk-right"))))

(defun captain-america-up (game-state _)
  (let* ((sprite (nth 1 game-state))
         (sy (retro-sprite-y sprite)))
    (setf (retro-sprite-y sprite) (- sy CAPTAIN-AMERICA-SPRITE-DXY))
    (if (eq (nth 2 game-state) :up)
        (retro--next-frame-sprite (nth 1 game-state))
      (setf (nth 2 game-state) :up)
      (retro--play-sprite sprite "walk-back"))))

(defun captain-america-down (game-state _)
  (let* ((sprite (nth 1 game-state))
         (sy (retro-sprite-y sprite)))
    (setf (retro-sprite-y sprite) (+ sy CAPTAIN-AMERICA-SPRITE-DXY))
    (if (eq (nth 2 game-state) :down)
        (retro--next-frame-sprite (nth 1 game-state))
      (setf (nth 2 game-state) :down)
      (retro--play-sprite sprite "walk-front"))))

(defun captain-america-init ()
  (let ((sprite (retro--load-sprite CAPTAIN-AMERICA-SPRITE 10 160)))
    (retro--play-sprite sprite "walk-right")
    (list 0                       ; frame counter
          sprite                  ; sprite
          :right                  ; direction of the sprite
          )))

(defun captain-america-demo ()
  "Move an animated sprite in a grid."
  (retro-game-create :name "captain-america"
                     :resolution (cons CAPTAIN-AMERICA-WIDTH CAPTAIN-AMERICA-HEIGHT)
                     :background-color (ht-get retro-palette-colors->index "#ffffff")
                     :bind '((("<left>" "h") . captain-america-left)
                             (("<right>" "l") . captain-america-right)
                             (("<up>" "u") . captain-america-up)
                             (("<down>" "d") . captain-america-down)
                             ("q" . retro--handle-quit))
                     :init 'captain-america-init
                     :update (lambda (elapsed game-state _canvas)
                               (when (eq 0 (mod (car game-state) 100))
                                 (message "FPS: %f - %f" (/ 1.0 elapsed) elapsed))
                               (cl-incf (car game-state)))
                     :render (lambda (_elapsed game-state canvas)
                               (retro--plot-sprite (nth 1 game-state) canvas))))

(defun captain-america-start ()
  (interactive)
  (retro--game-loop (captain-america-demo)))
