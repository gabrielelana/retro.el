;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))
(require 'retro-tween (expand-file-name "./../retro-tween.el"))

;;; ============================================================================
;;; Constants
(defconst *t-rex-width* 600)
(defconst *t-rex-height* 150)
(defconst *t-rex-cloud-y* '(10 . 100))
(defconst *t-rex-cloud-gap* '(100 . 300))
(defconst *t-rex-cloud-velocity* 100.0)  ; pixels per second
(defconst *t-rex-cloud-max* 6)
(defconst *t-rex-score-x* 430)
(defconst *t-rex-score-y* 20)
(defconst *t-rex-score-text* "HI %05d %05d")
(defconst *t-rex-t-rex-x* 10)
(defconst *t-rex-t-rex-ground-y* 101)
(defconst *t-rex-t-rex-top-y* 40)

;;; ============================================================================
;;; T-Rex

(defvar t-rex-running-tween (tween-distinct-until-changed (tween-loop (tween 0.3 0 1 'linear))))
(defvar t-rex-current-tween t-rex-running-tween)
(defvar t-rex-current-play "running")

(defun t-rex-jump (sprite)
  (when (not (equal "jumping" t-rex-current-play))
    (setq t-rex-current-play "jumping"
          t-rex-current-tween (tween-concat (tween 0.3 *t-rex-t-rex-ground-y* *t-rex-t-rex-top-y* 'ease-out-cubic)
                                            (tween 0.3 *t-rex-t-rex-top-y* *t-rex-t-rex-ground-y* 'ease-in-cubic)))
    (retro--play-sprite sprite "jumping")))

(defun t-rex-update (sprite elapsed)
  (let (y)
    (cond
     ((equal "jumping" t-rex-current-play)
      (setq y (funcall t-rex-current-tween elapsed))
      (if (numberp y)
          (setf (retro-sprite-y sprite) y)
        (setf (retro-sprite-y sprite) *t-rex-t-rex-ground-y*)
        (setq t-rex-current-play "running"
              t-rex-current-tween t-rex-running-tween)
        (retro--play-sprite sprite "running")))
     ((equal "running" t-rex-current-play)
      (when (funcall t-rex-current-tween elapsed)
        (retro--next-frame-sprite sprite)))
     (t (error "unreachable")))))

(defun t-rex-render (sprite canvas)
  (retro--plot-sprite sprite canvas))

;;; ============================================================================
;;; Clouds

(defun t-rex-demo-render-clouds (clouds canvas)
  "Render clouds."
  (dolist (cloud clouds)
    (retro--plot-tile cloud canvas)))

(defun t-rex-demo-update-clouds (clouds elapsed)
  "Update clouds."
  (setf clouds (seq-filter (lambda (cloud) (> (+ (retro-tile-x cloud) (retro-tile-width cloud)) 0)) clouds))
  (dolist (cloud clouds)
    (setf (retro-tile-x cloud) (- (retro-tile-x cloud) (round (* *t-rex-cloud-velocity* elapsed)))))
  (if (>= (seq-length clouds) *t-rex-cloud-max*)
      clouds
    (let ((last-cloud (car clouds))
          (y (+ (car *t-rex-cloud-y*) (random (cdr *t-rex-cloud-y*))))
          (gap (+ (car *t-rex-cloud-gap*) (random (cdr *t-rex-cloud-gap*)))))
      (cons (retro--load-tile "./asset/t-rex-cloud.sprite" (+ gap (retro-tile-x last-cloud)) y) clouds))))

;;; ============================================================================
;;; Cactus

(defconst *t-rex-ground-velocity* 250)
(defconst *t-rex-cactus-max* 3)
(defconst *t-rex-cactus-gap* '(180 . 320))
(defconst *t-rex-cactus-y* 100)

(defun t-rex-demo-render-cactuses (cactuses canvas)
  "Render cactuses."
  (dolist (cactus cactuses)
    (retro--plot-tile cactus canvas)))

(defun t-rex-demo-update-cactuses (cactuses elapsed)
  "Update cactuses."
  (setf cactuses (seq-filter (lambda (cactus) (> (+ (retro-tile-x cactus) (retro-tile-width cactus)) 0)) cactuses))
  (dolist (cactus cactuses)
    (setf (retro-tile-x cactus) (- (retro-tile-x cactus) (round (* *t-rex-ground-velocity* elapsed)))))
  (if (>= (seq-length cactuses) *t-rex-cactus-max*)
      cactuses
    (let ((last-cactus (car cactuses))
          (gap (+ (car *t-rex-cactus-gap*) (random (cdr *t-rex-cactus-gap*)))))
      (cons (retro--load-tile "./asset/t-rex-cactus-single-big.sprite" (+ gap (retro-tile-x last-cactus)) *t-rex-cactus-y*) cactuses))))

;;; ============================================================================
;;; ============================================================================
;;; ============================================================================

(defun t-rex-demo-init ()
  (list 0
        (retro--load-background "./asset/t-rex-horizon.sprite" *t-rex-width* 0 0 (- *t-rex-height* 12 1))
        (retro--load-sprite "./asset/t-rex.sprite" *t-rex-t-rex-x* *t-rex-t-rex-ground-y*)
        (list (retro--load-tile "./asset/t-rex-cloud.sprite" (- *t-rex-width* 50) 24))
        (retro--load-font "./asset/dino.font")
        (list (retro--load-tile "./asset/t-rex-cactus-single-big.sprite" (- *t-rex-width* 50) *t-rex-cactus-y*))
        ))

(defun t-rex-demo-update (elapsed game-state _canvas)
  (message "[%03d] FPS: %f, elapsed: %fs" (nth 0 game-state) (/ 1.0 elapsed) elapsed)
  (retro--scroll-background (nth 1 game-state) (round (* *t-rex-ground-velocity* elapsed)))
  (t-rex-update (nth 2 game-state) elapsed)
  (setf (nth 3 game-state) (t-rex-demo-update-clouds (nth 3 game-state) elapsed))
  (setf (nth 5 game-state) (t-rex-demo-update-cactuses (nth 5 game-state) elapsed))
  (cl-incf (car game-state)))

(defun t-rex-demo-render (game-state canvas)
  (retro--plot-background (nth 1 game-state) canvas)
  (retro--plot-string (nth 4 game-state)
                      (format *t-rex-score-text* 0 (nth 0 game-state))
                      *t-rex-score-x*
                      *t-rex-score-y*
                      2
                      canvas)
  (t-rex-demo-render-clouds (nth 3 game-state) canvas)
  (t-rex-demo-render-cactuses (nth 5 game-state) canvas)
  (t-rex-render (nth 2 game-state) canvas))

(defun t-rex-demo ()
  "Show a T-Rex running."
  (retro-game-create :name "t-rex"
                     :resolution (cons *t-rex-width* *t-rex-height*)
                     :background-color (ht-get retro-palette-colors->index "#ffffff")
                     :bind `(("q" . retro--handle-quit)
                             ("SPC" . (lambda (game-state _)
                                        (t-rex-jump (nth 2 game-state)))))
                     :init 't-rex-demo-init
                     :update 't-rex-demo-update
                     :render 't-rex-demo-render))

(defun t-rex-demo-start ()
  (interactive)
  (retro--game-loop (t-rex-demo)))
