;; -*- lexical-binding: t -*-

(require 'retro (expand-file-name "./../retro.el"))
(require 'retro-tween (expand-file-name "./../retro-tween.el"))

;;; ============================================================================
;;; Constants
(defconst *WIDTH* 600)
(defconst *HEIGHT* 150)
(defconst *SCORE-X* 430)
(defconst *SCORE-Y* 20)
(defconst *SCORE-TEXT* "HI %05d %05d")
(defconst *GROUND-VELOCITY* 200)

;;; ============================================================================
;;; T-Rex

(defconst *T-REX-X* 10)
(defconst *T-REX-GROUND-Y* 101)
(defconst *T-REX-TOP-Y* 40)

(defvar t-rex-running-tween (tween-distinct-until-changed (tween-loop (tween 0.3 0 1 'linear))))
(defvar t-rex-current-tween t-rex-running-tween)
(defvar t-rex-current-play "running")

(defun t-rex-jump (sprite)
  (when (not (equal "jumping" t-rex-current-play))
    (setq t-rex-current-play "jumping"
          t-rex-current-tween (tween-concat (tween 0.3 *T-REX-GROUND-Y* *T-REX-TOP-Y* 'ease-out-cubic)
                                            (tween 0.3 *T-REX-TOP-Y* *T-REX-GROUND-Y* 'ease-in-cubic)))
    (retro--play-sprite sprite "jumping")))

(defun t-rex-update (sprite elapsed)
  (let (y)
    (cond
     ((equal "jumping" t-rex-current-play)
      (setq y (funcall t-rex-current-tween elapsed))
      (if (numberp y)
          (setf (retro-sprite-y sprite) y)
        (setf (retro-sprite-y sprite) *T-REX-GROUND-Y*)
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

(defconst *CLOUD-Y* '(10 . 100))
(defconst *CLOUD-GAP* '(100 . 300))
(defconst *CLOUD-VELOCITY* 100.0)  ; pixels per second
(defconst *CLOUD-MAX* 6)

(defun render-clouds (clouds canvas)
  "Render clouds."
  (dolist (cloud clouds)
    (retro--plot-tile cloud canvas)))

(defun update-clouds (clouds elapsed)
  "Update clouds."
  (setf clouds (seq-filter (lambda (cloud) (> (+ (retro-tile-x cloud) (retro-tile-width cloud)) 0)) clouds))
  (dolist (cloud clouds)
    (setf (retro-tile-x cloud) (- (retro-tile-x cloud) (round (* *CLOUD-VELOCITY* elapsed)))))
  (if (>= (seq-length clouds) *CLOUD-MAX*)
      clouds
    (let ((last-cloud (car clouds))
          (y (+ (car *CLOUD-Y*) (random (cdr *CLOUD-Y*))))
          (gap (+ (car *CLOUD-GAP*) (random (cdr *CLOUD-GAP*)))))
      (cons (retro--load-tile "./asset/dino-cloud.sprite" (+ gap (retro-tile-x last-cloud)) y) clouds))))

;;; ============================================================================
;;; Cactus

(defconst *CACTUS-MAX* 3)
(defconst *CACTUS-GAP* '(180 . 320))
(defconst *CACTUS-Y* 100)

(defun render-cactuses (cactuses canvas)
  "Render cactuses."
  (dolist (cactus cactuses)
    (retro--plot-tile cactus canvas)))

(defun update-cactuses (cactuses elapsed)
  "Update cactuses."
  (setf cactuses (seq-filter (lambda (cactus) (> (+ (retro-tile-x cactus) (retro-tile-width cactus)) 0)) cactuses))
  (dolist (cactus cactuses)
    (setf (retro-tile-x cactus) (- (retro-tile-x cactus) (round (* *GROUND-VELOCITY* elapsed)))))
  (if (>= (seq-length cactuses) *CACTUS-MAX*)
      cactuses
    (let ((last-cactus (car cactuses))
          (gap (+ (car *CACTUS-GAP*) (random (cdr *CACTUS-GAP*)))))
      (cons (retro--load-tile "./asset/dino-cactus-single-big.sprite" (+ gap (retro-tile-x last-cactus)) *CACTUS-Y*) cactuses))))

;;; ============================================================================
;;; ============================================================================
;;; ============================================================================

(defun dino-init ()
  (list 0
        (retro--load-background "./asset/dino-horizon.sprite" *WIDTH* 0 0 (- *HEIGHT* 12 1))
        (retro--load-sprite "./asset/dino-t-rex.sprite" *T-REX-X* *T-REX-GROUND-Y*)
        (list (retro--load-tile "./asset/dino-cloud.sprite" (- *WIDTH* 50) 24))
        (retro--load-font "./asset/dino.font")
        (list (retro--load-tile "./asset/dino-cactus-single-big.sprite" (- *WIDTH* 50) *CACTUS-Y*))
        ))

(defun dino-update (elapsed game-state _canvas)
  ;; (message "[%03d] FPS: %f, elapsed: %fs" (nth 0 game-state) (/ 1.0 elapsed) elapsed)
  (retro--scroll-background (nth 1 game-state) (round (* *GROUND-VELOCITY* elapsed)))
  (t-rex-update (nth 2 game-state) elapsed)
  (setf (nth 3 game-state) (update-clouds (nth 3 game-state) elapsed))
  (setf (nth 5 game-state) (update-cactuses (nth 5 game-state) elapsed))
  (cl-incf (car game-state)))

(defun dino-render (_elapsed game-state canvas)
  (retro--plot-background (nth 1 game-state) canvas)
  (retro--plot-string (nth 4 game-state)
                      (format *SCORE-TEXT* 0 (nth 0 game-state))
                      *SCORE-X*
                      *SCORE-Y*
                      2
                      canvas)
  (render-clouds (nth 3 game-state) canvas)
  (render-cactuses (nth 5 game-state) canvas)
  (t-rex-render (nth 2 game-state) canvas))

(defun dino ()
  "Dino must avoid obstacles while running."
  (retro-game-create :name "dino"
                     :resolution (cons *WIDTH* *HEIGHT*)
                     :background-color (ht-get retro-palette-colors->index "#ffffff")
                     :bind `(("q" . retro--handle-quit)
                             ("SPC" . (lambda (game-state _)
                                        (t-rex-jump (nth 2 game-state)))))
                     :init 'dino-init
                     :update 'dino-update
                     :render 'dino-render))

(defun dino-start ()
  (interactive)
  (retro--game-loop (dino)))
