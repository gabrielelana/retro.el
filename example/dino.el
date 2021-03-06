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
;;; Tiles

(defun initial-tiles (tiles-count tile-y tile-gap)
  "Generate the initial tiles."
  (let ((tiles (first-tile tile-y)))
    (dotimes (_ (1- tiles-count))
      (setq tiles (append-tile tiles tiles-count tile-y tile-gap)))
    tiles))

(defun first-tile (tile-y)
  "Generate the first tile."
  (list (cons (+ *WIDTH* 50)
              (+ (car tile-y) (random (cdr tile-y))))))

(defun append-tile (tiles tiles-count tile-y tile-gap)
  "Append one tile to list of tiles if there are less tiles than wanted."
  (if (>= (length tiles) tiles-count)
      tiles
    (let ((last-tile (car (last tiles)))
          x y)
      (setq x (+ (car last-tile) (car tile-gap) (random (cdr tile-gap)))
            y (+ (car tile-y) (random (cdr tile-y))))
      (reverse (cons (cons x y) (reverse tiles))))))

(defun remove-passed-tiles (tiles tile-width)
  "Remove tiles that are no more visible."
  (seq-filter (lambda (tile) (> (+ (car tile) tile-width) 0)) tiles))

;;; ============================================================================
;;; Clouds

(defconst *CLOUD-Y* '(10 . 100))
(defconst *CLOUD-GAP* '(100 . 300))
(defconst *CLOUD-VELOCITY* 100.0)  ; pixels per second
(defconst *CLOUD-MAX* 6)

(defun render-clouds (clouds canvas)
  "Render clouds."
  (dolist (cloud-coordinates (nth 0 clouds))
    (setf (retro-tile-x (nth 1 clouds)) (car cloud-coordinates)
          (retro-tile-y (nth 1 clouds)) (cdr cloud-coordinates))
    (retro--plot-tile (nth 1 clouds) canvas)))

(defun update-clouds (clouds elapsed)
  "Update clouds."
  (let ((coordinates (nth 0 clouds))
        (tile (nth 1 clouds)))
    (setq coordinates (remove-passed-tiles coordinates (retro-tile-width tile)))
    (while (< (length coordinates) *CLOUD-MAX*)
      (setq coordinates (append-tile coordinates *CLOUD-MAX* *CLOUD-Y* *CLOUD-GAP*)))
    (setq coordinates
          (mapcar (lambda (xy)
                    (cons (- (car xy) (round (* *CLOUD-VELOCITY* elapsed))) (cdr xy)))
                  coordinates))
    (list coordinates tile)))

;;; ============================================================================
;;; Cactus

(defconst *CACTUS-MAX* 3)
(defconst *CACTUS-GAP* '(180 . 320))
(defconst *CACTUS-Y* '(100 . 1))

(defun render-cactuses (cactuses canvas)
  "Render cactuses."
  (dolist (cactus-coordinates (nth 0 cactuses))
    (setf (retro-tile-x (nth 1 cactuses)) (car cactus-coordinates)
          (retro-tile-y (nth 1 cactuses)) (cdr cactus-coordinates))
    (retro--plot-tile (nth 1 cactuses) canvas)))

(defun update-cactuses (cactuses elapsed)
  "Update cactuses."
  (let ((coordinates (nth 0 cactuses))
        (tile (nth 1 cactuses)))
    (setq coordinates (remove-passed-tiles coordinates (retro-tile-width tile)))
    (while (< (length coordinates) *CACTUS-MAX*)
      (setq coordinates (append-tile coordinates *CACTUS-MAX* *CACTUS-Y* *CACTUS-GAP*)))
    (setq coordinates
          (mapcar (lambda (xy)
                    (cons (- (car xy) (round (* *GROUND-VELOCITY* elapsed))) (cdr xy)))
                  coordinates))
    (list coordinates tile)))

;;; ============================================================================
;;; Collision detection

(defmacro bb-left (bb) `(caar ,bb))
(defmacro bb-right (bb) `(cadr ,bb))
(defmacro bb-top (bb) `(cdar ,bb))
(defmacro bb-bottom (bb) `(cddr ,bb))

(defmacro bb-sprite (sprite) `(cons (cons (retro-sprite-x ,sprite)
                                          (retro-sprite-y ,sprite))
                                    (cons (+ (retro-sprite-x ,sprite) (retro-sprite-width ,sprite))
                                          (+ (retro-sprite-y ,sprite) (retro-sprite-height ,sprite)))))

(defun bb-intersect? (bbl bbr)
  "Check intersection between bounding boxes."
  (and (< (bb-left bbl) (bb-right bbr))
       (> (bb-right bbl) (bb-left bbr))
       (< (bb-top bbl) (bb-bottom bbr))
       (> (bb-bottom bbl) (bb-top bbr))))

(defun collision? (t-rex-bb tiles-bb)
  "Detect collision between T-Rex and some tiles"
  (->> tiles-bb
       (seq-filter (lambda (tile-bb) (<= (bb-left tile-bb) (bb-left t-rex-bb))))
       (seq-some (lambda (tile-bb) (bb-intersect? t-rex-bb tile-bb)))))

;;; ============================================================================

(defun dino-init ()
  (list 0
        (retro--load-background "./asset/dino-horizon.sprite" *WIDTH* 0 0 (- *HEIGHT* 12 1))
        (retro--load-sprite "./asset/dino-t-rex.sprite" *T-REX-X* *T-REX-GROUND-Y*)
        (list (initial-tiles *CLOUD-MAX* *CLOUD-Y* *CLOUD-GAP*) (retro--load-tile "./asset/dino-cloud.sprite" 0 0))
        (retro--load-font "./asset/dino.font")
        (list (initial-tiles *CACTUS-MAX* *CACTUS-Y* *CACTUS-GAP*) (retro--load-tile "./asset/dino-cactus-single-big.sprite" 0 0))
        :playing
        ))

(defun dino-update (elapsed game-state _canvas)
  (if (eq (nth 6 game-state) :game-over)
      nil
    ;; (message "[%03d] FPS: %f, elapsed: %fs" (nth 0 game-state) (/ 1.0 elapsed) elapsed)
    (retro--scroll-background (nth 1 game-state) (round (* *GROUND-VELOCITY* elapsed)))
    (t-rex-update (nth 2 game-state) elapsed)
    (setf (nth 3 game-state) (update-clouds (nth 3 game-state) elapsed))
    (setf (nth 5 game-state) (update-cactuses (nth 5 game-state) elapsed))
    (let ((cactus-width (retro-tile-width (nth 1 (nth 5 game-state))))
          (cactus-height (retro-tile-height (nth 1 (nth 5 game-state)))))
      (when (collision? (bb-sprite (nth 2 game-state))
                        (mapcar (lambda (coords)
                                  (cons (cons (+ (car coords) (* cactus-width 0.25))
                                              (cdr coords))
                                        (cons (+ (car coords) (* cactus-width 0.75))
                                              (+ (cdr coords) cactus-height))))
                                (nth 0 (nth 5 game-state))))
        (setf (nth 6 game-state) :game-over)
        (message "GAME OVER")))
    (cl-incf (car game-state))))

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
