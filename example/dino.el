;;; dino.el --- Porting of dino from Chrome to Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Porting of dino from Chrome to Emacs.

;;; Code:

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
(defconst *ASSET-DIRECTORY* (file-name-as-directory (concat (file-name-directory (buffer-file-name)) "asset")))

;;; ============================================================================
;;; T-Rex

(defconst *T-REX-X* 10)
(defconst *T-REX-GROUND-Y* 101)
(defconst *T-REX-TOP-Y* 40)

(defvar t-rex-running-tween (tween-distinct-until-changed (tween-loop (tween 0.3 0 1 'linear))))
(defvar t-rex-hit-tween (tween-distinct-until-changed (tween-loop (tween 0.5 0 1 'linear))))
(defvar t-rex-current-tween t-rex-running-tween)
(defvar t-rex-current-play "running")

(defun t-rex-jump (sprite)
  (when (not (equal "jumping" t-rex-current-play))
    (setq t-rex-current-play "jumping"
          t-rex-current-tween (tween-concat (tween 0.3 *T-REX-GROUND-Y* *T-REX-TOP-Y* 'ease-out-cubic)
                                            (tween 0.3 *T-REX-TOP-Y* *T-REX-GROUND-Y* 'ease-in-cubic)))
    (retro--play-sprite sprite "jumping")))

(defun t-rex-update (sprite elapsed)
  "Update state of SPRITE t-rex after ELAPSED."
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
     ((equal "hit" t-rex-current-play)
      (when (funcall t-rex-current-tween elapsed)
        (retro--next-frame-sprite sprite)))
     (t (error "Unreachable")))))

(defun t-rex-render (sprite canvas)
  (retro--plot-sprite sprite canvas))

;;; ============================================================================
;;; Tiles

(defun dino--load-tiles ()
  "Load game tiles.

TILE (list tile-instance (min-y . gap-y) tile-width)"
  (let ((cloud (retro--load-tile (asset "dino-cloud.sprite") 0 0))
        (cactus-single-big (retro--load-tile (asset "dino-cactus-single-big.sprite") 0 0)))
    (ht (:cloud (list cloud *CLOUD-Y* (retro-tile-width cloud)))
        (:cactus-single-big (list cactus-single-big *CACTUS-Y* (retro-tile-width cactus-single-big))))))

(defun dino--spawn-initial-tiles (tiles tiles-count tile-x-gap tile-kinds)
  "Spawn initial TILES-COUNT tiles from TILES of TILE-KINDS."
  (dino--spawn-tiles
   tiles
   (list (dino--spawn-first-tile tiles (nth (random (seq-length tile-kinds)) tile-kinds)))
   (1- tiles-count)
   tile-x-gap
   tile-kinds))

(defun dino--spawn-first-tile (tiles tile-kind)
  "Spawn the first tile from TILES with TILE-KIND.

Spawn te first tile outside the visible screen"
  (dino--spawn-tile tiles (+ *WIDTH* 50) tile-kind))

(defun dino--spawn-tile (tiles tile-x tile-kind)
  "Spawn a tile from TILES with TILE-KIND at TILE-X."
  (let* ((tile (ht-get tiles tile-kind))
         (tile-y-min (car (nth 1 tile)))
         (tile-y-gap (cdr (nth 1 tile))))
    (cons tile-kind (cons tile-x (+ tile-y-min (random tile-y-gap))))))

(defun dino--spawn-tiles (tiles spawned-tiles tiles-count tile-x-gap tile-kinds)
  "Spawn one or more TILES if SPAWNED-TILES are less than TILES-COUNT.

SPAWNED-TILES is (list (cons tile-kind tile-coordinates))

TILE-X-GAP is (cons min-x-gap max-x-gap)
TILE-KINDS is the kind of tiles that can be spawn"
  (let ((last-tile (cdar (last spawned-tiles))) x)
    (while (< (length spawned-tiles) tiles-count)
      (setq x (+ (car last-tile) (car tile-x-gap) (random (cdr tile-x-gap)))
            tile-kind (nth (random (seq-length tile-kinds)) tile-kinds)
            new-tile (dino--spawn-tile tiles x tile-kind)
            spawned-tiles (reverse (cons new-tile (reverse spawned-tiles)))
            last-tile (cdr new-tile)))
    spawned-tiles))

(defun dino--render-tiles (tiles spawned-tiles canvas)
  "Render SPAWNED-TILES from TILES on CANVAS."
  (dolist (spawned-tile spawned-tiles)
    (let* ((tile-kind (car spawned-tile))
           (tile-coordindates (cdr spawned-tile))
           (tile (car (ht-get tiles tile-kind))))
      (setf (retro-tile-x tile) (car tile-coordindates)
            (retro-tile-y tile) (cdr tile-coordindates))
      (retro--plot-tile tile canvas))))

(defun dino--kill-tiles (tiles spawned-tiles)
  "Remove tiles in SPAWNED-TILES.

TILES is the collection of available tiles in the game"
  (let (tile-kind tile-coordinates tile-width)
    (seq-filter (lambda (spawned-tile)
                  (setq tile-kind (car spawned-tile)
                        tile-coordinates (cdr spawned-tile)
                        tile-width (nth 2 (ht-get tiles tile-kind)))
                  (> (+ (car tile-coordinates) tile-width) 0))
                spawned-tiles)))

(defun dino--update-tiles (tiles spawned-tiles tiles-count tile-x-gap tile-velocity tile-kinds elapsed)
  "Update tiles in SPAWNED-TILES after ELAPSED.

TILES is the collection of available tiles in the game
TILES-COUNT is the max number of tiles
TILE-X-GAP is (cons min-x-gap max-x-gap)
TILE-KINDS is the list of tile kinds that can be spawned"
  (setq spawned-tiles (dino--kill-tiles tiles spawned-tiles)
        spawned-tiles (dino--spawn-tiles tiles spawned-tiles tiles-count tile-x-gap tile-kinds)
        spawned-tiles (mapcar (lambda (spawned-tile)
                                (setf (cadr spawned-tile) (- (cadr spawned-tile) (round (* tile-velocity elapsed))))
                                spawned-tile)
                              spawned-tiles)))

;;; ============================================================================
;;; Clouds

(defconst *CLOUD-Y* '(10 . 100))
(defconst *CLOUD-GAP* '(100 . 300))
(defconst *CLOUD-VELOCITY* 100.0)  ; pixels per second
(defconst *CLOUD-MAX* 6)

;;; ============================================================================
;;; Cactus

(defconst *CACTUS-MAX* 3)
(defconst *CACTUS-GAP* '(180 . 320))
(defconst *CACTUS-Y* '(100 . 1))

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
  "Check intersection between bounding boxes BBL and BBR."
  (and (< (bb-left bbl) (bb-right bbr))
       (> (bb-right bbl) (bb-left bbr))
       (< (bb-top bbl) (bb-bottom bbr))
       (> (bb-bottom bbl) (bb-top bbr))))

(defun collision? (t-rex-bb tiles-bb)
  "Detect collision between T-REX-BB and TILES-BB."
  (->> tiles-bb
       (seq-filter (lambda (tile-bb) (<= (bb-left tile-bb) (bb-left t-rex-bb))))
       (seq-some (lambda (tile-bb) (bb-intersect? t-rex-bb tile-bb)))))

;;; ============================================================================

(defmacro game-over? (game-state)
  "Is game over in GAME-STATE."
  `(eq (nth 6 ,game-state) :game-over))

(defun game-over! (game-state)
  "Set GAME-STATE as game over."
  (setf (nth 6 game-state) :game-over)
  (setq t-rex-current-play "hit"
        t-rex-current-tween t-rex-hit-tween)
  (retro--play-sprite (nth 2 game-state) "hit")
  (message "GAME OVER"))

(defun game-reset! (game-state)
  "Reset GAME-STATE."
  (setq t-rex-current-play "running")
  (setq t-rex-current-tween t-rex-running-tween)
  (let ((initial-state (dino-init)))
    (setf
     (nth 0 game-state) (nth 0 initial-state)
     (nth 1 game-state) (nth 1 initial-state)
     (nth 2 game-state) (nth 2 initial-state)
     (nth 3 game-state) (nth 3 initial-state)
     (nth 4 game-state) (nth 4 initial-state)
     (nth 5 game-state) (nth 5 initial-state)
     (nth 6 game-state) (nth 6 initial-state)
     (nth 7 game-state) (nth 7 initial-state)
     )))

(defmacro asset (file-name)
  "Absolute file path of asset FILE-NAME."
  `(concat ,*ASSET-DIRECTORY* ,file-name))

(defun dino-init ()
  "Init game-state."
  (setq t-rex-current-play "running")
  (setq t-rex-current-tween t-rex-running-tween)
  (let ((tiles (dino--load-tiles)))
    (list 0                       ; 0
          (retro--load-background (asset "dino-horizon.sprite") *WIDTH* 0 0 (- *HEIGHT* 12 1)) ;1
          (retro--load-sprite (asset "dino-t-rex.sprite") *T-REX-X* *T-REX-GROUND-Y*) ;2
          (dino--spawn-initial-tiles tiles *CLOUD-MAX* *CLOUD-GAP* '(:cloud)) ; 3
          (dino--spawn-initial-tiles tiles *CACTUS-MAX* *CACTUS-GAP* '(:cactus-single-big)) ; 4
          (retro--load-font (asset "dino.font")) ; 5
          :playing                ; 6
          tiles      ; 7
          )))

(defun dino-update (elapsed game-state _canvas)
  "Update GAME-STATE after ELAPSED."
  (if (game-over? game-state)
      (progn
        (t-rex-update (nth 2 game-state) elapsed))
    (when (eq (% (nth 0 game-state) 100) 0)
      (message "[%03d] FPS: %f, elapsed: %fs" (nth 0 game-state) (/ 1.0 elapsed) elapsed))
    (retro--scroll-background (nth 1 game-state) (round (* *GROUND-VELOCITY* elapsed)))
    (t-rex-update (nth 2 game-state) elapsed)
    (setf (nth 3 game-state) (dino--update-tiles (nth 7 game-state) (nth 3 game-state) *CLOUD-MAX* *CLOUD-GAP* *CLOUD-VELOCITY* '(:cloud) elapsed))
    (setf (nth 4 game-state) (dino--update-tiles (nth 7 game-state) (nth 4 game-state) *CACTUS-MAX* *CACTUS-GAP* *GROUND-VELOCITY* '(:cactus-single-big) elapsed))
    ;; collision detection
    (when (collision? (bb-sprite (nth 2 game-state))
                      (mapcar (lambda (obstacle)
                                (let* ((obstacle-kind (car obstacle))
                                       (obstacle-coordinates (cdr obstacle))
                                       (obstacle (car (ht-get (nth 7 game-state) obstacle-kind))))
                                  (cons obstacle-coordinates
                                        (cons (+ (car obstacle-coordinates) (retro-tile-width obstacle))
                                              (+ (cdr obstacle-coordinates) (retro-tile-height obstacle))))))
                              (nth 4 game-state)))
      (game-over! game-state))
    (cl-incf (car game-state))))

(defun dino-render (_elapsed game-state canvas)
  "Render GAME-STATE on CANVAS."
  (retro--plot-background (nth 1 game-state) canvas)
  (dino--render-tiles (nth 7 game-state) (nth 3 game-state) canvas)
  (dino--render-tiles (nth 7 game-state) (nth 4 game-state) canvas)
  (retro--plot-string (nth 5 game-state)
                      (format *SCORE-TEXT* 0 (nth 0 game-state))
                      *SCORE-X*
                      *SCORE-Y*
                      2
                      canvas)
  (t-rex-render (nth 2 game-state) canvas)
  (when (game-over? game-state)
    (retro--plot-string (nth 5 game-state)
                        "GAME OVER"
                        200 50 10 canvas)))

(defun dino ()
  "Dino must avoid obstacles while running."
  (retro-game-create :name "dino"
                     :resolution (cons *WIDTH* *HEIGHT*)
                     :background-color (ht-get retro-palette-colors->index "#ffffff")
                     :bind `(("q" . retro--handle-quit)
                             ("SPC" . (lambda (game-state _)
                                        (cond
                                         ((equal "running" t-rex-current-play)
                                          (t-rex-jump (nth 2 game-state)))
                                         ((equal "hit" t-rex-current-play)
                                          (game-reset! game-state))))))
                     :init 'dino-init
                     :update 'dino-update
                     :render 'dino-render))

(defun dino-start ()
  "Start dino game interactively."
  (interactive)
  (retro--game-loop (dino)))

(provide 'dino)

;; Local Variables:
;; coding: utf-8
;; End:
;;; dino.el ends here
