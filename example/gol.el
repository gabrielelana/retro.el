;;; gol.el --- Game of Life -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Game of Life
;;
;; ASCII patterns of interesting configurations are taken from:
;; - https://www.conwaylife.com/ref/lexicon/lex.htm
;; - https://www.conwaylife.com/patterns

;;; Code:

(require 'ert)
(require 'ht)

(defun alive-p (cell) (eq 'alive cell))
(defun dead-p (cell) (eq 'dead cell))

(defun evolve (cell neighbors)
  "Evolve a CELL given its NEIGHBORS.

Any live cell with fewer than two live neighbors dies, as if by
underpopulation.

Any live cell with two or three live neighbors lives on to the
next generation.

Any live cell with more than three live neighbors dies, as if by
overpopulation.

Any dead cell with exactly three live neighbors becomes a live
cell, as if by reproduction.
"
  (let ((alive-neighbors (seq-length (seq-filter (apply-partially 'eq 'alive) neighbors))))
    (cond ((and (alive-p cell) (< alive-neighbors 2)) 'dead)
          ((and (alive-p cell) (> alive-neighbors 3)) 'dead)
          ((and (dead-p cell) (= alive-neighbors 3)) 'alive)
          (t cell))))

(defun universe-create ()
  (ht-create))

(defun universe-set! (universe xy cell)
  (ht-set! universe xy cell))

(defun universe-reset! (universe)
  (ht-clear! universe))

(defun universe-get (universe xy)
  (ht-get universe xy 'dead))

(defun universe-evolve (universe)
  (let ((next (universe-create))
        (cells (ht-create)))
    (cl-loop for alive in (universe-alives-coordinates universe) do
             (ht-set! cells alive t)
             (cl-loop for neighbour in (neighbors-coordinates alive) do
                      (ht-set! cells neighbour t)))
    (cl-loop for cell in (ht-keys cells) do
             (universe-set! next cell (evolve (universe-get universe cell)
                                              (universe-neighbors-cells universe cell))))
    next))

(defun universe-alives-coordinates (universe)
  (seq-filter (lambda (xy) (alive-p (universe-get universe xy))) (ht-keys universe)))

(defun universe-neighbors-cells (universe xy)
  (mapcar (apply-partially 'universe-get universe)
          (neighbors-coordinates xy)))

(defun neighbors-coordinates (xy)
  (list
   (cons (1- (car xy)) (1- (cdr xy)))
   (cons (    car xy)  (1- (cdr xy)))
   (cons (1+ (car xy)) (1- (cdr xy)))
   (cons (1- (car xy))     (cdr xy))
   (cons (1+ (car xy))     (cdr xy))
   (cons (1- (car xy)) (1+ (cdr xy)))
   (cons     (car xy)  (1+ (cdr xy)))
   (cons (1+ (car xy)) (1+ (cdr xy)))))


;;; Graphics

(require 'cl-lib)
(require 'retro)

(cl-defstruct (gol (:constructor gol--create)
                   (:copier nil))
  "Game of life game state."
  (universe nil :type ht)
  (pause-p nil :type boolean)
  (generation 0 :type number)
  (patterns nil :type list)
  (pattern-index 0 :type number))

(defun gol-create (patterns)
  (gol--create :universe (universe-create)
               :pause-p nil
               :generation 0
               :patterns patterns
               :pattern-index 0))

(defun gol-show-update (_elapsed game-state _canvas)
  (when (not (gol-pause-p game-state))
    (cl-incf (gol-generation game-state))
    (let ((current-pattern (gol-current-pattern game-state)))
      (if (> (nth 3 current-pattern) (gol-generation game-state))
          (setf (gol-universe game-state) (universe-evolve (gol-universe game-state)))
        (gol-load-next-pattern game-state)))))

(defun gol-show-render (_elapsed game-state canvas)
  (let ((pixels (retro-canvas-pixels canvas))
        (width (retro-canvas-width canvas))
        (height (retro-canvas-height canvas))
        (color (retro--add-color-to-palette "#ffffff"))
        (x 0)
        (y 0))
    (message "[%s:%s] %s"
             (file-name-base (nth 2 (gol-current-pattern game-state)))
             (gol-generation game-state)
             (if (gol-pause-p game-state) "pause" "play"))
    (dolist (xy (universe-alives-coordinates (gol-universe game-state)))
      (setq x (car xy) y (- height (cdr xy) 1))
      (when (and (>= x 0)
                 (>= y 0)
                 (< x width)
                 (< y height))
        (retro--plot-pixel x y color pixels width)))))

(defun gol-show-init ()
  (let ((game-state (gol-create '((20 35 "./asset/glider-duplicator.cells" 300)
                                  (60 60  "./asset/ant-stretcher.cells" 250)
                                  (30 40  "./asset/b52.cells" 500)
                                  (35 25  "./asset/fanout.cells" 500)
                                  (30 40 "./asset/against-the-grain.cells" 100)))))
    (gol-load-current-pattern game-state)
    game-state))

(defun gol-current-pattern (game-state)
  "Current pattern in universe."
  (nth (gol-pattern-index game-state) (gol-patterns game-state)))

(defun gol-load-next-pattern (game-state)
  "Load next pattern in universe."
  (setf (gol-generation game-state) 0
        (gol-pattern-index game-state) (% (1+ (gol-pattern-index game-state)) (seq-length (gol-patterns game-state))))
  (gol-load-current-pattern game-state))

(defun gol-load-current-pattern (game-state)
  "Load current pattern in universe."
  (let ((current-pattern (nth (gol-pattern-index game-state) (gol-patterns game-state)))
        (universe (gol-universe game-state))
        (current-char nil))
    (universe-reset! universe)
    (pcase-let ((`(,x ,y ,file-path) current-pattern))
      (let ((xi x) (yi y))
          (with-temp-buffer
            (insert-file-contents file-path)
            (goto-char (point-min))
            (while (not (eobp))
              (setq current-char (char-after))
              (cond ((eq ?O current-char)
                     (universe-set! universe (cons xi yi) 'alive)
                     (setq xi (1+ xi)))
                    ((eq ?\n current-char)
                     (setq yi (1+ yi)
                           xi x))
                    (t (setq xi (1+ xi))))
              (forward-char 1)))))))

(defun gol-show ()
  "Show game of life"
  (interactive)
  (let ((width 100)
        (height 100))
    (retro--game-loop
     (retro-game-create :name "game-of-life"
                        :resolution (cons width height)
                        :background-color (retro--add-color-to-palette "#000000")
                        :bind `(("p" . (lambda (game-state _)
                                         (setf (gol-pause-p game-state) (not (gol-pause-p game-state)))))
                                ("n" . (lambda (game-state _)
                                         (when (gol-pause-p game-state)
                                           (cl-incf (gol-generation game-state))
                                           (setf (gol-universe game-state) (universe-evolve (gol-universe game-state))))))
                                ("<down-mouse-1>" . (lambda (game-state _ click-xy)
                                                      (when (gol-pause-p game-state)
                                                        (let ((cell-x (car click-xy))
                                                              (cell-y (- ,height (cdr click-xy) 1)))
                                                          (universe-set! (gol-universe game-state)
                                                                         (cons cell-x cell-y)
                                                                         (if (alive-p (universe-get (gol-universe game-state) (cons cell-x cell-y)))
                                                                             'dead
                                                                           'alive))))))
                                ("q" . retro--handle-quit))
                        :init 'gol-show-init
                        :update (retro--update-every 0.1 'gol-show-update)
                        :render 'gol-show-render))))


;;; Tests

(ert-deftest gol-evolve ()
  (should (eq 'alive (evolve 'alive '(alive alive))))
  (should (eq 'alive (evolve 'alive '(alive alive alive))))
  (should (eq 'dead (evolve 'alive '(alive alive alive alive))))
  (should (eq 'dead (evolve 'alive '(alive))))
  (should (eq 'dead (evolve 'dead '(alive))))
  (should (eq 'dead (evolve 'dead '(alive alive))))
  (should (eq 'alive (evolve 'dead '(alive alive alive)))))

(ert-deftest gol-universe-contains-cells ()
  (let ((u (universe-create)))
    (universe-set! u (cons 0 0) 'alive)
    (should (equal 'alive (universe-get u (cons 0 0))))
    (should (equal 'dead (universe-get u (cons 1 1))))))

(ert-deftest gol-universe-evolve ()
  (let ((u1 (universe-create)))
    (universe-set! u1 (cons 1 0) 'alive)
    (universe-set! u1 (cons 1 1) 'alive)
    (universe-set! u1 (cons 1 2) 'alive)
    (let ((u2 (universe-evolve u1)))
      (should (equal 'dead (universe-get u2 (cons 1 0))))
      (should (equal 'dead (universe-get u2 (cons 1 2))))
      (should (equal 'alive (universe-get u2 (cons 0 1))))
      (should (equal 'alive (universe-get u2 (cons 1 1))))
      (should (equal 'alive (universe-get u2 (cons 2 1)))))))

(provide 'gol)

;; Local Variables:
;; coding: utf-8
;; End:
;;; gol.el ends here
