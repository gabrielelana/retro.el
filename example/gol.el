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

;;; Code:

(require 'ert)
(require 'ht)

(defun alive-p (cell) (eq 'alive cell))
(defun dead-p (cell) (eq 'dead cell))

(defun evolve (cell neighbors)
  "Evolve a CELL given its NEIGHBORS.

Any live cell with fewer than two live neighbours dies, as if by
underpopulation.

Any live cell with two or three live neighbours lives on to the
next generation.

Any live cell with more than three live neighbours dies, as if by
overpopulation.

Any dead cell with exactly three live neighbours becomes a live
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

(defun universe-get (universe xy)
  (ht-get universe xy 'dead))

(defun universe-alives-coordinates (universe)
  (seq-filter (lambda (xy) (alive-p (universe-get universe xy))) (ht-keys universe)))

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

(defun universe-neighbors-cells (universe xy)
  (mapcar (apply-partially 'universe-get universe)
          (neighbors-coordinates xy)))

;;; TODO: rewrite with cl-loop
(defun universe-evolve (universe)
  (let ((next (universe-create)))
    (dolist (alive-xy (universe-alives-coordinates universe))
      (universe-set! next alive-xy (evolve 'alive
                                           (universe-neighbors-cells universe alive-xy)))
      (dolist (neighbor-xy (neighbors-coordinates alive-xy))
        (universe-set! next neighbor-xy (evolve (universe-get universe neighbor-xy)
                                                (universe-neighbors-cells universe neighbor-xy)))))
    next))

;;; Graphics

(require 'retro)

(defun update-every (seconds update)
  (let ((since-last-update 0.0))
    (lambda (elapsed game-state canvas)
      (setq since-last-update (+ since-last-update elapsed))
      (when (> since-last-update seconds)
        (funcall update since-last-update game-state canvas)
        (setq since-last-update 0.0)))))

(defun gol-show-update (_elapsed game-state _canvas)
  (garbage-collect)
  (when (not (nth 1 game-state))
    (cl-incf (nth 2 game-state))
    (setf (car game-state) (universe-evolve (car game-state)))))

(defun gol-show-render (game-state canvas)
  (let ((pixels (retro-canvas-pixels canvas))
        (width (retro-canvas-width canvas))
        (height (retro-canvas-height canvas))
        (color (retro--add-color-to-palette "#ffffff"))
        (x 0)
        (y 0))
    (message "[%d] %s" (nth 2 game-state) (if (nth 1 game-state) "pause" "play"))
    (dolist (xy (universe-alives-coordinates (car game-state)))
      (setq x (car xy) y (- height (cdr xy) 1))
      (when (and (>= x 0)
                 (>= y 0)
                 (< x width)
                 (< y height))
        (retro--plot-pixel x y color pixels width)))))

(defun gol-show-init ()
  (let ((u (universe-create)))
    ;; (load-pattern-at u 20 35 "./glider-duplicator.cells")
    ;; (load-pattern-at u 60 60  "./ant-stretcher.cells")
    ;; (load-pattern-at u 30 40  "./b52.cells")
    ;; (load-pattern-at u 35 25  "./fanout.cells")
    (load-pattern-at u 30 40 "./against-the-grain.cells")
    ;; (universe-set! u (cons 2 2) 'alive)
    ;; (universe-set! u (cons 2 3) 'alive)
    ;; (universe-set! u (cons 2 4) 'alive)
    (list
     u                                  ; universe
     nil                                ; pause
     0                                  ; generation number
     )))

(defun load-pattern-at (universe x y file-path)
  (let ((current-char nil)
        (xi x)
        (yi y))
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
        (forward-char 1))))
  universe)

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
                                         (setf (nth 1 game-state) (not (nth 1 game-state)))))
                                ("n" . (lambda (game-state _)
                                         (when (nth 1 game-state)
                                           (cl-incf (nth 2 game-state))
                                           (setf (car game-state) (universe-evolve (car game-state))))))
                                ("<down-mouse-1>" . (lambda (game-state _ xy)
                                                      (when (nth 1 game-state)
                                                        (let ((u (car game-state))
                                                              (x (car xy))
                                                              (y (- ,height (cdr xy) 1)))
                                                          (universe-set! u
                                                                         (cons x y)
                                                                         (if (alive-p (universe-get u (cons x y))) 'dead 'alive))))))
                                ("q" . retro--handle-quit))
                        :init 'gol-show-init
                        :update (update-every 0.1 'gol-show-update)
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
