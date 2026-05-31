;;; background-test.el --- Tests for backgrounds -*- lexical-binding: t -*-

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

;; Tests for backgrounds, in particular for clipping a background against
;; the canvas when it is positioned partly (or wholly) outside of it.

;;; Code:

(require 'retro)
(require 'test-helper (expand-file-name "./test-helper.el") t)

;; A 2x2 tile.  Once loaded as a background it is duplicated horizontally
;; (seamless scrolling), so the double-width source becomes:
;;   row 0: red green red green
;;   row 1: blue yellow blue yellow
;; A clip-width of 2 at clip-x 0 shows the original tile.
(defvar background-2x2 '("2 2 #000000"
                         "#ff0000 #00ff00"
                         "#0000ff #ffff00"))

(defmacro with-background-2x2 (var clip-width clip-x x y &rest body)
  "Bind VAR to a 2x2 background and evaluate BODY."
  (declare (indent 5))
  `(with-content-file background-file background-2x2
     (let ((,var (retro--load-background background-file ,clip-width ,clip-x ,x ,y)))
       ,@body)))

(ert-deftest background-test-can-be-loaded ()
  (with-background-2x2 bg 2 0 0 0
    (should (retro-background-p bg))
    (should (equal 4 (retro-background-width bg)))    ; doubled
    (should (equal 2 (retro-background-height bg)))
    (should (equal 2 (retro-background-clip-width bg)))))

(ert-deftest background-test-plot-within-canvas ()
  "A background fully inside the canvas is plotted unchanged (regression)."
  (let ((red (retro--add-color-to-palette "#ff0000"))
        (green (retro--add-color-to-palette "#00ff00"))
        (blue (retro--add-color-to-palette "#0000ff"))
        (yellow (retro--add-color-to-palette "#ffff00")))
    (with-background-2x2 bg 2 0 0 0
      (let* ((canvas (retro-canvas-create :margin-left 0 :margin-top 0
                                          :width 3 :height 3 :background-color 0))
             (cpxs (retro-canvas-pixels canvas)))
        ;; r g .
        ;; b y .
        ;; . . .
        (retro--plot-background bg canvas)
        (should (equal red    (aref cpxs 0)))
        (should (equal green  (aref cpxs 1)))
        (should (equal 0      (aref cpxs 2)))
        (should (equal blue   (aref cpxs 3)))
        (should (equal yellow (aref cpxs 4)))
        (should (equal 0      (aref cpxs 5)))
        (should (equal 0      (aref cpxs 6)))
        (should (equal 0      (aref cpxs 7)))
        (should (equal 0      (aref cpxs 8)))))))

(ert-deftest background-test-plot-taller-than-canvas-does-not-error ()
  "A background taller than the canvas must clip, not write out of bounds."
  (let ((red (retro--add-color-to-palette "#ff0000"))
        (green (retro--add-color-to-palette "#00ff00")))
    (with-background-2x2 bg 2 0 0 0
      ;; canvas is only 1 row tall, background is 2 rows tall at y=0:
      ;; the second source row would land on canvas index 2 (length is 2).
      (let* ((canvas (retro-canvas-create :margin-left 0 :margin-top 0
                                          :width 2 :height 1 :background-color 0))
             (cpxs (retro-canvas-pixels canvas)))
        (should-not (condition-case err
                        (progn (retro--plot-background bg canvas) nil)
                      (error err)))
        ;; the visible (first) row is plotted
        (should (equal red   (aref cpxs 0)))
        (should (equal green (aref cpxs 1)))))))

(ert-deftest background-test-plot-wider-than-canvas-does-not-error ()
  "A clip wider than the canvas must clip, not write out of bounds."
  (let ((red (retro--add-color-to-palette "#ff0000"))
        (blue (retro--add-color-to-palette "#0000ff")))
    (with-background-2x2 bg 2 0 0 0
      ;; canvas is only 1 column wide, clip is 2 wide at x=0:
      ;; the second source column would land on canvas index 1 of a 1-wide row.
      (let* ((canvas (retro-canvas-create :margin-left 0 :margin-top 0
                                          :width 1 :height 2 :background-color 0))
             (cpxs (retro-canvas-pixels canvas)))
        (should-not (condition-case err
                        (progn (retro--plot-background bg canvas) nil)
                      (error err)))
        (should (equal red  (aref cpxs 0)))
        (should (equal blue (aref cpxs 1)))))))

(ert-deftest background-test-plot-clips-right-edge ()
  "A background past the right edge keeps only its visible left column."
  (let ((red (retro--add-color-to-palette "#ff0000"))
        (blue (retro--add-color-to-palette "#0000ff")))
    (with-background-2x2 bg 2 0 0 0
      (let* ((canvas (retro-canvas-create :margin-left 0 :margin-top 0
                                          :width 3 :height 3 :background-color 0))
             (cpxs (retro-canvas-pixels canvas)))
        ;; place the background so its left edge is on the last column (x=2):
        ;; only column 0 of the tile (red/blue) is visible, at canvas x=2.
        (setf (retro-background-x bg) 2)
        (retro--plot-background bg canvas)
        (should (equal red  (aref cpxs 2)))   ; (x=2, y=0)
        (should (equal blue (aref cpxs 5)))   ; (x=2, y=1)
        (should (equal 0 (aref cpxs 0)))
        (should (equal 0 (aref cpxs 1)))
        ;; the clipped-off column must NOT wrap into the next row
        (should (equal 0 (aref cpxs 3)))
        (should (equal 0 (aref cpxs 6)))))))

(ert-deftest background-test-plot-clips-left-edge ()
  "A background partly off the left edge starts from a deeper source column."
  (let ((green (retro--add-color-to-palette "#00ff00"))
        (yellow (retro--add-color-to-palette "#ffff00")))
    (with-background-2x2 bg 2 0 0 0
      (let* ((canvas (retro-canvas-create :margin-left 0 :margin-top 0
                                          :width 3 :height 3 :background-color 0))
             (cpxs (retro-canvas-pixels canvas)))
        ;; left edge at x=-1: tile column 0 is off screen, column 1
        ;; (green/yellow) is the first visible one, at canvas x=0.
        (setf (retro-background-x bg) -1)
        (retro--plot-background bg canvas)
        (should (equal green  (aref cpxs 0)))  ; (x=0, y=0)
        (should (equal yellow (aref cpxs 3)))  ; (x=0, y=1)
        (should (equal 0 (aref cpxs 1)))))))

(provide 'background-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; background-test.el ends here
