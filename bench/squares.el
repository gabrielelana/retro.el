;;; squares.el --- benchmark based on square tiles -*- lexical-binding: t -*-

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

;; benchmark time taken to render various kind of tiles

;;; Code:

(require 'retro (expand-file-name "./../retro.el") t)
(require 'retro (expand-file-name "./retro.el") t)
(require 'cl-lib)

(defvar TILE-COLORS
  '("#800000" "#8B0000" "#A52A2A" "#B22222" "#DC143C" "#FF0000"
    "#FF6347" "#FF7F50" "#CD5C5C" "#F08080" "#E9967A" "#FA8072"
    "#FFA07A" "#FF4500" "#FF8C00" "#FFA500" "#FFD700" "#B8860B"
    "#DAA520" "#EEE8AA" "#BDB76B" "#F0E68C" "#808000" "#FFFF00"
    "#9ACD32" "#556B2F" "#6B8E23" "#7CFC00" "#7FFF00" "#ADFF2F"
    "#006400" "#008000" "#228B22" "#00FF00" "#32CD32" "#90EE90"
    "#98FB98" "#8FBC8F" "#00FA9A" "#00FF7F" "#2E8B57" "#66CDAA"
    "#3CB371" "#20B2AA" "#2F4F4F" "#008080" "#008B8B" "#00FFFF"
    "#00FFFF" "#E0FFFF" "#00CED1" "#40E0D0" "#48D1CC" "#AFEEEE"
    "#7FFFD4" "#B0E0E6" "#5F9EA0" "#4682B4" "#6495ED" "#00BFFF"
    "#1E90FF" "#ADD8E6" "#87CEEB" "#87CEFA" "#191970" "#000080"
    "#00008B" "#0000CD" "#0000FF" "#4169E1" "#8A2BE2" "#4B0082"
    "#483D8B" "#6A5ACD" "#7B68EE" "#9370DB" "#8B008B" "#9400D3"
    "#9932CC" "#BA55D3" "#800080" "#D8BFD8" "#DDA0DD" "#EE82EE"
    "#FF00FF" "#DA70D6" "#C71585" "#DB7093" "#FF1493" "#FF69B4"
    "#FFB6C1" "#FFC0CB" "#FAEBD7" "#F5F5DC" "#FFE4C4" "#FFEBCD"
    "#F5DEB3" "#FFF8DC" "#FFFACD" "#FAFAD2" "#FFFFE0" "#8B4513"
    "#A0522D" "#D2691E" "#CD853F" "#F4A460" "#DEB887" "#D2B48C"
    "#BC8F8F" "#FFE4B5" "#FFDEAD" "#FFDAB9" "#FFE4E1" "#FFF0F5"
    "#FAF0E6" "#FDF5E6" "#FFEFD5" "#FFF5EE" "#F5FFFA" "#708090"
    "#778899" "#B0C4DE" "#E6E6FA" "#FFFAF0" "#F0F8FF" "#F8F8FF"
    "#F0FFF0" "#FFFFF0" "#F0FFFF" "#FFFAFA" "#000000" "#696969"
    "#808080" "#A9A9A9" "#C0C0C0" "#D3D3D3" "#DCDCDC" "#F5F5F5"
    "#FFFFFF")
  "Colors to choose from for the tiles.")

(defun pick-random-color ()
  "Pick random color out of TILE-COLORS."
  (nth (random (length TILE-COLORS)) TILE-COLORS))

(defun clamp (x lo hi)
  "Clamp value X between LO and HI (inclusive)."
  (max lo (min x hi)))

(defmacro clock-it (&rest body)
  "Measure the time it takes to evaluate BODY.

Return time taken in milliseconds."
  (declare (indent defun))
  `(let ((start-at (current-time)))
     ,@body
     (* 1000 (float-time (time-since start-at)))))

(defun mean (list)
  "Return the mean of the elements in LIST."
  (/ (cl-loop for x in list
              sum x)
     (float (length list))))

(defun std-dev (list)
  "Return the standard deviation of the elements in LIST."
  (sqrt (/ (cl-loop for x in list
                    sum (expt (- x (mean list)) 2))
           (float (1- (length list))))))

(defun benchrmak-buffer-render (resolution background-color frames-to-render draw-canvas)
  "Benchmark time taken to render buffer updated with DRAW-CANVAS function.

RESOLUTION is the resolution of the canvas as `(cons width height).
BACKGROUND-COLOR is the default background color of the canvas.
FRAMES-TO-RENDER are the number of frames to render in the bechmark."
  (let ((buffer-name "*benchmark-buffer-render*")
         (current-window (selected-window))
         (frame-counter 0)
         (laps nil)
         current-canvas
         previous-canvas)
    (save-window-excursion
      (with-current-buffer (get-buffer-create buffer-name)
        (setq current-canvas (retro--init-canvas current-window
                                                 buffer-name
                                                 (car resolution)
                                                 (cdr resolution)
                                                 background-color)
              previous-canvas (retro-canvas-copy current-canvas))
        (while (< frame-counter frames-to-render)
          (funcall draw-canvas current-canvas)
          (setq laps (cons
                      (clock-it
                        (retro--buffer-render current-canvas previous-canvas)
                        (retro--reset-canvas previous-canvas)
                        (cl-rotatef current-canvas previous-canvas))
                      laps))
          (cl-incf frame-counter)
          (sit-for 0))))
    (kill-buffer buffer-name)
    (list
     (cons 'mean (mean laps))
     (cons 'std-dev (std-dev laps))
     (cons 'fps (round (/ 1 (/ (mean laps) 1000)))))))

(defun single-square ()
  "Benchmark of single square, with single color."
  (let ((resolution (cons 320 240))
        (background-color (ht-get retro-palette-colors->index "#000000"))
        (square-color (ht-get retro-palette-colors->index "#ff0000"))
        (frames-to-render 100))
    (benchrmak-buffer-render
     resolution
     background-color
     frames-to-render
     (lambda (current-canvas)
       (retro--plot-filled-rectangle 0 0 (1- (car resolution)) (1- (cdr resolution)) square-color current-canvas)))))

(defun single-square-multiple-colors ()
  "Benchmark of single square, change color at each frame."
  (let ((resolution (cons 320 240))
        (background-color (retro-palette-color-index "#000000"))
        (frames-to-render 100)
        square-color)
    (benchrmak-buffer-render
     resolution
     background-color
     frames-to-render
     (lambda (current-canvas)
       (setq square-color (retro-palette-color-index (pick-random-color)))
       (retro--plot-filled-rectangle 0 0 (1- (car resolution)) (1- (cdr resolution)) square-color current-canvas)))))

(defun multiple-squares-multiple-colors (&optional square-side)
  "Benchmark of multiple square, random color each frame.

SQUARE-SIDE is the side of the square, must be a divider of
resolution. Default 80."
  (setq square-side (or square-side 80))
  (let* ((resolution (cons 320 240))
         (background-color (retro-palette-color-index "#000000"))
         (frames-to-render 100)
         (square-count-x (/ (car resolution) square-side))
         (square-count-y (/ (cdr resolution) square-side))
         square-x
         square-y
         square-color)
    (benchrmak-buffer-render
     resolution
     background-color
     frames-to-render
     (lambda (current-canvas)
       (dotimes (y square-count-y)
         (dotimes (x square-count-x)
           (setq
            square-x (* x square-side)
            square-y (* y square-side)
            square-color (retro-palette-color-index (pick-random-color)))
           (retro--plot-filled-rectangle
            square-x
            square-y
            (1- (+ square-x square-side))
            (1- (+ square-y square-side))
            square-color
            current-canvas)))))))

(defun multiple-shifting-squares-multiple-colors (&optional square-side)
  "Benchmark of multiple square, shifting with a random color each frame.

SQUARE-SIDE is the side of the square, must be a divider of
resolution. Default 80."
  (setq square-side (or square-side 80))
  (let* ((resolution (cons 320 240))
         (background-color (retro-palette-color-index "#000000"))
         (frames-to-render 100)
         (square-count-x (1+ (/ (car resolution) square-side)))
         (square-count-y (1+ (/ (cdr resolution) square-side)))
         (shift-x 0)
         (shift-y 0)
         (canvas-min-x 0)
         (canvas-max-x (1- (car resolution)))
         (canvas-min-y 0)
         (canvas-max-y (1- (cdr resolution)))
         (clamp-x (lambda (x) (clamp x canvas-min-x canvas-max-x)))
         (clamp-y (lambda (y) (clamp y canvas-min-y canvas-max-y)))
         square-x
         square-y
         square-color)
    (benchrmak-buffer-render
     resolution
     background-color
     frames-to-render
     (lambda (current-canvas)
       (dotimes (y square-count-y)
         (dotimes (x square-count-x)
           (setq
            square-x (- (* x square-side) shift-x)
            square-y (- (* y square-side) shift-y)
            square-color (retro-palette-color-index (pick-random-color)))
           (retro--plot-filled-rectangle
            (funcall clamp-x square-x)
            (funcall clamp-y square-y)
            (funcall clamp-x (+ square-x square-side))
            (funcall clamp-y (+ square-y square-side))
            square-color
            current-canvas)
           ))
       (setq shift-x (mod (1+ shift-x) square-side)
             shift-y (mod (1+ shift-y) square-side))))))

;;; TODO: compare results with best, print results table with colored diff

(defun benchmark-run-all (benchmarks)
  "Run all BENCHMARKS."
  (let* ((current-directory (file-name-directory (buffer-file-name)))
         (results-file-name (expand-file-name "./squares.results.last" current-directory))
         (results-buffer (get-buffer-create results-file-name))
         results)
    (dolist (b benchmarks)
      (message "%s %s" (car b) (cdr b))
      (setq results (cons (cons (car b) (apply (car b) (cdr b))) results)))
    (with-current-buffer results-buffer
      (insert (format "%s" results))
      (write-file results-file-name))
    (kill-buffer results-buffer)
    results))

(benchmark-run-all
 '((single-square)
   (single-square-multiple-colors)
   (multiple-squares-multiple-colors)
   (multiple-squares-multiple-colors 40)
   (multiple-squares-multiple-colors 20)
   (multiple-squares-multiple-colors 10)
   (multiple-squares-multiple-colors 5)
   (multiple-shifting-squares-multiple-colors)
   (multiple-shifting-squares-multiple-colors 40)
   (multiple-shifting-squares-multiple-colors 20)
   (multiple-shifting-squares-multiple-colors 10)
   (multiple-shifting-squares-multiple-colors 5)))

(provide 'squares)

;; Local Variables:
;; coding: utf-8
;; End:
;;; squares.el ends here
