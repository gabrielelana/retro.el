;;; retro.el --- Library to create retro games in Emacs -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((ht "2.0.0"))
;; Homepage: http://github.com/gabrielelana/retro.el
;; Keywords: retro gaming, game, library, graphics

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

;; Library to create retro games in Emacs.

;;; Code:

(require 'cl-lib)
(require 'ht)

(defgroup retro nil
  "Library to create retro games."
  :group 'games)

;;; TODO: documentation
(defconst retro-palette-size 65536)

;;; TODO: documentation
(defvar retro-palette-faces (make-vector retro-palette-size 0))

;;; TODO: documentation
(defvar retro-palette-pointer 0)

;;; TODO: documentation
(defvar retro-palette-colors->index (ht-create))

;;; TODO: check license of the font
;;; TODO: find a way to reliably distribute and install the font
(defvar retro-square-font-family "Kreative Square SM"
  "Font family used to create the illusion of pixels.")

;; (defvar retro-square-font-family "Topaz-8"
;   "Font family used to create the illusion of pixels.")


;;; Palette

(defun retro--add-color-to-palette (color)
  "Add COLOR to palette (if not already present) and return its index."
  (setq color (downcase color))
  (if (ht-contains-p retro-palette-colors->index color)
      (ht-get retro-palette-colors->index color)
    ;; check if retro-palette-pointer < retro-palette-size
    (let ((face-name (intern (format "retro-mode-face-%s" (substring color 1)))))
      (eval `(defface ,face-name
               '((t :background ,color))
               ,(format "Face for pixel with color %s" color)
               :group 'retro-mode))
      (aset retro-palette-faces retro-palette-pointer face-name)
      (ht-set! retro-palette-colors->index color retro-palette-pointer)
      (prog1
          retro-palette-pointer
        (setq retro-palette-pointer (1+ retro-palette-pointer))))))

;;; TODO: better initialization, when? eval-after-load?
(let* ((colors (list "#000000" "#800000" "#008000"
                     "#808000" "#000080" "#800080"
                     "#008080" "#c0c0c0" "#808080"
                     "#ff0000" "#00ff00" "#ffff00"
                     "#0000ff" "#ff00ff" "#00ffff"
                     "#ffffff")))
  (dolist (color colors)
    (retro--add-color-to-palette color)))


;;; Canvas

(cl-defstruct (retro-canvas (:constructor retro-canvas--create)
                            (:copier nil))
  "Canvas data structure."
  (margin-left 0 :type number)
  (margin-top 0 :type number)
  (width 0 :type number)
  (height 0 :type number)
  (background-color 0 :type number)
  (pixels nil :type hashmap)
  (buffer-before-length 0 :type number)
  (buffer-line-length 0 :type number))

(cl-defun retro-canvas-create (&key margin-left margin-top width height background-color)
  "Create CANVAS."
  (retro-canvas--create :margin-left margin-left
                        :margin-top margin-top
                        :width width
                        :height height
                        :background-color background-color
                        :pixels (make-vector (* width height) background-color)
                        :buffer-before-length (* (+ margin-left width 1) margin-top)
                        :buffer-line-length (+ margin-left width 1)))

(defun retro-canvas-copy (canvas)
  "Copy CANVAS, no memory is shared."
  (retro-canvas-create :margin-left (retro-canvas-margin-left canvas)
                       :margin-top (retro-canvas-margin-top canvas)
                       :width (retro-canvas-width canvas)
                       :height (retro-canvas-height canvas)
                       :background-color (retro-canvas-background-color canvas)))

(defun retro--reset-canvas (canvas)
  "Clean all CANVAS pixels."
  (fillarray (retro-canvas-pixels canvas) (retro-canvas-background-color canvas)))

(defun retro--calibrate-canvas-in-window (width height window)
  "Return optimal size of pixel in WINDOW for canvas WIDTH x HEIGHT.

We want to calculate the size in pixel of a single character
coming from 'retro-square-font-family (a pixel of our canvas) so
that we will minimize the margin of the canvas with the wanted
resolution in WINDOW."
  (let* ((min-pixel-size 1)
         (max-pixel-size 300)
         (current-pixel-size nil)
         (result nil)
         (stop nil))
    (while (not stop)
      (setq current-pixel-size (+ (/ (- max-pixel-size min-pixel-size) 2) min-pixel-size))
      (if (eq min-pixel-size max-pixel-size)
          (setq stop t)
        (with-temp-buffer
          (when display-line-numbers
            (display-line-numbers-mode -1))
          (set-window-buffer window (current-buffer))
          (face-remap-add-relative 'default :family retro-square-font-family :height current-pixel-size)
          (let* ((window-width (window-body-width window t))
                 (font-width (window-font-width window))
                 ;; window-mode-line-height lies with doom-modeline
                 (mode-line-height (or (and (boundp 'doom-modeline-mode) doom-modeline-mode
                                            (boundp 'doom-modeline-height) doom-modeline-height)
                                       (window-mode-line-height window)))
                 (window-height (- (window-body-height window t) (window-header-line-height window) mode-line-height))
                 (font-height (window-font-height window))
                 (n-columns (/ window-width font-width))
                 (n-lines (floor (/ window-height font-height)))
                 (waste (+ (- n-columns width) (- n-lines height))))
            (if (or (< n-columns width) (< n-lines height))
                ;; current-pixel-size is too big
                (setq max-pixel-size current-pixel-size)
              ;; current-pixel-size is ok
              (if (or (not (car result)) (< waste (car result)))
                  ;; we did improve
                  (setq min-pixel-size current-pixel-size
                        result (list waste current-pixel-size n-columns n-lines))
                ;; we did not improve, bail
                (setq stop t))
              )))))
    (cdr result)))

(defun retro--init-canvas (window buffer-name canvas-width canvas-height background-color)
  "Create a buffer with BUFFER-NAME in WINDOW for the specified canvas.

CANVAS-WIDTH and CANVAS-HEIGHT are respectively the wanted width
and height of the canvas in pixels.

PIXEL-SIZE is the size of the single pixel (TODO: clarify).

BACKGROUND-COLOR is the background color."
  (unless (window-valid-p window)
    (error "Window is not a window, cannot be used to initialize screen"))
  (let* ((calibration (retro--calibrate-canvas-in-window canvas-width canvas-height window))
         (pixel-size (nth 0 calibration))
         (window-width (nth 1 calibration))
         (window-height (nth 2 calibration)))
    (unless pixel-size
      (error "Window is not big enough for the required canvas resolution"))
    (with-current-buffer (get-buffer-create buffer-name)
      (set-window-buffer window (current-buffer))
      (special-mode)
      (when (boundp 'hl-line-mode) (setq-local hl-line-mode nil))
      (when (boundp 'global-hl-line-mode) (setq-local global-hl-line-mode nil))
      (when (boundp 'line-number-mode) (setq-local line-number-mode nil))
      (when (boundp 'column-number-mode) (setq-local column-number-mode nil))
      (setq-local visible-cursor nil
                  cursor-type nil
                  inhibit-modification-hooks t
                  inhibit-compacting-font-caches t
                  bidi-inhibit-bpa t
                  bidi-display-reordering nil
                  buffer-read-only nil
                  mode-line-format nil)
      (erase-buffer)
      (buffer-disable-undo)
      (jit-lock-mode nil)
      (font-lock-mode -1)
      (mouse-wheel-mode -1)
      (auto-save-mode -1)
      (goto-char (point-min))
      (face-remap-add-relative 'default :family retro-square-font-family :height pixel-size)
      (let* ((margin-top (/ (- window-height canvas-height) 2))
             (margin-left (/ (- window-width canvas-width) 2))
             (canvas (retro-canvas-create :margin-left margin-left
                                          :margin-top margin-top
                                          :width canvas-width
                                          :height canvas-height
                                          :background-color background-color))
             (margin-top-string (propertize (make-string (+ margin-left canvas-width) 32) 'face 'default))
             (margin-left-string (propertize (make-string margin-left 32) 'face 'default))
             (canvas-string (propertize (make-string canvas-width 32) 'face (aref retro-palette-faces background-color))))
        (dotimes (_ margin-top)
          (insert margin-top-string)
          (insert "\n"))
        (dotimes (_ canvas-height)
          (insert margin-left-string)
          (insert canvas-string)
          (insert "\n"))
        (setq-local buffer-read-only t)
        canvas))))

;;; TODO: inline
;;; TODO: unroll?
(defun retro--vector-clip-blit (sv sx0 sy0 sx1 sy1 sw dv dx dy dw tc)
  "Copy clip from source vector to destination vector.

- SV is the source vector.
- (SX0, SY0) coordinates of clip's top left corner in source
  vector.
- (SX1, SY1) coordinates of clip's bottom right corner in the
source vector. SW is the width of the source vector. DV is the
destination vector.
- (DX, DY) coordinates of clip's top left corner in the
destination vector. DW is the width of the destination vector.
- TC is the transparent color of the source vector, a pixel of
this color is not copied."
  (let* ((si (+ (* sy0 sw) sx0))        ; index in source vector
         (di (+ (* dy dw) dx))          ; index in destination vector
         (cw (1+ (- sx1 sx0)))          ; clip width
         (sg (- sw cw))                 ; gap in source vector
         (dg (- dw cw))                 ; gap in destination vector
         (sie (+ (* sy1 sw) sx1))       ; index end in source vector
         (sye (+ (1- cw) sx0))          ; last column (y) of the clip in source vector
         (px nil))
    (while (<= si sie)
      (setq px (aref sv si))
      (when (/= px tc)
        (aset dv di px))
      (if (= (% si sw) sye)
          (setq si (+ si sg 1)
                di (+ di dg 1))
        (setq si (1+ si)
              di (1+ di))))))

(defun retro--flip-h-vector (vector width height)
  "Flip vector of pixels on horizontal axis."
  (let* ((tl 0)
         (il 0)
         (ir (1- width)))
    (while (< tl height)
      (while (< il ir)
        (cl-rotatef (aref vector il) (aref vector ir))
        (setq il (1+ il)
              ir (1- ir)))
      (setq tl (1+ tl)
            il (* tl width)
            ir (+ il (1- width))))))


;;; Tile

(cl-defstruct (retro-tile (:constructor retro--tile-create)
                          (:copier nil))
  "Represent a tile in a game."
  (x 0 :type number)
  (y 0 :type number)
  (width 0 :type number)
  (height 0 :type number)
  (transparent-color 0 :type number)
  (pixels nil :type vector))

;;; TODO: check file-path exists
;;; TODO: check file format
;;; TODO: documentation
(defun retro--load-tile (file-path &optional x y)
  "Load a TILE from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let* ((header (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (tokens (split-string header))
           (width (string-to-number (nth 0 tokens)))
           (height (string-to-number (nth 1 tokens)))
           (transparent-color (nth 2 tokens))
           (current-line nil)
           (pixels (make-vector (* width height) 0))
           (i 0))
      (forward-line 1)
      (while (not (eobp))
        (setq current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (when (not (string-match-p "^\\s-*//" current-line))
          (dolist (color (split-string current-line))
            (aset pixels i (retro--add-color-to-palette color))
            (cl-incf i)))
        (forward-line 1))
      (retro--tile-create :x (or x 0)
                          :y (or y 0)
                          :width width
                          :height height
                          :transparent-color (retro--add-color-to-palette transparent-color)
                          :pixels pixels))))

(defun retro--move-tile (tile dx dy x0 y0 x1 y1)
  "Move tile"
  (let ((x (retro-tile-x tile))
        (y (retro-tile-y tile))
        (w (1- (retro-tile-width tile)))
        (h (1- (retro-tile-height tile))))
    (when (/= dx 0)
      (setf (retro-tile-x tile) (if (> dx 0)
                                    (min (+ x dx) (- x1 w))
                                  (max (+ x dx) x0))))
    (when (/= dy 0)
      (setf (retro-tile-y tile) (if (> dy 0)
                                    (min (+ y dy) (- y1 h))
                                  (max (+ y dy) y0))))))


(defun retro--flip-h-tile (tile)
  "Flip tile based on horizontal axis."
  (retro--flip-h-vector (retro-tile-pixels tile)
                        (retro-tile-width tile)
                        (retro-tile-height tile)))

(defun retro--plot-tile (tile canvas)
  "Plot a TILE on CANVAS."
  ;; TODO: duplicated in retro--plot-sprite
  (let* ((cw (retro-canvas-width canvas))
         (ch (retro-canvas-height canvas))
         (sw (retro-tile-width tile))
         (sh (retro-tile-height tile))
         (sx (retro-tile-x tile))
         (sy (retro-tile-y tile))
         ;; coordinates are relative to canvas
         (cl (cons 0 0))                ; canvas top left corner
         (cr (cons (1- cw) (1- ch)))    ; canvar right bottom corner
         (sl (cons sx sy))              ; tile top left corner
         (sr (cons (1- sw) (1- sh)))    ; tile right bottom corner
         ;; coordinates of the intersection
         (sx0 (max (car cl)
                   (car sl)))
         (sx1 (min (+ (car cl) (car cr))
                   (+ (car sl) (car sr))))
         (sy0 (max (cdr cl)
                   (cdr sl)))
         (sy1 (min (+ (cdr cl) (cdr cr))
                   (+ (cdr sl) (cdr sr)))))
    (when (and (<= sx0 sx1) (<= sy0 sy1))
      (retro--vector-clip-blit (retro-tile-pixels tile)
                               ;; convert sx0 sy0 sx1 sy1 to tile relative coordinates
                               (- sx0 sx)
                               (- sy0 sy)
                               (- sx1 sx)
                               (- sy1 sy)
                               sw
                               (retro-canvas-pixels canvas)
                               sx0
                               sy0
                               cw
                               (retro-tile-transparent-color tile)))))


;;; Background

(cl-defstruct (retro-background (:constructor retro--background-create)
                                (:copier nil))
  "Represent a background in a game."
  (x 0 :type number)
  (y 0 :type number)
  (width 0 :type number)
  (height 0 :type number)
  (clip-x 0 :type number)
  (clip-width 0 :type number)
  (transparent-color 0 :type number)
  (pixels nil :type vector))

;;; TODO: documentation
;;; TODO: check clip-x and clip-width (< clip-x tile-width) && (< clip-width tile-width)
;;; https://javl.github.io/image2cpp/
(defun retro--load-background (file-path clip-width &optional clip-x x y)
  "Load a BACKGROUND from FILE-PATH with a CLIP-WIDTH width."
  (prog1
      (let* ((tile (retro--load-tile file-path x y))
             (tpxs (retro-tile-pixels tile))
             (tw (retro-tile-width tile))
             (th (retro-tile-height tile))
             (tpx nil)
             (tl (* tw th))
             (bpxs (make-vector (* 2 tl) 0))
             (ti 0)
             (bi 0))
        (while (< ti tl)
          (setq tpx (aref tpxs ti))
          (aset bpxs bi tpx)
          (aset bpxs (+ bi tw) tpx)
          (setq ti (1+ ti)
                bi (1+ bi))
          (when (= (% ti tw) 0)
            (setq bi (+ bi tw))))
        (retro--background-create :x x
                                  :y y
                                  :width (* 2 tw)
                                  :height th
                                  :clip-x (or clip-x 0)
                                  :clip-width clip-width
                                  :transparent-color (retro-tile-transparent-color tile)
                                  :pixels bpxs))
    (garbage-collect)))

(defun retro--scroll-background (background dx)
  "Scroll BACKGROUND horizontally by DX."
  (setf (retro-background-clip-x background)
        (% (+ (retro-background-clip-x background) dx)
           (/ (retro-background-width background) 2))))

(defun retro--plot-background (background canvas)
  "Plot BACKGROUND on CANVAS."
  (retro--vector-clip-blit (retro-background-pixels background)
                           (retro-background-clip-x background)
                           0
                           (1- (+ (retro-background-clip-x background) (retro-background-clip-width background)))
                           (1- (retro-background-height background))
                           (retro-background-width background)
                           (retro-canvas-pixels canvas)
                           (retro-background-x background)
                           (retro-background-y background)
                           (retro-canvas-width canvas)
                           (retro-background-transparent-color background)))


;;; Font

;;; retro-glyph
(cl-defstruct (retro-glyph (:constructor retro--glyph-create)
                           (:copier nil))
  "Represent a single glyph in a bitmap font."
  (glyph 0 :type string)
  (width 0 :type number)
  (height 0 :type number)
  (transparent-color 0 :type number)
  (pixels nil :type vector))

;;; retro-font
(cl-defstruct (retro-font (:constructor retro--font-create)
                          (:copier nil))
  "Represent a bitmap font."
  (glyphs nil :type ht-p))

;;; Load font from file
(defun retro--load-font (file-path)
  "Load FONT from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (retro--skip-comments)
    (let ((font (retro--font-create :glyphs (ht-create)))
          (current-glyph nil))
      (while (not (eobp))
        (setq current-glyph (retro--parse-font-glyph))
        (ht-set (retro-font-glyphs font) (retro-glyph-glyph current-glyph) current-glyph))
      font)))

(defun retro--parse-font-glyph ()
  "Create front glyph by parsing current buffer."
  (if (not (string-match-p "^\\s-*>>" (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))
      nil
    (let* ((header (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
           (tokens (split-string header))
           (glyph (if (equal (nth 1 tokens) "SPACE") " " (nth 1 tokens)))
           (width (string-to-number (nth 2 tokens)))
           (height (string-to-number (nth 3 tokens)))
           (transparent-color (nth 4 tokens))
           (pixels (make-vector (* width height) nil))
           (i 0)
           current-line)
      (forward-line 1)
      (retro--skip-comments)
      (setq current-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
      (while (and (not (eobp)) (not (string-match-p "^\\s-*>>" current-line)))
        (dolist (color (split-string current-line))
          (aset pixels i (retro--add-color-to-palette color))
          (cl-incf i))
        (forward-line 1)
        (retro--skip-comments)
        (setq current-line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
      (retro--glyph-create :glyph glyph
                           :width width
                           :height height
                           :transparent-color (retro--add-color-to-palette transparent-color)
                           :pixels pixels))))

;;; Plot char
(defun retro--plot-char (font char x y canvas)
  "Plot CHAR from FONT at (X, Y) in CANVAS."
  (let ((glyph (ht-get (retro-font-glyphs font) char)))
    (if (not glyph)
        (error (format "Unable to find glyph for char `%s` in font" char))
      (let* ((cw (retro-canvas-width canvas))
             (ch (retro-canvas-height canvas))
             (px (retro-glyph-pixels glyph))
             (gw (retro-glyph-width glyph))
             (gh (retro-glyph-height glyph))
             ;; coordinates are relative to canvas
             (cl (cons 0 0))                ; canvas top left corner
             (cr (cons (1- cw) (1- ch)))    ; canvar right bottom corner
             (gl (cons x y))                ; glyph top left corner
             (gr (cons (1- gw) (1- gh)))    ; glyph right bottom corner
             ;; coordinates of the intersection
             (sx0 (max (car cl)
                       (car gl)))
             (sx1 (min (+ (car cl) (car cr))
                       (+ (car gl) (car gr))))
             (sy0 (max (cdr cl)
                       (cdr gl)))
             (sy1 (min (+ (cdr cl) (cdr cr))
                       (+ (cdr gl) (cdr gr)))))
        (when (and (<= sx0 sx1) (<= sy0 sy1))
          (retro--vector-clip-blit px
                                   ;; convert sx0 sy0 sx1 sy1 to sprite relative coordinates
                                   (- sx0 x)
                                   (- sy0 y)
                                   (- sx1 x)
                                   (- sy1 y)
                                   gw
                                   (retro-canvas-pixels canvas)
                                   sx0
                                   sy0
                                   cw
                                   (retro-glyph-transparent-color glyph)))))))

;;; Plot string
(defun retro--plot-string (font string x y letter-spacing canvas)
  "Plot STRING usgin glyphs from FONT at (X, Y) in CANVANS.

Separate chars with LETTER-SPACING amount of pixels"
  (let ((chars (split-string string "" t))
        (glyph nil))
    (dolist (char chars)
      (setq glyph (ht-get (retro-font-glyphs font) char))
      (if (not glyph)
          (error (format "Unable to find glyph for char `%s` in font" char))
        (retro--plot-char font char x y canvas)
        (if (equal char " ")
            (setq x (+ x (retro-glyph-width glyph) (- letter-spacing)))
          (setq x (+ x (retro-glyph-width glyph) letter-spacing)))))))

;;; Change font color
(defun retro--change-colors-font (font colors-map)
  "Create a new font from FROM with glyphs colors mapped with COLORS-MAP.

Colors should be specified as RGB hex string (ex. \"0xffffff\")

- FONT is a font structure (retro-font-p)
- COLORS-MAP is a list of cons ((FROM-COLOR . TO-COLOR))"
  (let* ((to-font (retro--font-create :glyphs (ht-create)))
         (colors (mapcar (lambda (m) (cons (retro--add-color-to-palette (car m))
                                          (retro--add-color-to-palette (cdr m))))
                         colors-map))
         (color-to-color (lambda (from-color)
                           (cdr (seq-find (lambda (m) (eq (car m) from-color)) colors (cons from-color from-color)))))
         (glyph nil)
         (width 0)
         (height 0)
         (from-pixels nil)
         (to-pixels nil))
    (dolist (char (ht-keys (retro-font-glyphs font)))
      (setq glyph (ht-get (retro-font-glyphs font) char)
            from-pixels (retro-glyph-pixels glyph)
            width (retro-glyph-width glyph)
            height (retro-glyph-height glyph)
            to-pixels (make-vector (* width height) 0))
      (dotimes (i (* width height))
        (aset to-pixels i (funcall color-to-color (aref from-pixels i))))
      (ht-set (retro-font-glyphs to-font)
              char
              (retro--glyph-create :glyph char
                                   :width width
                                   :height height
                                   :transparent-color (funcall color-to-color (retro-glyph-transparent-color glyph))
                                   :pixels to-pixels)))
    to-font))


;;; Sprite

(cl-defstruct (retro-sprite (:constructor retro--sprite-create)
                            (:copier nil))
  "Represent a sprite in a game."
  (x 0 :type number)
  (y 0 :type number)
  (current-play nil :type retro-play-p)
  (plays nil :type ht-p))

(cl-defstruct (retro-play (:constructor retro--play-create)
                          (:copier nil))
  "Represent a play for a sprite."
  (name nil :type string)
  (width 0 :type number)
  (height 0 :type number)
  (frame-i 0 :type number)
  (frame-n 0 :type number)
  (frames nil :type vector)
  (transparent-color 0 :type number))

(defmacro retro-sprite-width (sprite)
  `(retro-play-width (retro-sprite-current-play ,sprite)))

(defmacro retro-sprite-height (sprite)
  `(retro-play-height (retro-sprite-current-play ,sprite)))

(defmacro retro-sprite-frame (sprite)
  `(aref (retro-play-frames (retro-sprite-current-play ,sprite))
         (retro-play-frame-i (retro-sprite-current-play ,sprite))))

(defmacro retro-sset (struct-type field-name struct-value field-value)
  `(setf (,(intern (format "%s-%s" (symbol-name struct-type) (symbol-name field-name))) ,struct-value) ,field-value))

(defmacro retro-sget (struct-type field-name struct-value)
  `(,(intern (format "%s-%s" (symbol-name struct-type) (symbol-name field-name))) ,struct-value))

(defun retro--skip-comments ()
  "Skip comments line in current buffer."
  (while (and (not (eobp)) (string-match-p "^\\s-*//" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (forward-line 1)))

;;; Load sprite from file
(defun retro--load-sprite (file-path &optional x y)
  "Load a SPRITE from FILE-PATH at (X, Y)."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (retro--skip-comments)
    (let ((sprite (retro--sprite-create :x (or x 0)
                                        :y (or y 0)
                                        :current-play nil
                                        :plays (ht-create)))
          (current-play nil))
      (while (not (eobp))
        (setq current-play (retro--parse-sprite-play))
        (ht-set (retro-sprite-plays sprite) (retro-play-name current-play) current-play)
        (when (null (retro-sprite-current-play sprite))
          (setf (retro-sprite-current-play sprite) current-play)))
      sprite)))

(defun retro--parse-sprite-play ()
  "Parse current buffer and create a play for a sprite."
  ;; TODO: extract regular expressions
  (if (not (string-match-p "^\\s-*>>" (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))
      nil
    (let* ((header (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
           (tokens (split-string header))
           (name (nth 1 tokens))
           (frame-count (string-to-number (nth 2 tokens)))
           (width (string-to-number (nth 3 tokens)))
           (height (string-to-number (nth 4 tokens)))
           (transparent-color (nth 5 tokens))
           (frames (make-vector frame-count nil)))
      (dotimes (i frame-count)
        (forward-line 1)
        (aset frames i (retro--parse-play-frame width height)))
      (retro--play-create :name name
                          :width width
                          :height height
                          :frame-i 0
                          :frame-n frame-count
                          :frames frames
                          :transparent-color (retro--add-color-to-palette transparent-color)))))

(defun retro--parse-play-frame (width height)
  "Parse current buffer and create a frame (WIDTH x HEIGHT) for a sprite."
  (let ((current-frame (make-vector (* width height) 0))
        (current-line "")
        (i 0))
    (when (eobp)
      (error "Unexpected end of buffer, expected frame, sprite file corrupted"))
    (retro--skip-comments)
    (setq current-line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
    (while (and (not (eobp)) (not (string-match-p "^\\s-*\\(--\\|>>\\)" current-line)))
      (dolist (color (split-string current-line))
        (aset current-frame i (retro--add-color-to-palette color))
        (cl-incf i))
      (forward-line 1)
      (retro--skip-comments)
      (setq current-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
    current-frame))

;;; Set the current play for a sprite
(defun retro--play-sprite (sprite play-name)
  "Set play with PLAY-NAME as current play in SPRITE."
  (when (not (ht-contains-p (retro-sprite-plays sprite) play-name))
    (error (format "Sprite doesn not support play with name `%s`" play-name)))
  (setf (retro-sprite-current-play sprite) (ht-get (retro-sprite-plays sprite) play-name)
        (retro-play-frame-i (retro-sprite-current-play sprite)) 0))

;;; Move sprite
(defun retro--move-sprite (sprite dx dy x0 y0 x1 y1)
  "Move sprite"
  (let* ((x (retro-sprite-x sprite))
         (y (retro-sprite-y sprite))
         (current-play (retro-sprite-current-play sprite))
         (w (1- (retro-play-width current-play)))
         (h (1- (retro-play-height current-play))))
    ;; TODO: what's the meaning of this? Maybe done before clipping? If yes, remove!!!
    (when (/= dx 0)
      (setf (retro-sprite-x sprite) (if (> dx 0)
                                        (min (+ x dx) (- x1 w))
                                      (max (+ x dx) x0))))
    (when (/= dy 0)
      (setf (retro-sprite-y sprite) (if (> dy 0)
                                        (min (+ y dy) (- y1 h))
                                      (max (+ y dy) y0))))))

;;; Advance to the next frame of the current play
(defun retro--next-frame-sprite (sprite)
  "Make SPRITE take the next animation frame."
  (setf (retro-play-frame-i (retro-sprite-current-play sprite))
        (% (1+ (retro-play-frame-i (retro-sprite-current-play sprite)))
           (retro-play-frame-n (retro-sprite-current-play sprite)))))

;;; Flip horizonally all the frames of the current play
(defun retro--flip-h-sprite (sprite)
  "Flip horizontally every frame of the current play of SPRITE."
  (let ((width (retro-play-width (retro-sprite-current-play sprite)))
        (height (retro-play-height (retro-sprite-current-play sprite))))
    (dotimes (i (retro-play-frame-n (retro-sprite-current-play sprite)))
      (retro--flip-h-vector (aref (retro-play-frames (retro-sprite-current-play sprite)) i) width height))))

;;; Flip vertically all the frames of the current play
(defun retro--flip-v-sprite (_sprite)
  "Flip vertically every frame of the current play of SPRITE."
  (error "Not implemented."))

;;; Plot current frame of current play of sprite on canvas
(defun retro--plot-sprite (sprite canvas)
  "Plot current frame of current play for SPRITE on CANVAS."
  ;; TODO: duplicated in retro--plot-tile
  (let* ((cw (retro-canvas-width canvas))
         (ch (retro-canvas-height canvas))
         (sx (retro-sprite-x sprite))
         (sy (retro-sprite-y sprite))
         (pl (retro-sprite-current-play sprite))
         (sw (retro-play-width pl))
         (sh (retro-play-height pl))
         ;; coordinates are relative to canvas
         (cl (cons 0 0))                ; canvas top left corner
         (cr (cons (1- cw) (1- ch)))    ; canvar right bottom corner
         (sl (cons sx sy))              ; sprite top left corner
         (sr (cons (1- sw) (1- sh)))    ; sprite right bottom corner
         ;; coordinates of the intersection
         (sx0 (max (car cl)
                   (car sl)))
         (sx1 (min (+ (car cl) (car cr))
                   (+ (car sl) (car sr))))
         (sy0 (max (cdr cl)
                   (cdr sl)))
         (sy1 (min (+ (cdr cl) (cdr cr))
                   (+ (cdr sl) (cdr sr)))))
    (when (and (<= sx0 sx1) (<= sy0 sy1))
      (retro--vector-clip-blit (aref (retro-play-frames pl) (retro-play-frame-i pl))
                               ;; convert sx0 sy0 sx1 sy1 to sprite relative coordinates
                               (- sx0 sx)
                               (- sy0 sy)
                               (- sx1 sx)
                               (- sy1 sy)
                               sw
                               (retro-canvas-pixels canvas)
                               sx0
                               sy0
                               cw
                               (retro-play-transparent-color pl)))))


;;; Game

(cl-defstruct (retro-game (:constructor retro-game--create)
                          (:copier nil))
  "Game."
  (name nil :type string)
  (buffer-name nil :type string)
  (resolution nil :type cons)
  (bind nil :type list)
  (init nil :type function)
  (update nil :type function)
  (render nil :type function)
  (quit-p nil :type boolean)
  (quit nil :type function)
  (pending-events () :type list)
  (current-canvas nil :type canvas)
  (previous-canvas nil :type canvas))

(cl-defun retro-game-create (&key name resolution bind init update render (background-color 0))
  "Create GAME."
  (let* ((original-buffer (current-buffer))
         (buffer-name (format "*%s-screen*" name))
         (current-window (get-buffer-window original-buffer))
         (current-canvas (retro--init-canvas current-window buffer-name (car resolution) (cdr resolution) background-color))
         (previous-canvas (retro-canvas-copy current-canvas))
         (keymap (make-sparse-keymap))
         (bind (retro--normalize-bind bind))
         (game (retro-game--create :name name
                                   :buffer-name buffer-name
                                   :resolution resolution
                                   :bind bind
                                   :init init
                                   :update update
                                   :render render
                                   :quit-p nil
                                   :quit (lambda () (switch-to-buffer original-buffer))
                                   :pending-events '()
                                   :current-canvas current-canvas
                                   :previous-canvas previous-canvas)))
    (suppress-keymap keymap)
    (dolist (key (mapcar 'car bind))
      (if (string-match-p "mouse" (symbol-name key))
          (define-key keymap (kbd (symbol-name key)) (retro--handle-mouseclick key game))
        (define-key keymap (kbd (symbol-name key)) (retro--handle-keypress key game))))
    (with-current-buffer buffer-name
      (use-local-map keymap))
    game))

(defun retro--handle-mouseclick (click game)
  "Handle mouse click."
  ;; NOTE: allocate once per handler rather once per event, less gc
  (let* ((canvas (retro-game-current-canvas game))
         (before (retro-canvas-buffer-before-length canvas))
         (line (retro-canvas-buffer-line-length canvas))
         (margin (retro-canvas-margin-left canvas)))
    (lambda (e)
      (interactive "e")
      ;; SEE: https://www.gnu.org/software/emacs/manual/html_node/elisp/Click-Events.html
      (let ((y (/ (- (nth 1 (cadr e)) before) line))
            (x (- (% (- (nth 1 (cadr e)) before) line) margin 1)))
        (push (cons 'mouseclick (cons click (cons x y)))
              (retro-game-pending-events game))))))

(defun retro--handle-keypress (key game)
  "Handle pressed key."
  (lambda ()
    (interactive)
    ;; TODO: handle event here as well like in mouseclick?
    (push (cons 'keypress key)
          (retro-game-pending-events game))))

(defun retro--handle-pending-events (game-state game)
  "Handle all pending events."
  (let ((bind (retro-game-bind game))
        (pending-events (retro-game-pending-events game))
        (handler nil))
    (dolist (event (reverse pending-events))
      (cond ((eq (car event) 'keypress)
             (setq handler (alist-get (cdr event) bind))
             (if handler
                 ;; TODO: give also the third parameter
                 (funcall handler game-state game)
               (user-error "missing handler for keypress %s" (cdr event))))
            ((eq (car event) 'mouseclick)
             (setq handler (alist-get (cadr event) bind))
             (if handler
                 (funcall handler game-state game (cddr event))
               (user-error "missing handler for mouseclick %s" (cadr event))))
            (t (user-error "missing handler for event %s" event)))))
  (setf (retro-game-pending-events game) '()))

(defun retro--update-every (seconds update)
  "Make call the UPDATE function ~ every x SECONDS.

Normally the update function gets called as soon as possible,
generally is not predictable how much time passes between one
call and the next.

Because of that generally the update function should take in
consideration the amout of time passed in between calls (given as
parameter) to update stuff in the game state.

In some situation is more than acceptable to be called with
regularity every a certain amout of time has passed (ex. 0.5
seconds).

To do that wrap your update function with this function like

    (retro-game ...
                :update (update-every 0.5 'your-update-function)
                ...)"
  (let ((since-last-update 0.0))
    (lambda (elapsed game-state canvas)
      (setq since-last-update (+ since-last-update elapsed))
      (when (> since-last-update seconds)
        (funcall update since-last-update game-state canvas)
        (setq since-last-update 0.0)))))

(defun retro--handle-quit (_game-state game)
  "Signal the game to quit."
  (setf (retro-game-quit-p game) t))

(defun retro--normalize-bind (bind)
  "Normalize bind."
  (let ((result '())
        (keys nil)
        (handler nil))
    (dolist (e bind result)
      (setq keys (if (listp (car e)) (car e) (list (car e)))
            handler (cdr e))
      (dolist (key keys)
        (setq result (cons (cons (intern key)
                                 handler)
                           result))))))

;;; compatibility with emacs versions < 28.1
(when (not (fboundp 'garbage-collect-maybe))
  (defun garbage-collect-maybe (_n) nil))

;; TODO(OPT): always keep the canvas length
;; TODO(OPT): inheirth from special-mode

;; TODO(OPT): extract current-canvas and previous-canvas from game-state,
;; make them local and pass them as call parameters, save time accessing
;; the game struct
(defun retro--game-loop (game &optional game-state last-time)
  "Game loop."
  (let* ((game-state (or game-state (funcall (retro-game-init game))))
         (last-time (or last-time (current-time)))
         (now (current-time))
         (elapsed (float-time (time-subtract now last-time)))
         (current-canvas (retro-game-current-canvas game))
         (previous-canvas (retro-game-previous-canvas game)))
    (retro--handle-pending-events game-state game)
    (funcall (retro-game-update game) elapsed game-state current-canvas)
    (funcall (retro-game-render game) elapsed game-state current-canvas)
    (with-current-buffer (retro-game-buffer-name game)
      (retro--buffer-render current-canvas previous-canvas))
    (retro--reset-canvas previous-canvas)
    (cl-rotatef (retro-game-current-canvas game) (retro-game-previous-canvas game))
    (if (retro-game-quit-p game)
        (funcall (retro-game-quit game))
      (redisplay t)
      (run-with-timer 0.001 nil 'retro--game-loop game game-state now)
      (garbage-collect-maybe 4)
      t)))


;;; Buffer

;; TODO(OPT): unroll loop (while-unroll 10 CONDITION BODY)
(defun retro--buffer-render (current-canvas previous-canvas)
  "Render CANVAS into current buffer."
  (let* ((cpxs (retro-canvas-pixels current-canvas))
         (ppxs (retro-canvas-pixels previous-canvas))
         (width (retro-canvas-width current-canvas))
         (height (retro-canvas-height current-canvas))
         (bbl (retro-canvas-buffer-before-length current-canvas))
         (bll (retro-canvas-buffer-line-length current-canvas))
         (bml (retro-canvas-margin-left current-canvas))
         (cl (* width height))          ; canvas length
         (column -1)
         (line 0)
         (bbcll 0)
         (start 0)
         (buffer-start nil)
         (buffer-end nil)
         (length 0)
         (cpc nil)                      ; current-canvas previous color
         (ccc nil)                      ; current-canvas current color
         (pcc nil)                      ; previous-canvas current color
         (i 0))
    (setq-local buffer-read-only nil)
    (while (< i cl)
      ;; TODO(OPT): optimize from here <<<
      (setq ccc (aref cpxs i)
            pcc (aref ppxs i)
            column (1+ column))
      (when (= column width)
        ;; at the end of the buffer line
        (when (> length 0)
          (setq buffer-start (+ bbl bbcll bml start 1)
                buffer-end (+ buffer-start length))
          (put-text-property buffer-start buffer-end 'face (aref retro-palette-faces cpc))
          (setq length 0
                start 0
                cpc nil))
        (setq line (1+ line)
              bbcll (* bll line)
              column 0))
      ;; TODO(OPT): to here >>>
      (if (eq pcc ccc)
          ;; TODO(OPT): optimize from here <<<
          ;; previous canvas pixel and current canvas pixel are the same
          (when (> length 0)
            ;; plot the accomulated line so far and reset the counters
            (setq buffer-start (+ bbl bbcll bml start 1)
                  buffer-end (+ buffer-start length))
            (put-text-property buffer-start buffer-end 'face (aref retro-palette-faces cpc))
            (setq length 0
                  start column
                  cpc nil))
        ;; TODO(OPT): to here >>>
        ;; previous canvas pixel and current canvas pixel are different
        (if (eq cpc ccc)
            ;; current pixel and previous pixel are on the same line
            (setq length (1+ length))
          ;; current pixel and previous pixel are different
          (when (> length 0)
            ;; plot the accomulated line so far and reset the counters
            (setq buffer-start (+ bbl bbcll bml start 1)
                  buffer-end (+ buffer-start length))
            (put-text-property buffer-start buffer-end 'face (aref retro-palette-faces cpc)))
          (setq start column
                length 1
                cpc ccc)))
      (setq i (1+ i)))
    (when (> length 0)
      ;; plot the accomulated line so far
      (setq buffer-start (+ bbl bbcll bml start 1)
            buffer-end (+ buffer-start length))
      (put-text-property buffer-start buffer-end 'face (aref retro-palette-faces cpc)))
    (setq-local buffer-read-only t)))


;;; Plot shapes

(defun retro--plot-filled-rectangle (x0 y0 x1 y1 color canvas)
  "Plot a filled rectangle with COLOR from (X0, Y0) to (X1, Y1) in CANVAS.

'(X0, Y0) is the top left corner.
'(X1, Y1) is the bottom right corner."
  (let ((pixels (retro-canvas-pixels canvas))
        (width (retro-canvas-width canvas)))
    (dotimes (dx (1+ (- x1 x0)))
      (dotimes (dy (1+ (- y1 y0)))
        (retro--plot-pixel (+ x0 dx) (+ y0 dy) color pixels width)))))

(defun retro--plot-line (x0 y0 x1 y1 color canvas)
  "Plot a line with COLOR from (X0, Y0) to (X1, Y1) in CANVAS."
  (let* ((pixels (retro-canvas-pixels canvas))
         (width (retro-canvas-width canvas))
         (dx (- x1 x0))
         (dy (- y1 y0))
         (D (- (* 2 dy) dx))
         (y y0))
    (cl-loop for x from x0 to x1 do
             (retro--plot-pixel x y color pixels width)
             (when (> D 0)
               (setq y (+ y 1)
                     D (- D (* 2 dx))))
             (setq D (+ D (* 2 dy))))))

(defun retro--plot-pixel-v (i color pixels)
  "Plot a pixel with COLOR at index I in CANVAS."
  ;; TODO: assert out of bounds
  (aset pixels i color))

(defun retro--plot-pixel (x y color pixels width)
  "Plot a pixel with COLOR at (X, Y) in CANVAS."
  ;; TODO: assert out of bounds
  (aset pixels (+ (* y width) x) color))


;; (retro-game "gol"
;;   :init gol-init
;;   :resolution (320 . 280)
;;   :bind (("q" . gol-quit)
;;          ("p" . gol-pause)
;;          ("s" . gol-play)
;;          ("n" . gol-next))
;;   :udpate gol-update
;;   :render gol-render)

;;; loading images https://javl.github.io/image2cpp/
;;; physics https://2dengine.com/?p=platformers
;;; physics https://www.youtube.com/playlist?list=PLtrSb4XxIVbpZpV65kk73OoUcIrBzoSiO
;;; take inspiration from: https://dthompson.us/projects/chickadee.html
;;; sprite https://penusbmic.itch.io/free-dungeon-ruins-tileset

;; (progn
;;   (let ((resolution (cons 5 5))
;;         (background-color 15)
;;         (buffer-name "*fix-tile-plotting*"))
;;     (with-current-buffer buffer-name
;;       (let* ((current-window (get-buffer-window buffer-name))
;;              (current-canvas (retro--init-canvas current-window buffer-name (car resolution) (cdr resolution) background-color))
;;              (previous-canvas (canvas-copy current-canvas))
;;              (tile (retro--load-tile-at "calibrate.sprite" 1 1)))
;;         (retro--plot-tile tile current-canvas 1 0 1 2)
;;         ;; (retro--plot-tile tile current-canvas)
;;         ;; (retro--plot-pixel-v 5 (ht-get retro-palette-colors->index "#ff0000") (canvas-pixels current-canvas))
;;         ;; (retro--plot-pixel-v 6 (ht-get retro-palette-colors->index "#0000ff") (canvas-pixels current-canvas))
;;         ;; (retro--plot-pixel-v 10 (ht-get retro-palette-colors->index "#ff0000") (canvas-pixels current-canvas))
;;         ;; (retro--plot-pixel-v 9 (ht-get retro-palette-colors->index "#0000ff") (canvas-pixels current-canvas))
;;         ;; (retro--plot-pixel 0 0 (ht-get retro-palette-colors->index "#ff0000") (canvas-pixels current-canvas) 4)
;;         (retro--buffer-render current-canvas previous-canvas)
;;         (sit-for 1)
;;         ))))


(provide 'retro)

;; Local Variables:
;; coding: utf-8
;; End:
;;; retro.el ends here
