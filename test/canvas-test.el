;;; canvas-test.el --- Tests for canvas -*- lexical-binding: t -*-

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

;; Tests for canvas

;;; Code:

(require 'ert)
(require 'retro (expand-file-name "./../retro.el"))

(defvar small-canvas (retro-canvas-create :margin-left 0
                                          :margin-top 0
                                          :width 2
                                          :height 2
                                          :background-color 0))

(ert-deftest canvas-test-create ()
  (should (retro-canvas-p small-canvas)))

(ert-deftest canvas-test-copy ()
  (should (equal small-canvas (retro-canvas-copy small-canvas))))

(ert-deftest canvas-test-create-with-background-color ()
  (let ((pixels (retro-canvas-pixels small-canvas)))
    (should (equal 4 (seq-length pixels)))
    (should (equal 0 (aref pixels 0)))
    (should (equal 0 (aref pixels 1)))
    (should (equal 0 (aref pixels 2)))
    (should (equal 0 (aref pixels 3)))))

(ert-deftest canvas-test-plot-pixel-x-y ()
  (let* ((canvas (retro-canvas-copy small-canvas))
         (width (retro-canvas-width canvas))
         (pixels (retro-canvas-pixels canvas)))
    (retro--plot-pixel 0 0 1 pixels width)
    (retro--plot-pixel 1 0 2 pixels width)
    (retro--plot-pixel 0 1 3 pixels width)
    (retro--plot-pixel 1 1 4 pixels width)
    (should (equal 1 (aref pixels 0)))
    (should (equal 2 (aref pixels 1)))
    (should (equal 3 (aref pixels 2)))
    (should (equal 4 (aref pixels 3)))))

(ert-deftest canvas-test-plot-line ()
  (let* ((canvas (retro-canvas-copy small-canvas))
         (pixels (retro-canvas-pixels canvas)))
    (retro--plot-line 0 0 1 1 4 canvas)
    (should (equal 4 (aref pixels 0)))
    (should (equal 0 (aref pixels 1)))
    (should (equal 0 (aref pixels 2)))
    (should (equal 4 (aref pixels 3)))))

(ert-deftest canvas-test-plot-filled-rectangle ()
  (let* ((canvas (retro-canvas-copy small-canvas))
         (pixels (retro-canvas-pixels canvas)))
    (retro--plot-filled-rectangle 0 0 1 1 4 canvas)
    (should (equal 4 (aref pixels 0)))
    (should (equal 4 (aref pixels 1)))
    (should (equal 4 (aref pixels 2)))
    (should (equal 4 (aref pixels 3)))))

(provide 'canvas-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; canvas-test.el ends here
