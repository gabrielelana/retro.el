;;; sprite-test.el --- Tests for sprites -*- lexical-binding: t -*-

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

;; Tests for sprites

;;; Code:

;;; TODO: is there a better way to support both flycheck and ert-runner?
(require 'test-helper (expand-file-name "./test-helper.el") t)
(require 'test-helper (expand-file-name "./test/test-helper.el") t)
(require 'retro (expand-file-name "./../retro.el") t)
(require 'retro (expand-file-name "./retro.el") t)

(defvar one-play-sprite '(">> default 3 3 3 #000000"
                          "#ff0000  #000000 #000000"
                          "#ff0000  #000000 #000000"
                          "#ff0000  #000000 #000000"
                          "--"
                          "#000000  #ff0000 #000000"
                          "#000000  #ff0000 #000000"
                          "#000000  #ff0000 #000000"
                          "--"
                          "#000000  #000000 #ff0000"
                          "#000000  #000000 #ff0000"
                          "#000000  #000000 #ff0000"))

(ert-deftest sprite-test-can-be-loaded ()
  (with-content-file sprite-file one-play-sprite
    (let ((sprite (retro--load-sprite sprite-file)))
      (should (retro-sprite-p sprite))
      (let* ((frames (retro-play-frames (retro-sprite-current-play sprite)))
             (pixels (aref frames (retro-play-frame-i (retro-sprite-current-play sprite))))
             (color-0 (retro--add-color-to-palette "#000000"))
             (color-1 (retro--add-color-to-palette "#ff0000")))
        (should (equal color-1 (aref pixels 0)))
        (should (equal color-0 (aref pixels 1)))
        (should (equal color-0 (aref pixels 2)))
        (should (equal color-1 (aref pixels 3)))))))

(ert-deftest sprite-test-can-be-plot ()
  (with-content-file sprite-file one-play-sprite
    (let* ((sprite (retro--load-sprite sprite-file))
           (color-0 (retro--add-color-to-palette "#000000"))
           (color-1 (retro--add-color-to-palette "#ff0000"))
           (canvas (retro-canvas-create :margin-left 0
                                        :margin-top 0
                                        :width 3
                                        :height 3
                                        :background-color 0))
           (cpxs (retro-canvas-pixels canvas)))
      (retro--plot-sprite sprite canvas)
      (should (equal color-1 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-1 (aref cpxs 3)))
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-1 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8))))))

(defvar one-play-sprite-with-comments '("// header comment"
                                        ">> default 3 3 3 #000000"
                                        "// this is a comment"
                                        "#ff0000  #000000 #000000"
                                        "#ff0000  #000000 #000000"
                                        "// this is a comment"
                                        "#ff0000  #000000 #000000"
                                        " // this is a comment"
                                        "--"
                                        "#000000  #ff0000 #000000"
                                        "#000000  #ff0000 #000000"
                                        "#000000  #ff0000 #000000"
                                        "--"
                                        "  // this is a comment"
                                        "#000000  #000000 #ff0000"
                                        "#000000  #000000 #ff0000"
                                        "#000000  #000000 #ff0000"
                                        " // this is a comment"
                                        ))

(ert-deftest sprite-test-load-skips-comments ()
  (with-content-file tile-file one-play-sprite-with-comments
    (let ((sprite (retro--load-sprite tile-file)))
      (should (retro-sprite-p sprite))
      (should (equal 3 (retro-play-frame-n (retro-sprite-current-play sprite))))
      (let* ((frames (retro-play-frames (retro-sprite-current-play sprite)))
             (pixels (aref frames (retro-play-frame-i (retro-sprite-current-play sprite))))
             (color-0 (retro--add-color-to-palette "#000000"))
             (color-1 (retro--add-color-to-palette "#ff0000")))
        (should (equal color-1 (aref pixels 0)))
        (should (equal color-0 (aref pixels 1)))
        (should (equal color-0 (aref pixels 2)))
        (should (equal color-1 (aref pixels 3)))))))


(defvar simple-sprite-full '(">> default 1 3 3 #000000"
                             "#ffffff  #ffffff #ffffff"
                             "#ffffff  #ffffff #ffffff"
                             "#ffffff  #ffffff #ffffff"))

(ert-deftest sprite-test-plot-bigger-than-canvas ()
  (with-content-file sprite-file simple-sprite-full
    (let* ((sprite (retro--load-sprite sprite-file))
           (color-1 (retro--add-color-to-palette "#ffffff"))
           (canvas (retro-canvas-create :margin-left 0
                                        :margin-top 0
                                        :width 2
                                        :height 2
                                        :background-color 0))
           (cpxs (retro-canvas-pixels canvas)))
      (setf (retro-sprite-x sprite) 0)
      (setf (retro-sprite-y sprite) 0)
      (retro--plot-sprite sprite canvas)
      (should (equal color-1 (aref cpxs 0)))
      (should (equal color-1 (aref cpxs 1)))
      (should (equal color-1 (aref cpxs 2)))
      (should (equal color-1 (aref cpxs 3))))))

(ert-deftest sprite-test-plot-smaller-than-canvas ()
  (with-content-file tile-file simple-sprite-full
    (let* ((sprite (retro--load-sprite tile-file))
           (color-0 (retro--add-color-to-palette "#000000"))
           (color-1 (retro--add-color-to-palette "#ffffff"))
           (canvas (retro-canvas-create :margin-left 0
                                        :margin-top 0
                                        :width 5
                                        :height 5
                                        :background-color 0))
           (cpxs (retro-canvas-pixels canvas)))

      ;; 1 1 1 0 0  top left corner
      ;; 1 1 1 0 0
      ;; 1 1 1 0 0
      ;; 0 0 0 0 0
      ;; 0 0 0 0 0
      (setf (retro-sprite-x sprite) 0)
      (setf (retro-sprite-y sprite) 0)
      (retro--plot-sprite sprite canvas)
      (should (equal color-1 (aref cpxs 0)))
      (should (equal color-1 (aref cpxs 1)))
      (should (equal color-1 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 3)))
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-1 (aref cpxs 5)))
      (should (equal color-1 (aref cpxs 6)))
      (should (equal color-1 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (should (equal color-0 (aref cpxs 9)))
      (should (equal color-1 (aref cpxs 10)))
      (should (equal color-1 (aref cpxs 11)))
      (should (equal color-1 (aref cpxs 12)))
      (should (equal color-0 (aref cpxs 13)))
      (should (equal color-0 (aref cpxs 14)))
      (should (equal color-0 (aref cpxs 15)))
      (should (equal color-0 (aref cpxs 16)))
      (should (equal color-0 (aref cpxs 17)))
      (should (equal color-0 (aref cpxs 18)))
      (should (equal color-0 (aref cpxs 19)))
      (retro--reset-canvas canvas)

      ;; 0 0 1 1 1  top right corner
      ;; 0 0 1 1 1
      ;; 0 0 1 1 1
      ;; 0 0 0 0 0
      ;; 0 0 0 0 0
      (setf (retro-sprite-x sprite) 2)
      (setf (retro-sprite-y sprite) 0)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-1 (aref cpxs 2)))
      (should (equal color-1 (aref cpxs 3)))
      (should (equal color-1 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-1 (aref cpxs 7)))
      (should (equal color-1 (aref cpxs 8)))
      (should (equal color-1 (aref cpxs 9)))
      (should (equal color-0 (aref cpxs 11)))
      (should (equal color-1 (aref cpxs 12)))
      (should (equal color-1 (aref cpxs 13)))
      (should (equal color-1 (aref cpxs 14)))
      (should (equal color-0 (aref cpxs 16)))
      (should (equal color-0 (aref cpxs 17)))
      (should (equal color-0 (aref cpxs 18)))
      (should (equal color-0 (aref cpxs 19)))
      (retro--reset-canvas canvas)

      ;; 0 0 0 0 0  bottom right corner
      ;; 0 0 0 0 0
      ;; 0 0 1 1 1
      ;; 0 0 1 1 1
      ;; 0 0 1 1 1
      (setf (retro-sprite-x sprite) 2)
      (setf (retro-sprite-y sprite) 2)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 3)))
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (should (equal color-0 (aref cpxs 9)))
      (should (equal color-0 (aref cpxs 10)))
      (should (equal color-0 (aref cpxs 11)))
      (should (equal color-1 (aref cpxs 12)))
      (should (equal color-1 (aref cpxs 13)))
      (should (equal color-1 (aref cpxs 14)))
      (should (equal color-0 (aref cpxs 15)))
      (should (equal color-0 (aref cpxs 16)))
      (should (equal color-1 (aref cpxs 17)))
      (should (equal color-1 (aref cpxs 18)))
      (should (equal color-1 (aref cpxs 19)))
      (should (equal color-0 (aref cpxs 20)))
      (should (equal color-0 (aref cpxs 21)))
      (should (equal color-1 (aref cpxs 22)))
      (should (equal color-1 (aref cpxs 23)))
      (should (equal color-1 (aref cpxs 24)))
      (retro--reset-canvas canvas)

      ;; 0 0 0 0 0  bottom left corner
      ;; 0 0 0 0 0
      ;; 1 1 1 0 0
      ;; 1 1 1 0 0
      ;; 1 1 1 0 0
      (setf (retro-sprite-x sprite) 0)
      (setf (retro-sprite-y sprite) 2)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 3)))
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (should (equal color-0 (aref cpxs 9)))
      (should (equal color-1 (aref cpxs 10)))
      (should (equal color-1 (aref cpxs 11)))
      (should (equal color-1 (aref cpxs 12)))
      (should (equal color-0 (aref cpxs 13)))
      (should (equal color-0 (aref cpxs 14)))
      (should (equal color-1 (aref cpxs 15)))
      (should (equal color-1 (aref cpxs 16)))
      (should (equal color-1 (aref cpxs 17)))
      (should (equal color-0 (aref cpxs 18)))
      (should (equal color-0 (aref cpxs 19)))
      (should (equal color-1 (aref cpxs 20)))
      (should (equal color-1 (aref cpxs 21)))
      (should (equal color-1 (aref cpxs 22)))
      (should (equal color-0 (aref cpxs 23)))
      (should (equal color-0 (aref cpxs 24)))
      (retro--reset-canvas canvas)

      ;; 0 0 0 0 0  center
      ;; 0 1 1 1 0
      ;; 0 1 1 1 0
      ;; 0 1 1 1 0
      ;; 0 0 0 0 0
      (setf (retro-sprite-x sprite) 1)
      (setf (retro-sprite-y sprite) 1)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 3)))
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-1 (aref cpxs 6)))
      (should (equal color-1 (aref cpxs 7)))
      (should (equal color-1 (aref cpxs 8)))
      (should (equal color-0 (aref cpxs 9)))
      (should (equal color-0 (aref cpxs 10)))
      (should (equal color-1 (aref cpxs 11)))
      (should (equal color-1 (aref cpxs 12)))
      (should (equal color-1 (aref cpxs 13)))
      (should (equal color-0 (aref cpxs 14)))
      (should (equal color-0 (aref cpxs 15)))
      (should (equal color-1 (aref cpxs 16)))
      (should (equal color-1 (aref cpxs 17)))
      (should (equal color-1 (aref cpxs 18)))
      (should (equal color-0 (aref cpxs 19)))
      (should (equal color-0 (aref cpxs 20)))
      (should (equal color-0 (aref cpxs 21)))
      (should (equal color-0 (aref cpxs 22)))
      (should (equal color-0 (aref cpxs 23)))
      (should (equal color-0 (aref cpxs 24))))))

(ert-deftest sprite-test-plot-interesction-with-canvas ()
  (with-content-file tile-file simple-sprite-full
    (let* ((sprite (retro--load-sprite tile-file))
           (color-0 (retro--add-color-to-palette "#000000"))
           (color-1 (retro--add-color-to-palette "#ffffff"))
           (canvas (retro-canvas-create :margin-left 0
                                        :margin-top 0
                                        :width 3
                                        :height 3
                                        :background-color 0))
           (cpxs (retro-canvas-pixels canvas)))

      ;; 1 1 1
      ;; 1 1 1
      ;; 1 1 1 0 0    upper left corner 1x1
      ;;     0 0 0
      ;;     0 0 0
      (setf (retro-sprite-x sprite) -2)
      (setf (retro-sprite-y sprite) -2)
      (retro--plot-sprite sprite canvas)
      (should (equal color-1 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 3)))
      (retro--reset-canvas canvas)

      ;; 1 1 1
      ;; 1 1 1 0      upper left corner 2x2
      ;; 1 1 1 0
      ;;   0 0 0
      (setf (retro-sprite-x sprite) -1)
      (setf (retro-sprite-y sprite) -1)
      (retro--plot-sprite sprite canvas)
      (should (equal color-1 (aref cpxs 0)))
      (should (equal color-1 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-1 (aref cpxs 3)))
      (should (equal color-1 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;;     1 1 1
      ;;     1 1 1
      ;; 0 0 1 1 1    upper right corner 1x1
      ;; 0 0 0
      ;; 0 0 0
      (setf (retro-sprite-x sprite) 2)
      (setf (retro-sprite-y sprite) -2)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-1 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (retro--reset-canvas canvas)

      ;;   1 1 1
      ;; 0 1 1 1    upper right corner 2x2
      ;; 0 1 1 1
      ;; 0 0 0
      (setf (retro-sprite-x sprite) 1)
      (setf (retro-sprite-y sprite) -1)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-1 (aref cpxs 1)))
      (should (equal color-1 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 3)))
      (should (equal color-1 (aref cpxs 4)))
      (should (equal color-1 (aref cpxs 5)))
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;;     0 0 0
      ;;     0 0 0
      ;; 1 1 1 0 0    bottom left corner 1x1
      ;; 1 1 1
      ;; 1 1 1
      (setf (retro-sprite-x sprite) -2)
      (setf (retro-sprite-y sprite) 2)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-1 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;;   0 0 0
      ;; 1 1 1 0     bottom left corner 2x2
      ;; 1 1 1 0
      ;; 1 1 1
      (setf (retro-sprite-x sprite) -1)
      (setf (retro-sprite-y sprite) 1)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-1 (aref cpxs 3)))
      (should (equal color-1 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-1 (aref cpxs 6)))
      (should (equal color-1 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;; 0 0 0
      ;; 0 0 0
      ;; 0 0 1 1 1   bottom right corner 1x1
      ;;     1 1 1
      ;;     1 1 1
      (setf (retro-sprite-x sprite) 2)
      (setf (retro-sprite-y sprite) 2)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 4)))
      (should (equal color-0 (aref cpxs 5)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-1 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;; 0 0 0
      ;; 0 1 1 1
      ;; 0 1 1 1     bottom right corner 2x2
      ;;   1 1 1
      (setf (retro-sprite-x sprite) 1)
      (setf (retro-sprite-y sprite) 1)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 2)))
      (should (equal color-0 (aref cpxs 3)))
      (should (equal color-1 (aref cpxs 4)))
      (should (equal color-1 (aref cpxs 5)))
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-1 (aref cpxs 7)))
      (should (equal color-1 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;; 1 1 1
      ;; 1 1 1       complete overlap
      ;; 1 1 1
      (setf (retro-sprite-x sprite) 0)
      (setf (retro-sprite-y sprite) 0)
      (retro--plot-sprite sprite canvas)
      (should (equal color-1 (aref cpxs 0)))
      (should (equal color-1 (aref cpxs 1)))
      (should (equal color-1 (aref cpxs 2)))
      (should (equal color-1 (aref cpxs 3)))
      (should (equal color-1 (aref cpxs 4)))
      (should (equal color-1 (aref cpxs 5)))
      (should (equal color-1 (aref cpxs 6)))
      (should (equal color-1 (aref cpxs 7)))
      (should (equal color-1 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;; 1 1 1
      ;; 1 1 1
      ;; 1 1 1
      ;;       0 0 0 outside
      ;;       0 0 0
      ;;       0 0 0
      (setf (retro-sprite-x sprite) -3)
      (setf (retro-sprite-y sprite) -3)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 3)))
      (retro--reset-canvas canvas)

      ;;       1 1 1
      ;;       1 1 1
      ;;       1 1 1
      ;; 0 0 0        outside
      ;; 0 0 0
      ;; 0 0 0
      (setf (retro-sprite-x sprite) 3)
      (setf (retro-sprite-y sprite) -3)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 0)))
      (should (equal color-0 (aref cpxs 1)))
      (should (equal color-0 (aref cpxs 3)))
      (retro--reset-canvas canvas)

      ;;       0 0 0
      ;;       0 0 0
      ;;       0 0 0
      ;; 1 1 1        outside
      ;; 1 1 1
      ;; 1 1 1
      (setf (retro-sprite-x sprite) -3)
      (setf (retro-sprite-y sprite) 3)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (retro--reset-canvas canvas)

      ;; 0 0 0
      ;; 0 0 0
      ;; 0 0 0
      ;;       1 1 1  outside
      ;;       1 1 1
      ;;       1 1 1
      (setf (retro-sprite-x sprite) 3)
      (setf (retro-sprite-y sprite) 3)
      (retro--plot-sprite sprite canvas)
      (should (equal color-0 (aref cpxs 6)))
      (should (equal color-0 (aref cpxs 7)))
      (should (equal color-0 (aref cpxs 8)))
      (retro--reset-canvas canvas))))

(provide 'sprite-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; sprite-test.el ends here
