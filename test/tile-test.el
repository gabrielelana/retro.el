;;; tile-test.el --- Tests for tiles -*- lexical-binding: t -*-

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

;; Tests for tiles

;;; Code:

(require 'ert)

;;; TODO: is there a better way to support both flycheck and ert-runner?
(require 'test-helper (expand-file-name "./test-helper.el") t)
(require 'test-helper (expand-file-name "./test/test-helper.el") t)
(require 'retro (expand-file-name "./../retro.el") t)
(require 'retro (expand-file-name "./retro.el") t)

(defvar simple-tile '("3 3 #000000"
                      "#ff0000  #00ff00 #0000ff"
                      "#00ff00  #0000ff #ff0000"
                      "#0000ff  #ff0000 #00ff00"))

(ert-deftest tile-test-can-be-loaded ()
  (with-content-file tile-file simple-tile
    (let ((tile (retro--load-tile tile-file)))
      (should (retro-tile-p tile))
      (let ((pixels (retro-tile-pixels tile))
            (color-1 (retro--add-color-to-palette "#ff0000")))
        (should (equal color-1 (aref pixels 0)))
        (should (equal color-1 (aref pixels 5)))
        (should (equal color-1 (aref pixels 7)))))))

(provide 'tile-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; tile-test.el ends here
