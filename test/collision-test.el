;;; collision-test.el --- summary -*- lexical-binding: t -*-

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

;; summary

;;; Code:

(require 'ert)

(require 'test-helper (expand-file-name "./test-helper.el") t)
(require 'test-helper (expand-file-name "./test/test-helper.el") t)
(require 'retro (expand-file-name "./../retro.el") t)
(require 'retro (expand-file-name "./retro.el") t)

(defvar shape-1
  (let ((px (make-vector (* 4 4) 0)))
    (aset px 0 1)
    (aset px 1 1)
    (aset px 4 1)
    (aset px 5 1)
    (aset px 9 1)
    (aset px 10 1)
    (aset px 14 1)
    (aset px 15 1)
    px)
  "Representation.
1100
1100
0110
0011")

(defvar shape-vertical-bar
  (let ((px (make-vector (* 3 3) 0)))
    (aset px 1 1)
    (aset px 4 1)
    (aset px 7 1)
    px)
  "Representation.
010
010
010")

(defvar shape-horizontal-bar
  (let ((px (make-vector (* 3 3) 0)))
    (aset px 3 1)
    (aset px 4 1)
    (aset px 5 1)
    px)
  "Representation.
010
010
010")

(ert-deftest test-intersect-bb-bottom-right-corner ()
  "It should collide.
111.
1**2
1**2
.222"
  (let ((bb1 (cons (cons 0 0) (cons 2 2)))
        (bb2 (cons (cons 1 1) (cons 3 3))))
   (should (eq 't (retro-bb-intersect? bb1 bb2)))
   (should (eq 't (retro-bb-intersect? bb2 bb1)))))

(ert-deftest test-intersect-bb-bottom-left-corner ()
  "It should collide.
.111
2**1
2**1
222."
  (let ((bb1 (cons (cons 1 0) (cons 3 2)))
        (bb2 (cons (cons 0 1) (cons 2 3))))
    (should (eq 't (retro-bb-intersect? bb1 bb2)))
    (should (eq 't (retro-bb-intersect? bb2 bb1)))))

(ert-deftest test-intersect-bb-superimposed ()
  "It should collide.
***
***
***"
  (let ((bb (cons (cons 0 0) (cons 2 2))))
    (should (eq 't (retro-bb-intersect? bb bb)))))

(ert-deftest test-do-not-intersect-bb ()
  "It should not collide.
111
111.222
111.222
....222"
  (let ((bb1 (cons (cons 0 0) (cons 2 2)))
        (bb2 (cons (cons 4 1) (cons 6 3))))
    (should (eq nil (retro-bb-intersect? bb1 bb2)))
    (should (eq nil (retro-bb-intersect? bb2 bb1)))))

(ert-deftest test-intersect-pp-001 ()
  "They should not collide.
..1100
..1100
..0110
110011
1100..
0110..
0011.."
  (let ((cl shape-1)
        (cl-bb (cons (cons 2 0) (cons 5 3)))
        (cl-tc 0)
        (cr shape-1)
        (cr-bb (cons (cons 0 3) (cons 3 6)))
        (cr-tc 0))
    (should (eq nil (retro-pp-intersect? cl cl-bb cl-tc
                                         cr cr-bb cr-tc)))))

(ert-deftest test-intersect-pp-002 ()
  "They should not collide.
.1100
.1100
.0110
11011
1100.
0110.
0011."
  (let ((cl shape-1)
        (cl-bb (cons (cons 1 0) (cons 4 3)))
        (cl-tc 0)
        (cr shape-1)
        (cr-bb (cons (cons 0 3) (cons 3 6)))
        (cr-tc 0))
    (should (eq nil (retro-pp-intersect? cl cl-bb cl-tc
                                         cr cr-bb cr-tc)))))

(ert-deftest test-intersect-pp-003 ()
  "They should not collide.
1100
1100
0110
1111
1100
0110
0011"
  (let ((cl shape-1)
        (cl-bb (cons (cons 1 0) (cons 4 3)))
        (cl-tc 0)
        (cr shape-1)
        (cr-bb (cons (cons 0 3) (cons 3 6)))
        (cr-tc 0))
    (should (eq nil (retro-pp-intersect? cl cl-bb cl-tc
                                         cr cr-bb cr-tc)))))

(ert-deftest test-intersect-pp-004 ()
  "They should collide.
1100
1100
0110
00**00
..1100
..0110
..0011"
  (let ((cl shape-1)
        (cl-bb (cons (cons 0 0) (cons 3 3)))
        (cl-tc 0)
        (cr shape-1)
        (cr-bb (cons (cons 2 3) (cons 5 6)))
        (cr-tc 0))
    (should (eq 't (retro-pp-intersect? cl cl-bb cl-tc
                                        cr cr-bb cr-tc)))))

(ert-deftest test-intersect-pp-005 ()
  "They should collide.
1100
1100
0110
01*10
.1100
.0110
.0011"
  (let ((cl shape-1)
        (cl-bb (cons (cons 0 0) (cons 3 3)))
        (cl-tc 0)
        (cr shape-1)
        (cr-bb (cons (cons 1 3) (cons 4 6)))
        (cr-tc 0))
    (should (eq 't (retro-pp-intersect? cl cl-bb cl-tc
                                        cr cr-bb cr-tc)))))

(provide 'collision-test)

;; Local Variables:
;; coding: utf-8
;; End:
;;; collision-test.el ends here
