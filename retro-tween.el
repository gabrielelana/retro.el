;;; retro-tween.el --- Tween functions -*- lexical-binding: t -*-

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

;; Tween functions
;; - https://en.wikipedia.org/wiki/Inbetweening
;; - https://www.gamedesigning.org/animation/tweening/
;; - https://easings.net/

;;; Code:

(defconst float-pi/2 (/ float-pi 2))

(defun linear (x)
  "..."
  x)

(defun smoothstep (x)
  "..."
  (* x x (- 3 (* 2 x))))

(defun ease-in-quad (x)
  (* x x))

(defun ease-out-quad (x)
  (* x (- 2.0 x)))

(defun ease-in-cubic (x)
  (* x x x))

(defun ease-out-cubic (x)
  (let ((x* (- x 1.0)))
    (+ 1.0 (* x* x* x*))))

(defun ease-in-out-quad (x)
  (if (< x 0.5)
      (* 2.0 x x)
    (- (* (- 4 (* 2.0 x)) x) 1.0)))

(defun ease-in-sine (x)
  (+ (* (- x) (cos (* x float-pi/2))) x))

;;; interpolate
(defun lerp (start end alpha)
  (round (+ (* start (- 1.0 alpha))
            (* end alpha))))

;;; animate
(defun tween (duration start end &optional ease interpolation)
  "..."
  (let ((elapsed 0))
    (lambda (dt)
      (setq elapsed (+ elapsed dt))
      (if (> elapsed duration)
          ;; TODO: make continuation optional
          (list nil (- elapsed duration) (tween duration start end ease interpolation))
        (funcall (or interpolation 'lerp)
                 start
                 end
                 (funcall (or ease 'smoothstep)
                          (/ elapsed duration)))))))

(defun tween-loop (tw)
  "..."
  (let ((v nil))
    (lambda (dt)
      (setq v (funcall tw dt))
      (when (listp v)
        (setq tw (nth 2 v)
              v (funcall tw (nth 1 v))))
      v)))

(defun tween-concat (twl twr)
  (let ((v nil)
        (ntwl nil)
        (ntwr nil)
        (tw twl))
    (lambda (dt)
      (setq v (funcall tw dt))
      (when (listp v)
        (if (eq tw twl)
            (setq ntwl (nth 2 v)
                  tw twr
                  v (funcall tw (nth 1 v)))
          (setq ntwr (nth 2 v)
                tw nil
                v (list nil (nth 1 v) (tween-concat ntwl ntwr)))))
      v)))

(defun tween-distinct-until-changed (tw &optional test)
  (let ((pv nil)
        (cv nil))
    (lambda (dt)
      (setq cv (funcall tw dt))
      (if (listp cv)
          cv
        (if (funcall (or test 'eq) cv pv)
            nil
          (setq pv cv)
          cv)))))

;;; TODO: tween-cons

(provide 'retro-tween)

;; Local Variables:
;; coding: utf-8
;; End:
;;; retro-tween.el ends here
