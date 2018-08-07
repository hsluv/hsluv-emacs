;;; hsluv.el --- hsluv color space conversions -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2017 Geert Vermeiren
;;
;; Version: 1.0.0
;; Author: Geert Vermeiren
;; Keywords: color
;; URL: https://github.com/woozong/hsluv-elisp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an elisp implementation of the HSLUV colorspace
;; conversions as created by Alexei Boronine, and documented on
;; http://www.hsluv.org/

;; HSLuv is a human-friendly alternative to HSL.

;; CIELUV is a color space designed for perceptual uniformity based on human
;; experiments. When accessed by polar coordinates, it becomes functionally
;; similar to HSL with a single problem: its chroma component doesn't fit into
;; a specific range.

;; HSLuv extends CIELUV with a new saturation component that allows you to span
;; all the available chroma as a neat percentage.

;; The reference implementation is written in Haxe and released under
;; the MIT license. It can be found at https://github.com/hsluv/hsluv
;; The math is available under the public domain.

;; The following functions provide the conversions
;; from RGB to/from HSLUV and HPLUV:

;;   (hsluv-hsluv-to-rgb (list H S Luv))
;;   (hsluv-hpluv-to-rgb (list H P Luv))
;;   (hsluv-rgb-to-hsluv (list R G B))
;;   (hsluv-rgb-to-hpluv (list R G B))

;;; Changelog

;; 2017/04/15 v1.0  First version

;;; Code:


(defconst hsluv--m
  '((3.240969941904521 -1.537383177570093 -0.498610760293)
    (-0.96924363628087 1.87596750150772 0.041555057407175)
    (0.055630079696993 -0.20397695888897 1.056971514242878)))

(defconst hsluv--minv
  '((0.41239079926595 0.35758433938387 0.18048078840183)
    (0.21263900587151 0.71516867876775 0.072192315360733)
    (0.019330818715591 0.11919477979462 0.95053215224966)))

(defconst hsluv--refY 1.0)
(defconst hsluv--refU 0.19783000664283)
(defconst hsluv--refV 0.46831999493879)

;; CIE LUV constants

(defconst hsluv--kappa 903.2962962)
(defconst hsluv--epsilon 0.0088564516)

(defun hsluv--get-bounds (l)
  "For a given lightness, return a list of 6 lines in slope-intercept
form that represent the bounds in CIELUV, stepping over which will
push a value out of the RGB gamut"
  (let* ((L (float l))
         (sub1 (/ (expt (+ L 16) 3) 1560896))
         (sub2 (if (> sub1 hsluv--epsilon)
                   sub1
                 (/ L hsluv--kappa)))
         (bounds '()))
    (reverse
     (dotimes (c 3 bounds)
       (let ((m1 (elt (elt hsluv--m c) 0))
             (m2 (elt (elt hsluv--m c) 1))
             (m3 (elt (elt hsluv--m c) 2)))
         (dotimes (T 2)
           (let ((bottom (+ (* sub2
                               (- (* 632260 m3)
                                  (* 126452 m2)))
                            (* 126452 T)))
                 (top1 (* sub2 (- (* 284517 m1)
                                  (* 94839 m3))))
                 (top2 (- (* L sub2 (+ (* 838422 m3)
                                       (* 769860 m2)
                                       (* 731718 m1)))
                          (* 769860 T L))))
             (setq bounds (cons (list (/ top1 bottom) (/ top2 bottom))
                                bounds)))))))))

(defun hsluv--intersect-line-line (line-a line-b)
  (/ (- (elt line-a 1)
        (elt line-b 1))
     (- (elt line-b 0)
        (elt line-a 0))))

(defun hsluv-distance-from-pole (point)
  (sqrt (+ (expt (elt point 0) 2)
           (expt (elt point 1) 2))))

(defun hsluv-length-of-ray-until-intersect (theta line)
  (/ (elt line 1)
     (- (sin theta)
        (* (elt line 0)
           (cos theta)))))

(defun hsluv--max-safe-chroma-for-l (L)
  "For given lightness, returns the maximum chroma. Keeping the chroma value
below this number will ensure that for any hue, the color is within the RGB
gamut."
  (let ((bounds (hsluv--get-bounds L))
        (minimum 1.0e+INF))
    (dotimes (i 2 minimum)
      (let* ((m1 (elt (elt bounds i) 0))
             (b1 (elt (elt bounds i) 1))
             (line (list m1 b1))
             (x (hsluv--intersect-line-line line (list (/ -1.0 m1) 0)))
             (length (hsluv-distance-from-pole (list x (+ b1 (* x m1))))))
        (setq minimum (min minimum length))))))

(defun hsluv-max-chroma-for-l-h (L H)
  (let ((hrad (* (/ H 360.0) float-pi 2))
        (bounds (hsluv--get-bounds L))
        (minimum 1.0e+INF))
    (dolist (bound bounds minimum)
      (let ((length (hsluv-length-of-ray-until-intersect hrad bound)))
        (when (> length 0)
          (setq minimum (min minimum length)))))))

(defun hsluv--dot-product (a b)
  (let ((sum 0))
    (dotimes (i (safe-length a) sum)
      (setq sum (+ sum (* (elt a i) (elt b i)))))))

(defun hsluv-round (value places)
  (let ((n (float (expt 10 places))))
    (/ (fround (* value n)) n)))

;; used for rgb conversions
(defun hsluv--from-linear (c)
  (if (<= c 0.0031308)
      (* 12.92 c)
    (- (* 1.055 (expt c (/ 1 2.4))) 0.055)))

(defun hsluv--to-linear (c)
  (if (> c 0.04045)
      (expt (/ (+ c 0.055) (+ 1 0.055)) 2.4)
    (/ c 12.92)))

(defun hsluv-rgb-prepare (tuple)
  (let ((results '()))
    (reverse
     (dolist (chan tuple results)
       (let ((rounded (hsluv-round chan 3)))
         (when (or (< rounded -0.0001) (> rounded 1.0001))
           (error "Illegal rgb value: %s" tuple))
         (setq results (cons (round (* rounded 255)) results)))))))

(defun hsluv-xyz-to-rgb (tuple)
  "Converts colors from XYZ color-space to RGB color-space.
XYZ coordinates are ranging in [0;1] and RGB coordinates in [0;1] range.
TUPLE is a list containing the color's X,Y and Z values.
Returns a list containing the resulting color's red, green and blue."
  (list (hsluv--from-linear (hsluv--dot-product (elt hsluv--m 0) tuple))
        (hsluv--from-linear (hsluv--dot-product (elt hsluv--m 1) tuple))
        (hsluv--from-linear (hsluv--dot-product (elt hsluv--m 2) tuple))))

(defun hsluv-rgb-to-xyz (tuple)
  "Converts colors from RGB color-space to XYZ color-space.
RGB coordinates are ranging in [0;1] and XYZ coordinates in [0;1] range.
TUPLE is a list containing the color's R,G and B values.
Returns a list containing the resulting color's XYZ coordinates."
  (let ((rgbl (list (hsluv--to-linear (float (elt tuple 0)))
                    (hsluv--to-linear (float (elt tuple 1)))
                    (hsluv--to-linear (float (elt tuple 2))))))
    (list (hsluv--dot-product (elt hsluv--minv 0) rgbl)
          (hsluv--dot-product (elt hsluv--minv 1) rgbl)
          (hsluv--dot-product (elt hsluv--minv 2) rgbl))))

(defun hsluv--y-to-l (Y)
  "http://en.wikipedia.org/wiki/CIELUV
In these formulas, Yn refers to the reference white point. We are using
illuminant D65, so Yn (see refY in Maxima file) equals 1. The formula is
simplified accordingly."
  (if (<= Y hsluv--epsilon)
      (* (/ Y hsluv--refY) hsluv--kappa)
    (- (* 116
          (expt (/ Y hsluv--refY)
                (/ 1.0 3.0)))
       16)))

(defun hsluv-l-to-y (L)
  (if (<= L 8)
      (/ (* L hsluv--refY)
         hsluv--kappa)
    (* hsluv--refY
       (expt (/ (+ L 16) 116) 3))))

(defun hsluv-xyz-to-luv (tuple)
  (let* ((X (float (elt tuple 0)))
         (Y (float (elt tuple 1)))
         (Z (float (elt tuple 2)))
         (varU (/ (* 4 X)
                  (+ X
                     (* 15 Y)
                     (* 3 Z))))
         (varV (/ (* 9 Y)
                  (+ X
                     (* 15 Y)
                     (* 3 Z))))
         (L (hsluv--y-to-l Y))
         (U (* 13
               L
               (- varU hsluv--refU)))
         (V (* 13
               L
               (- varV hsluv--refV))))
    (list L U V)))


(defun hsluv-luv-to-xyz (tuple)
  (let ((L (float (elt tuple 0)))
        (U (float (elt tuple 1)))
        (V (float (elt tuple 2))))
    (if (= L 0)
        (list 0 0 0)
      (let* ((varU (+ (/ U (* 13 L)) hsluv--refU))
             (varV (+ (/ V (* 13 L)) hsluv--refV))
             (Y (hsluv-l-to-y L))
             (X (- 0 (/ (* 9 Y varU)
                        (* -4 varV))))
             (Z (- (/ (* 9 Y)
                      (* 3 varV))
                   (* 5 Y)
                   (/ X 3))))
        (list X Y Z)))))

(defun hsluv-luv-to-lch (tuple)
  (let* ((L (float (elt tuple 0)))
         (U (float (elt tuple 1)))
         (V (float (elt tuple 2)))
         (C (sqrt (+ (* U U) (* V V))))
         (H 0))
    (when (>= C 0.00000001)
      (setq H (radians-to-degrees (atan V U)))
      (when (< H 0) (setq H (+ H 360.0))))
    (list L C H)))

(defun hsluv-lch-to-luv (tuple)
  (let* ((L (float (elt tuple 0)))
         (C (float (elt tuple 1)))
         (H (float (elt tuple 2)))
         (Hrad (degrees-to-radians H))
         (U (* C (cos Hrad)))
         (V (* C (sin Hrad))))
    (list L U V)))

(defun hsluv-hsluv-to-lch (tuple)
  (let ((H (float (elt tuple 0)))
        (S (float (elt tuple 1)))
        (L (float (elt tuple 2))))
    (cond ((> L 99.9999999)
           (list 100.0 0.0 H))
          ((< L 0.00000001)
           (list 0.0 0.0 H))
          (t
           (list L (* (/ (hsluv-max-chroma-for-l-h L H) 100.0) S) H)))))

(defun hsluv-lch-to-hsluv (tuple)
  (let ((L (float (elt tuple 0)))
        (C (float (elt tuple 1)))
        (H (float (elt tuple 2))))
    (cond ((> L 99.9999999)
           (list H 0.0 100.0))
          ((< L 0.00000001)
           (list H 0.0 0.0))
          (t
           (list H (* 100.0 (/ C (hsluv-max-chroma-for-l-h L H))) L)))))

(defun hsluv-hpluv-to-lch (tuple)
  (let ((H (float (elt tuple 0)))
        (S (float (elt tuple 1)))
        (L (float (elt tuple 2))))
    (cond ((> L 99.9999999)
           (list 100.0 0.0 H))
          ((< L 0.00000001)
           (list 0.0 0.0 H))
          (t
           (list L (* (/ (hsluv--max-safe-chroma-for-l L) 100.0) S) H)))))

(defun hsluv-lch-to-hpluv (tuple)
  (let ((L (float (elt tuple 0)))
        (C (float (elt tuple 1)))
        (H (float (elt tuple 2))))
    (cond ((> L 99.9999999)
           (list H 0.0 100.0))
          ((< L 0.00000001)
           (list H 0.0 0.0))
          (t
           (list H (* 100.0 (/ C (hsluv--max-safe-chroma-for-l L))) L)))))

(defun hsluv-lch-to-rgb (tuple)
  (hsluv-xyz-to-rgb (hsluv-luv-to-xyz (hsluv-lch-to-luv tuple))))

(defun hsluv-rgb-to-lch (tuple)
  (hsluv-luv-to-lch (hsluv-xyz-to-luv (hsluv-rgb-to-xyz tuple))))

(defun hsluv-hsluv-to-rgb (tuple)
  (hsluv-lch-to-rgb (hsluv-hsluv-to-lch tuple)))

(defun hsluv-hpluv-to-rgb (tuple)
  (hsluv-lch-to-rgb (hsluv-hpluv-to-lch tuple)))

(defun hsluv-rgb-to-hsluv (tuple)
  (hsluv-lch-to-hsluv (hsluv-rgb-to-lch tuple)))

(defun hsluv-rgb-to-hpluv (tuple)
  (hsluv-lch-to-hpluv (hsluv-rgb-to-lch tuple)))

(defun hsluv-hsluv-to-hex (tuple)
  (apply 'color-rgb-to-hex (hsluv-hsluv-to-rgb tuple)))

(defun hsluv-hpluv-to-hex (tuple)
  (apply 'color-rgb-to-hex (hsluv-hpluv-to-rgb tuple)))

(defun hsluv-hex-to-hsluv (tuple)
  (hsluv-rgb-to-hsluv (color-name-to-rgb tuple)))

(defun hsluv-hex-to-hpluv (tuple)
  (hsluv-rgb-to-hpluv (color-name-to-rgb tuple)))

(require 'json)
(defun hsluv--read-test-file (file)
  (let ((json-object-type 'alist)
        (json-key-type 'string)
        (json-array-type 'list)
        )
    (json-read-file file)))


(defun hsluv--test-assert (hex conversion input expected actual)
  (let* ((header-printed nil)
         (error-pos 0))
    (-zip-with (lambda (v1 v2)
                 (setq error-pos (+ error-pos 1))
                 (when (> (abs (- v1 v2)) 1.0e-11)
                   (when (not header-printed)
                     (princ (concat hex " " conversion ":"))
                     (terpri)
                     (princ "    input   : ")
                     (princ input)
                     (terpri)
                     (princ "    expected: ")
                     (princ expected)
                     (terpri)
                     (princ "    got     : ")
                     (princ actual)
                     (terpri)
                     (princ "    errors @:")
                     (setq header-printed t))
                   (princ " ")
                   (princ error-pos)))
               expected
               actual)
    (if (not header-printed)
        0
      (terpri)
      1)))

(defun hsluv--test-get-tuple (name cspace))
(defun hsluv--test (file)
  (let ((test-set (hsluv--read-test-file file)))
    (dolist (color test-set)
      (let* ((hex (car color))
             (lch)
             (luv)
             (rgb)
             (xyz)
             (hpluv)
             (hsluv)
             (errors 0))
        (dolist (cspace (cdr color))
          (let ((symbl (car cspace))
                (tuple (mapcar 'float (cdr cspace))))
            (cond ((equal "lch" symbl)
                   (setq lch tuple))
                  ((equal "luv" symbl)
                   (setq luv tuple))
                  ((equal "rgb" symbl)
                   (setq rgb tuple))
                  ((equal "xyz" symbl)
                   (setq xyz tuple))
                  ((equal "hpluv" symbl)
                   (setq hpluv tuple))
                  ((equal "hsluv" symbl)
                   (setq hsluv tuple))
                  (t
                   (error (concat hex ": Unexpected cspace '" symbl "'"))))))
        (setq errors (+ errors (hsluv--test-assert hex "hex-rgb" hex rgb (color-name-to-rgb hex))))
        (setq errors (+ errors (hsluv--test-assert hex "xyz-rgb" xyz rgb (hsluv-xyz-to-rgb xyz))))
        (setq errors (+ errors (hsluv--test-assert hex "rgb-xyz" rgb xyz (hsluv-rgb-to-xyz rgb))))
        (setq errors (+ errors (hsluv--test-assert hex "xyz-luv" xyz luv (hsluv-xyz-to-luv xyz))))
        (setq errors (+ errors (hsluv--test-assert hex "luv-xyz" luv xyz (hsluv-luv-to-xyz luv))))
        (setq errors (+ errors (hsluv--test-assert hex "lch-luv" lch luv (hsluv-lch-to-luv lch))))
        (setq errors (+ errors (hsluv--test-assert hex "luv-lch" luv lch (hsluv-luv-to-lch luv))))
        (setq errors (+ errors (hsluv--test-assert hex "hsluv-lch" hsluv lch (hsluv-hsluv-to-lch hsluv))))
        (setq errors (+ errors (hsluv--test-assert hex "lch-hsluv" lch hsluv (hsluv-lch-to-hsluv lch))))
        (setq errors (+ errors (hsluv--test-assert hex "hpluv-lch" hpluv lch (hsluv-hpluv-to-lch hpluv))))
        (setq errors (+ errors (hsluv--test-assert hex "lch-hpluv" lch hpluv (hsluv-lch-to-hpluv lch))))

        (when (> errors 0)
          (error (concat hex ": " (number-to-string errors) " conversion" (if (> errors 1) "s" "") " failed.")))))))

(provide 'hsluv)
;;; hsluv.el ends here
