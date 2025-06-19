;;; cube.el --- Ascii Cube -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 16, 2025
;; Modified: June 16, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/cube
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Draw Ascii Cube
;;
;;; Code:

;;(setq tileset ",-~:;=!*#$@")
(setq ascii-cube-tileset ".:!@##")
(load "/mnt/localdisk/__tifr/faltu/lisp/debug.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/garbage.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/matrix.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/triangle.el")

;; half of size of cube side
(setq ascii-cube-size 10.0)
(setq ascii-cube-shift 20.0)

(setq ascii-cube-T1 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                           (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift))))
(setq ascii-cube-T2 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift))))
(setq ascii-cube-T3 (ascii-cube-triangle-create (list (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                           (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T4 (ascii-cube-triangle-create (list (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T5 (ascii-cube-triangle-create (list (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T6 (ascii-cube-triangle-create (list (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                           (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))


(setq ascii-cube-T7 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T8 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T9 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                           (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                           (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T10 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                            (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                            (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T11 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                            (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                            (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T12 (ascii-cube-triangle-create (list (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                            (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                            (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))


(defun ascii-cube-translate (object shift)
  (dolist (triangle object)
    (dolist (vertex triangle)
      (aset vertex 0 (+ (aref vertex 0) (aref shift 0)))
      (aset vertex 1 (+ (aref vertex 1) (aref shift 1)))
      (aset vertex 2 (+ (aref vertex 2) (aref shift 2)))
      )))

(setq ascii-cube-theta 0.2)
(setq ascii-cube-theta- -0.2)

(setq ascii-cube-Rx (ascii-cube-matrix-create (list 1 0 0
                              0 (cos ascii-cube-theta) (- (sin ascii-cube-theta))
                              0 (sin ascii-cube-theta) (cos ascii-cube-theta))
                        3 3))
(setq ascii-cube-Ry (ascii-cube-matrix-create (list (cos ascii-cube-theta) 0 (sin ascii-cube-theta)
                              0 1 0
                              (- (sin ascii-cube-theta)) 0 (cos ascii-cube-theta))
                        3 3))
(setq ascii-cube-Rz (ascii-cube-matrix-create (list (cos ascii-cube-theta) (- (sin ascii-cube-theta)) 0
                              (sin ascii-cube-theta) (cos ascii-cube-theta) 0
                              0 0 1)
                        3 3))
(setq ascii-cube-Rx- (ascii-cube-matrix-create (list 1 0 0
                              0 (cos ascii-cube-theta-) (- (sin ascii-cube-theta-))
                              0 (sin ascii-cube-theta-) (cos ascii-cube-theta-))
                        3 3))
(setq ascii-cube-Ry- (ascii-cube-matrix-create (list (cos ascii-cube-theta-) 0 (sin ascii-cube-theta-)
                              0 1 0
                              (- (sin ascii-cube-theta-)) 0 (cos ascii-cube-theta-))
                        3 3))
(setq ascii-cube-Rz- (ascii-cube-matrix-create (list (cos ascii-cube-theta-) (- (sin ascii-cube-theta-)) 0
                              (sin ascii-cube-theta-) (cos ascii-cube-theta-) 0
                              0 0 1)
                        3 3))


(defun ascii-cube-rotate (object dir)
  (ascii-cube-translate object (ascii-cube-matrix-create (list 0 0 (- 0 ascii-cube-shift ascii-cube-size)) 1 3))
  (dolist (triangle object)
    (dolist (vertex triangle)
      (setq tmp vertex)
      (if (= dir 0)
          (setq tmp (ascii-cube-matrix-mult ascii-cube-Ry- tmp 3 3 3 1)))
      (if (= dir 1)
          (setq tmp (ascii-cube-matrix-mult ascii-cube-Rx tmp 3 3 3 1)))
      (if (= dir 2)
          (setq tmp (ascii-cube-matrix-mult ascii-cube-Rx- tmp 3 3 3 1)))
      (if (= dir 3)
          (setq tmp (ascii-cube-matrix-mult ascii-cube-Ry tmp 3 3 3 1)))
      (if (= dir 4)
          (setq tmp (ascii-cube-matrix-mult ascii-cube-Rz tmp 3 3 3 1)))
      (if (= dir 5)
          (setq tmp (ascii-cube-matrix-mult ascii-cube-Rz- tmp 3 3 3 1)))
      (aset vertex 0 (aref tmp 0))
      (aset vertex 1 (aref tmp 1))
      (aset vertex 2 (aref tmp 2))))
  (ascii-cube-translate object (ascii-cube-matrix-create (list 0 0 (+ ascii-cube-shift ascii-cube-size)) 1 3))
  )

(setq ascii-cube (list  ascii-cube-T1 ascii-cube-T2 ascii-cube-T3 ascii-cube-T4
                  ascii-cube-T5 ascii-cube-T6 ascii-cube-T7 ascii-cube-T8
                  ascii-cube-T9 ascii-cube-T10 ascii-cube-T11 ascii-cube-T12))

(setq ascii-cube-half-screen-width 15)
(setq ascii-cube-half-screen-height 15)

;; garbage collection
;; gc-elapsed
;; (setq garbage-collection-messages nil)


(defun ascii-cube-draw-frame (object)
  (with-current-buffer "ascii-cube"
    (erase-buffer)
      (dotimes (i (* 2 ascii-cube-half-screen-width))
        (dotimes (j (* 2 ascii-cube-half-screen-width))
          (setq z_min 100.0)
          (setq x (- j ascii-cube-half-screen-width))
          (setq y (- ascii-cube-half-screen-height i))
          (setq ascii-cube-collision -1)
          (dolist (triangle object)
            ;; (debug-nl (list "dolist  " itr))
            (setq centr_z (aref (ascii-cube-triangle-centroid triangle) 2))
            (if (and (< centr_z z_min)
                 (ascii-cube-triangle-inside (ascii-cube-triangle-project triangle) x y))
               (progn (setq ascii-cube-collision (ascii-cube-triangle-shade triangle))
                      (setq z_min centr_z))))
          (if (>= ascii-cube-collision 0)
              (progn (insert (aref ascii-cube-tileset ascii-cube-collision))
                     (insert (aref ascii-cube-tileset ascii-cube-collision)))
              (insert "  ")))
        (insert "\n"))
      (put-text-property 1 2 'keymap ascii-cube-keymap)
      (beginning-of-buffer))
  (garbage-collect))


(defun ascii-cube-animate ()
  (interactive)
  (draw-frame ascii-cube)
  (ascii-cube-rotate ascii-cube 0)
  (ascii-cube-rotate ascii-cube 1)
  )

(defun ascii-cube-animate-up ()
  (interactive)
  (ascii-cube-rotate ascii-cube 2) (ascii-cube-draw-frame ascii-cube))
(defun ascii-cube-animate-down ()
  (interactive)
  (ascii-cube-rotate ascii-cube 1) (ascii-cube-draw-frame ascii-cube))
(defun ascii-cube-animate-left ()
  (interactive)
  (ascii-cube-rotate ascii-cube 0) (ascii-cube-draw-frame ascii-cube))
(defun ascii-cube-animate-right ()
  (interactive)
  (ascii-cube-rotate ascii-cube 3) (ascii-cube-draw-frame ascii-cube))
(defun ascii-cube-animate-z-left ()
  (interactive)
  (ascii-cube-rotate ascii-cube 4) (ascii-cube-draw-frame ascii-cube))
(defun ascii-cube-animate-z-right ()
  (interactive)
  (ascii-cube-rotate ascii-cube 5) (ascii-cube-draw-frame ascii-cube))



(provide 'cube)
;;; cube.el ends here
