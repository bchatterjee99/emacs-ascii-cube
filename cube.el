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
(setq tileset ".:o@##")
(load "/mnt/localdisk/__tifr/faltu/lisp/debug.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/garbage.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/matrix.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/triangle.el")

;; half of size of cube side
(setq size 10.0)
(setq shift 20.0)

(setq T1 (triangle-create (list (- size) (- size) (+ shift)
                                (- size) (+ size) (+ shift)
                                (+ size) (+ size) (+ shift))))
(setq T2 (triangle-create (list (- size) (- size) (+ shift)
                                (+ size) (- size) (+ shift)
                                (+ size) (+ size) (+ shift))))
(setq T3 (triangle-create (list (- size) (+ size) (+ shift)
                                (- size) (+ size) (+ shift size size)
                                (+ size) (+ size) (+ shift size size))))
(setq T4 (triangle-create (list (- size) (+ size) (+ shift)
                                (+ size) (+ size) (+ shift)
                                (+ size) (+ size) (+ shift size size))))
(setq T5 (triangle-create (list (+ size) (- size) (+ shift)
                                (+ size) (+ size) (+ shift)
                                (+ size) (+ size) (+ shift size size))))
(setq T6 (triangle-create (list (+ size) (- size) (+ shift)
                                (+ size) (- size) (+ shift size size)
                                (+ size) (+ size) (+ shift size size))))


(setq T7 (triangle-create (list (- size) (- size) (+ shift size size)
                                (- size) (+ size) (+ shift size size)
                                (+ size) (+ size) (+ shift size size))))
(setq T8 (triangle-create (list (- size) (- size) (+ shift size size)
                                (+ size) (- size) (+ shift size size)
                                (+ size) (+ size) (+ shift size size))))
(setq T9 (triangle-create (list (- size) (- size) (+ shift)
                                (- size) (- size) (+ shift size size)
                                (+ size) (- size) (+ shift size size))))
(setq T10 (triangle-create (list (- size) (- size) (+ shift)
                                 (+ size) (- size) (+ shift)
                                 (+ size) (- size) (+ shift size size))))
(setq T11 (triangle-create (list (- size) (- size) (+ shift)
                                 (- size) (+ size) (+ shift)
                                 (- size) (+ size) (+ shift size size))))
(setq T12 (triangle-create (list (- size) (- size) (+ shift)
                                 (- size) (- size) (+ shift size size)
                                 (- size) (+ size) (+ shift size size))))



(defun object-translate (object shift)
  (dolist (triangle object)
    (dolist (vertex triangle)
      (aset vertex 0 (+ (aref vertex 0) (aref shift 0)))
      (aset vertex 1 (+ (aref vertex 1) (aref shift 1)))
      (aset vertex 2 (+ (aref vertex 2) (aref shift 2)))
      )))

(setq theta 0.2)
(setq Rx (matrix-create (list 1 0 0
                              0 (cos theta) (- (sin theta))
                              0 (sin theta) (cos theta))
                        3 3))
(setq Ry (matrix-create (list (cos theta) 0 (sin theta)
                              0 1 0
                              (- (sin theta)) 0 (cos theta))
                        3 3))
(setq Rz (matrix-create (list (cos theta) (- (sin theta)) 0
                              (sin theta) (cos theta) 0
                              0 0 1)
                        3 3))
(setq scale2 (matrix-create (list 2 0 0
                                  0 2 0
                                  0 0 2)
                            3 3))
(setq scale3 (matrix-create (list 3 0 0
                                  0 3 0
                                  0 0 3)
                            3 3))


(defun object-rotate (object)
  (object-translate object (matrix-create (list 0 0 (- 0 shift size)) 1 3))
  (dolist (triangle object)
    (dolist (vertex triangle)
      (setq tmp vertex)
      (setq tmp (matrix-mult Rz tmp 3 3 3 1))
      (setq tmp (matrix-mult Rx tmp 3 3 3 1))
      (aset vertex 0 (aref tmp 0))
      (aset vertex 1 (aref tmp 1))
      (aset vertex 2 (aref tmp 2))))
  (object-translate object (matrix-create (list 0 0 (+ shift size)) 1 3))
  )

(setq cube (list  T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12))
;; (setq cube (list  T1 T2))
;; (object-rotate cube)


(setq half-screen-width 10)
(setq half-screen-height 10)


;; garbale collection
gc-elapsed
(setq garbage-collection-messages t)



(defun draw-frame (object)
  (with-current-buffer "cube"
    (erase-buffer)
      (dotimes (i (* 2 half-screen-width))
        (dotimes (j (* 2 half-screen-width))
          (setq z_min 100.0)
          (setq x (- j half-screen-width))
          (setq y (- half-screen-height i))
          (setq collision -1)
          (setq itr 0)
          (dolist (triangle object collision)
            ;; (debug-nl (list "dolist  " itr))
            (setq centr_z (aref (triangle-centroid triangle) 2))
            (if (and (< centr_z z_min)
                 (triangle-inside (triangle-project triangle) x y))
               (progn (setq collision (triangle-shade triangle))
                      (setq z_min centr_z)))
            (setq itr (1+ itr)))
          (if (>= collision 0)
              (progn (insert (aref tileset collision))
                     (insert (aref tileset collision)))
              (insert "  ")))
        (insert "\n")))
  (garbage-collect))



(defun animate ()
  (interactive)
  (draw-frame cube)
  (object-rotate cube)
  )

;; TESTING GROUNDS --------------------------------------

;; (draw-frame cube)
(map! :n "q" 'animate)

(with-current-buffer "cube"
  (erase-buffer))


;; (defun draw ()
;;   (interactive)
;;   (with-current-buffer "cube"
;;     (insert "a\n"))
;;   )
;; (draw)
;; (map! :n "q" 'draw)



(provide 'cube)
;;; cube.el ends here
