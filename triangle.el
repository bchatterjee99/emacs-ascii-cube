;;; triangle.el --- Triangles -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 15, 2025
;; Modified: June 15, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/triangle
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(setq ascii-cube-camera-dist 50.0)

(defun  ascii-cube-triangle-project (T)
  (setq itr 0)
    (dolist (v T)
      ;; (debug-matrix v 1 3)
      ;;(debug-nl (list z))
      (setq z (aref v 2))
      ;; (debug-nl (list  "ascii-cube-camera-dist=" ascii-cube-camera-dist " z=" z))
      (aset (nth itr ascii-cube-projected-triangle) 0 (/ (* ascii-cube-camera-dist (aref v 0)) (+ ascii-cube-camera-dist z)))
      (aset (nth itr ascii-cube-projected-triangle) 1 (/ (* ascii-cube-camera-dist (aref v 1)) (+ ascii-cube-camera-dist z)))
      (aset (nth itr ascii-cube-projected-triangle) 2 0)
      (setq itr (1+ itr))
      )
  ascii-cube-projected-triangle)

;; reset vector to [0 0 0]
(defun ascii-cube-triangle-reset-vector (v)
  (aset v 0 0.0)
  (aset v 1 0.0)
  (aset v 2 0.0)
  )

;; calculate centroid; store in centroid
(defun ascii-cube-triangle-centroid (T)
  (ascii-cube-triangle-reset-vector centroid)
  (dolist (v T)
    ;; (debug-matrix ans 1 3)
    (aset ascii-cube-centroid 0 (+ (aref ascii-cube-centroid 0) (aref v 0)))
    (aset ascii-cube-centroid 1 (+ (aref ascii-cube-centroid 1) (aref v 1)))
    (aset ascii-cube-centroid 2 (+ (aref ascii-cube-centroid 2) (aref v 2)))
    )
  (aset ascii-cube-centroid 0 (/ (aref ascii-cube-centroid 0) 3.0))
  (aset ascii-cube-centroid 1 (/ (aref ascii-cube-centroid 1) 3.0))
  (aset ascii-cube-centroid 2 (/ (aref ascii-cube-centroid 2) 3.0))
  ascii-cube-centroid)

;; calculate centroid; store in tmp-centroid [?]
(defun ascii-cube-triangle-centroid-tmp (T)
  (ascii-cube-triangle-reset-vector ascii-cube-tmp-centroid)
    (dolist (v T)
      ;; (debug-matrix ans 1 3)
      (aset ascii-cube-tmp-centroid 0 (+ (aref ascii-cube-tmp-centroid 0) (aref v 0)))
      (aset ascii-cube-tmp-centroid 1 (+ (aref ascii-cube-tmp-centroid 1) (aref v 1)))
      (aset ascii-cube-tmp-centroid 2 (+ (aref ascii-cube-tmp-centroid 2) (aref v 2)))
      )
  (aset ascii-cube-tmp-centroid 0 (/ (aref ascii-cube-tmp-centroid 0) 3.0))
  (aset ascii-cube-tmp-centroid 1 (/ (aref ascii-cube-tmp-centroid 1) 3.0))
  (aset ascii-cube-tmp-centroid 2 (/ (aref ascii-cube-tmp-centroid 2) 3.0))
  ascii-cube-tmp-centroid)

;; normalize vector a
(defun ascii-cube-normalize (a)
  (setq len (sqrt (+ (expt (aref a 0) 2)
                     (expt (aref a 1) 2)
                     (expt (aref a 2) 2))))
  (aset a 0 (/ (aref a 0) len))
  (aset a 1 (/ (aref a 1) len))
  (aset a 2 (/ (aref a 2) len))
  )

;; returns normlaized cross-product
(defun ascii-cube-cross-product (a b)
  (aset ascii-cube-cross 0 (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1))))
  (aset ascii-cube-cross 1 (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2))))
  (aset ascii-cube-cross 2 (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0))))
  (ascii-cube-normalize ascii-cube-cross)
  ascii-cube-cross)

(defun ascii-cube-dot-product (a b)
  (+ (* (aref a 0) (aref b 0))
     (* (aref a 1) (aref b 1))
     (* (aref a 2) (aref b 2)))
  )

(defun ascii-cube-triangle-normal (T)
  ;; vec1
  (aset ascii-cube-vec1 0 (- (aref (nth 1 T) 0) (aref (nth 0 T) 0)))
  (aset ascii-cube-vec1 1 (- (aref (nth 1 T) 1) (aref (nth 0 T) 1)))
  (aset ascii-cube-vec1 2 (- (aref (nth 1 T) 2) (aref (nth 0 T) 2)))
  ;; vec2
  (aset ascii-cube-vec2 0 (- (aref (nth 2 T) 0) (aref (nth 0 T) 0)))
  (aset ascii-cube-vec2 1 (- (aref (nth 2 T) 1) (aref (nth 0 T) 1)))
  (aset ascii-cube-vec2 2 (- (aref (nth 2 T) 2) (aref (nth 0 T) 2)))
  (ascii-cube-cross-product ascii-cube-vec1 ascii-cube-vec2)
  )


(defun ascii-cube-triangle-shade (T)
  (ascii-cube-triangle-centroid-tmp T)
  (aset ascii-cube-camera-dir 0 (aref tmp-centroid 0))
  (aset ascii-cube-camera-dir 1 (aref tmp-centroid 1))
  (aset ascii-cube-camera-dir 2 (+ (aref tmp-centroid 2) ascii-cube-camera-dist))
  (ascii-cube-normalize ascii-cube-camera-dir)
  (floor (* (1- (length ascii-cube-tileset))
            (abs (ascii-cube-dot-product ascii-cube-camera-dir (ascii-cube-triangle-normal T)))))
  )

(defun ascii-cube-triangle-inside-div (T px py)
  (setq centr (ascii-cube-triangle-centroid-tmp T))
  (setq cx (aref centr 0))
  (setq cy (aref centr 1))
  (setq x1 (aref (nth 0 T) 0))
  (setq y1 (aref (nth 0 T) 1))
  (setq x2 (aref (nth 1 T) 0))
  (setq y2 (aref (nth 1 T) 1))
  (setq x3 (aref (nth 2 T) 0))
  (setq y3 (aref (nth 2 T) 1))
  (and
   (<= 0 (*
              (- (/ (- cx x1) (- x2 x1))
                 (/ (- cy y1) (- y2 y1)))
              (- (/ (- px x1) (- x2 x1))
                 (/ (- py y1) (- y2 y1)))
              ))
   (<= 0 (*
              (- (/ (- cx x2) (- x3 x2))
                 (/ (- cy y2) (- y3 y2)))
              (- (/ (- px x2) (- x3 x2))
                 (/ (- py y2) (- y3 y2)))
              ))
   (<= 0 (*
              (- (/ (- cx x3) (- x1 x3))
                 (/ (- cy y3) (- y1 y3)))
              (- (/ (- px x3) (- x1 x3))
                 (/ (- py y3) (- y1 y3)))
              ))
       )
  )


(defun ascii-cube-triangle-inside (T px py)
  (ascii-cube-triangle-centroid-tmp T)
  (setq cx (aref tmp-centroid 0))
  (setq cy (aref tmp-centroid 1))
  (setq x1 (aref (nth 0 T) 0))
  (setq y1 (aref (nth 0 T) 1))
  (setq x2 (aref (nth 1 T) 0))
  (setq y2 (aref (nth 1 T) 1))
  (setq x3 (aref (nth 2 T) 0))
  (setq y3 (aref (nth 2 T) 1))
  (and
   (< 0 (*
              (- (* (- cx x1) (- y2 y1))
                 (* (- cy y1) (- x2 x1)))
              (- (* (- px x1) (- y2 y1))
                 (* (- py y1) (- x2 x1)))
              ))
   (< 0 (*
              (- (* (- cx x2) (- y3 y2))
                 (* (- cy y2) (- x3 x2)))
              (- (* (- px x2) (- y3 y2))
                 (* (- py y2) (- x3 x2)))
              ))
   (< 0 (*
              (- (* (- cx x3) (- y1 y3))
                 (* (- cy y3) (- x1 x3)))
              (- (* (- px x3) (- y1 y3))
                 (* (- py y3) (- x1 x3)))
              ))
       )
  )

(defun ascii-cube-triangle-create (L)
  (setq T ())
  (setq vert (make-vector 3 0.0))
  (aset vert 0 (nth 0 L))
  (aset vert 1 (nth 1 L))
  (aset vert 2 (nth 2 L))
  (setq T (cons vert T))
  (setq vert (make-vector 3 0.0))
  (aset vert 0 (nth 3 L))
  (aset vert 1 (nth 4 L))
  (aset vert 2 (nth 5 L))
  (setq T (cons vert T))
  (setq vert (make-vector 3 0.0))
  (aset vert 0 (nth 6 L))
  (aset vert 1 (nth 7 L))
  (aset vert 2 (nth 8 L))
  (setq T (cons vert T))
  )



;; TESTING GROUNDS ---------------------------------------


;; (setq  triangle1 '([0.0 0.0 5.0]
;;                    [0.0 1.0 5.0]
;;                    [1.0 0.0 5.0]))
;; (triangle-normal triangle1)
;; (triangle-project triangle1)
;; (setq A1 '([1.0 2.0 3.0]
;;            [7.0 9.0 8.0]
;;            [6.0 7.0 6.0]))
;; A1

;; (triangle-normal A1)
;; (triangle-centroid triangle1)
;; (triangle-project A1)

;; (setq A1 '([1.0 2.0 0.0]
;;            [7.0 9.0 0.0]
;;            [6.0 7.0 0.0]))
;; (setq cnt (triangle-centroid A1))
;; (triangle-inside A1 (aref cnt 0) (aref cnt 1))

;; (triangle-shade A1)
;; ;;(insert 79)

;; (triangle-create (list 1 2 3
;;                        4 5 6
;;                        7 8 9))


;; (setq vec1 [1.0 2.0 3.0])
;; (setq vec2 [4.0 5.0 6.0])
;; (dot-product vec1 vec2)
;; (dot-product vec1 (cross-product vec1 vec2))


(provide 'triangle)
;;; triangle.el ends here
