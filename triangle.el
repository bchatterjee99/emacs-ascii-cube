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


(load "/mnt/localdisk/__tifr/faltu/lisp/matrix.el")
(load "/mnt/localdisk/__tifr/faltu/lisp/debug.el")

(setq camera-dist 5.0)

(defun  triangle-project (T)
  (let (ans)
    (dolist (v T ans)
      ;; (debug-matrix v 1 3)
      (setq z (aref v 2))
      (setq vertex (make-vector 3 0.0))
      ;; (debug-nl (list  "camera-dist=" camera-dist " z=" z))
      (aset vertex 0 (/ (* camera-dist (aref v 0)) (+ camera-dist z)))
      (aset vertex 1 (/ (* camera-dist (aref v 1)) (+ camera-dist z)))
      (aset vertex 2 0)
      (setq ans (cons vertex ans))
      )
    )
  )

;; (nth 0 cube)
;; (triangle-project (nth 0 cube))

(defun triangle-centroid (T)
  (let ((ans (make-vector 3 0.0)))
    (dolist (v T ans)
      ;; (debug-matrix ans 1 3)
      (aset ans 0 (+ (aref ans 0) (aref v 0)))
      (aset ans 1 (+ (aref ans 1) (aref v 1)))
      (aset ans 2 (+ (aref ans 2) (aref v 2)))
      )
  (aset ans 0 (/ (aref ans 0) 3.0))
  (aset ans 1 (/ (aref ans 1) 3.0))
  (aset ans 2 (/ (aref ans 2) 3.0))
  ans))

;; normalize vector a
(defun normalize (a)
  (setq len (sqrt (+ (expt (aref a 0) 2)
                     (expt (aref a 1) 2)
                     (expt (aref a 2) 2))))
  (aset a 0 (/ (aref a 0) len))
  (aset a 1 (/ (aref a 1) len))
  (aset a 2 (/ (aref a 2) len))
  )

;; returns normlaized cross-product
(defun cross-product (a b)
  (setq c (make-vector 3 0.0))
  (aset c 0 (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1))))
  (aset c 1 (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2))))
  (aset c 2 (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0))))
  (normalize c)
  c)

(defun dot-product (a b)
  (+ (* (aref a 0) (aref b 0))
     (* (aref a 1) (aref b 1))
     (* (aref a 2) (aref b 2)))
  )

(defun triangle-normal (T)
  (setq vec1 (make-vector 3 0))
  (setq vec2 (make-vector 3 0))
  ;; vec1
  (aset vec1 0 (- (aref (nth 1 T) 0) (aref (nth 0 T) 0)))
  (aset vec1 1 (- (aref (nth 1 T) 1) (aref (nth 0 T) 1)))
  (aset vec1 2 (- (aref (nth 1 T) 2) (aref (nth 0 T) 2)))
  ;; vec2
  (aset vec2 0 (- (aref (nth 2 T) 0) (aref (nth 0 T) 0)))
  (aset vec2 1 (- (aref (nth 2 T) 1) (aref (nth 0 T) 1)))
  (aset vec2 2 (- (aref (nth 2 T) 2) (aref (nth 0 T) 2)))
  (cross-product vec1 vec2)
  )


(defun triangle-shade (T)
  (setq camera-dir (make-vector 3 0.0))
  (setq centr (triangle-centroid T))
  (aset camera-dir 0 (aref centr 0))
  (aset camera-dir 1 (aref centr 1))
  (aset camera-dir 2 (+ (aref centr 2) camera-dist))
  (normalize camera-dir)
  (floor (* (1- (length tileset))
            (abs (dot-product camera-dir (triangle-normal T)))))
  )


(defun triangle-inside (T px py)
  (setq centr (triangle-centroid T))
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

(defun triangle-create (L)
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


(setq  triangle1 '([0.0 0.0 5.0]
                   [0.0 1.0 5.0]
                   [1.0 0.0 5.0]))
(triangle-normal triangle1)
(triangle-project triangle1)
(setq A1 '([1.0 2.0 3.0]
           [7.0 9.0 8.0]
           [6.0 7.0 6.0]))
A1
(triangle-normal A1)
(triangle-centroid A1)

(setq A1 '([1.0 2.0 0.0]
           [7.0 9.0 0.0]
           [6.0 7.0 0.0]))
(setq cnt (triangle-centroid A1))
(triangle-inside A1 (aref cnt 0) (aref cnt 1))

(triangle-shade A1)
;;(insert 79)

(triangle-create (list 1 2 3
                       4 5 6
                       7 8 9))


;; (setq vec1 [1.0 2.0 3.0])
;; (setq vec2 [4.0 5.0 6.0])
;; (dot-product vec1 vec2)
;; (dot-product vec1 (cross-product vec1 vec2))


(provide 'triangle)
;;; triangle.el ends here
