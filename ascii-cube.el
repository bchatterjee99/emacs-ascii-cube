;;; ascii-cube.el --- Ascii Cube -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 19, 2025
;; Modified: June 19, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/ascii-cube
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Ascii Cube
;;
;;; Code:

;; Settings -------------------------------------------
(setq ascii-cube-new-window t)
(setq ascii-cube-half-screen-width 15)
(setq ascii-cube-half-screen-height 15)
(setq ascii-cube-light-dir [-10 -10 10])
(setq ascii-cube-shift 20.0)
;; half of size of cube side
(setq ascii-cube-size 10.0)

;; Settings -------------------------------------------

;; garbage.el -----------------------------------------
;; centroid calculated in animation frame
(setq ascii-cube-centroid (make-vector 3 0.0))
;; tmp centroid for trangle-inside or triangle-shade
(setq ascii-cube-tmp-centroid (make-vector 3 0.0))
;; projected triangle
(setq ascii-cube-projected-triangle '([0.0 0.0 0.0]
                           [0.0 0.0 0.0]
                           [0.0 0.0 0.0]))
;; temporary vectors
(setq ascii-cube-vec1 [0.0 0.0 0.0])
(setq ascii-cube-vec2 [0.0 0.0 0.0])
;; cross product storage
(setq ascii-cube-cross [0.0 0.0 0.0])
;; camera-dir
(setq ascii-cube-camera-dir [0.0 0.0 0.0])
;; reflected-ray
(setq ascii-cube-reflected-ray [0.0 0.0 0.0])
;; z-min
(setq ascii-cube-z_min 100.0)
;; collision
(setq ascii-cube-collision 0)
;; global co-ord
(setq ascii-cube-x 0)
(setq ascii-cube-y 0)
;; garbage.el -----------------------------------------

;; matrix.el -----------------------------------
(defun ascii-cube-matrix (i j)
  (make-vector (* i j) 0))
(defun ascii-cube-mref (A row col i j)
  (aref A (+ (* i col) j)))
(defun ascii-cube-mset (A row col i j val)
  (aset A (+ (* i col) j)
        val))

(defun ascii-cube-matrix-mult (A B row1 col1 row2 col2)
  ;; (debug-nl (list "entered ascii-cube-matrix-mult" row1 col1 row2 col2))
(setq C (ascii-cube-matrix row1 col2))
(dotimes (i row1)
  (dotimes (j col2)
    (dotimes (k col1)
      (ascii-cube-mset C row1 col2 i j (+ (ascii-cube-mref C row1 col2 i j)
                               (* (ascii-cube-mref A row1 col1 i k) (ascii-cube-mref B row2 col2 k j))))
      ;; (debug-matrix C row1 col2)
      )))
C)

(defun ascii-cube-matrix-create (arr row col)
  (setq M (ascii-cube-matrix row col))
  (setq i 0)
  (dolist (x arr)
    (aset M i x)
    (setq i (1+ i)))
  M)
;; matrix.el -----------------------------------

;; triangle.el----------------------------------
(setq ascii-cube-tileset ".::!!!@@@####")
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
  (ascii-cube-triangle-reset-vector ascii-cube-centroid)
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
  (ascii-cube-cross-product ascii-cube-vec1 ascii-cube-vec2))

(defun ascii-cube-vec-negate (vec)
  (aset vec 0 (- (aref vec 0)))
  (aset vec 1 (- (aref vec 1)))
  (aset vec 2 (- (aref vec 2)))
  vec)

(defun ascii-cube-reflected-ray (T)
  (setq normal (ascii-cube-triangle-normal T))
  (setq dot (ascii-cube-dot-product normal ascii-cube-light-dir))
  ;; (if (> dot 0)
  ;;    (ascii-cube-vec-negate normal)) ;; multiplying with dot takes care of dir
  ;; (ascii-cube-normalize normal) ;; already normalized
  (aset ascii-cube-reflected-ray 0 (- (aref ascii-cube-light-dir 0)
                                      (* dot (aref normal 0))
                                      (* dot (aref normal 0))))
  (aset ascii-cube-reflected-ray 1 (- (aref ascii-cube-light-dir 1)
                                      (* dot (aref normal 1))
                                      (* dot (aref normal 1))))
  (aset ascii-cube-reflected-ray 2 (- (aref ascii-cube-light-dir 2)
                                      (* dot (aref normal 2))
                                      (* dot (aref normal 2))))
  (ascii-cube-normalize ascii-cube-reflected-ray)
  ascii-cube-reflected-ray)

(defun ascii-cube-non-zero (a)
  (if (< a 0) 0 a))


(defun ascii-cube-triangle-shade (T)
  (ascii-cube-triangle-centroid-tmp T)
  ;; (aset ascii-cube-camera-dir 0 (aref ascii-cube-tmp-centroid 0))
  ;; (aset ascii-cube-camera-dir 1 (aref ascii-cube-tmp-centroid 1))
  ;; (aset ascii-cube-camera-dir 2 (+ (aref ascii-cube-tmp-centroid 2) ascii-cube-camera-dist))
  (aset ascii-cube-camera-dir 0 0)
  (aset ascii-cube-camera-dir 1 0)
  (aset ascii-cube-camera-dir 2 -1)
  (ascii-cube-normalize ascii-cube-camera-dir)
  (floor (* (1- (length ascii-cube-tileset))
            (ascii-cube-non-zero (ascii-cube-dot-product ascii-cube-camera-dir (ascii-cube-reflected-ray T)))))
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


;; check if (px, py) is inside triangle T
(defun ascii-cube-triangle-inside (T px py)
  (ascii-cube-triangle-centroid-tmp T)
  (setq cx (aref ascii-cube-tmp-centroid 0))
  (setq cy (aref ascii-cube-tmp-centroid 1))
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
;; triangle.el----------------------------------

;; cube.el -------------------------------------

(setq ascii-cube-T1 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                 (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift))))
(setq ascii-cube-T2 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift))))
(setq ascii-cube-T3 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                                 (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T4 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T5 (ascii-cube-triangle-create (list
                                                 (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T6 (ascii-cube-triangle-create (list
                                                 (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                 (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))


(setq ascii-cube-T7 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T8 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (+ ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T9 (ascii-cube-triangle-create (list
                                                 (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                 (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size)
                                                 (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T10 (ascii-cube-triangle-create (list
                                                  (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                  (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                  (+ ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T11 (ascii-cube-triangle-create (list
                                                  (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
                                                  (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift)
                                                  (- ascii-cube-size) (+ ascii-cube-size) (+ ascii-cube-shift ascii-cube-size ascii-cube-size))))
(setq ascii-cube-T12 (ascii-cube-triangle-create (list
                                                  (- ascii-cube-size) (- ascii-cube-size) (+ ascii-cube-shift)
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

;;;; garbage collection
;; gc-elapsed
;; (setq garbage-collection-messages nil)
(setq ascii-cube-new-window t)


              ;;                         ########
              ;;             ####################
              ;;       ##########################
              ;;   ......########################
              ;; ........##########################
              ;; ........##########################
              ;; ........##########################
              ;; ..........########################
              ;; ..........##########################
              ;;   ........##########################
              ;;   ........##########################
              ;;   ..........########################
              ;;   ..........##########################
              ;;     ........######################
              ;;     ........##############
              ;;     ..........######


(defun ascii-cube-triangle-collision (T)
  (setq centr_z (aref (ascii-cube-triangle-centroid T) 2))
  (if (and (< centr_z ascii-cube-z_min)
           (ascii-cube-triangle-inside (ascii-cube-triangle-project T) ascii-cube-x ascii-cube-y))
      (progn (setq ascii-cube-collision (ascii-cube-triangle-shade T))
             (setq ascii-cube-z_min centr_z))))

(defun ascii-cube-draw-frame (object)
  (with-current-buffer "ascii-cube"
    (erase-buffer)
      (dotimes (i (* 2 ascii-cube-half-screen-width))
        (dotimes (j (* 2 ascii-cube-half-screen-width))
          (setq ascii-cube-z_min 100.0)
          (setq ascii-cube-x (- j ascii-cube-half-screen-width))
          (setq ascii-cube-y (- ascii-cube-half-screen-height i))
          (setq ascii-cube-collision -1)
          (mapc #'ascii-cube-triangle-collision object)
          ;; (dolist (triangle object)
            ;; (debug-nl (list "dolist  " itr))
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
  (ascii-cube-draw-frame ascii-cube)
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
;; cube.el -------------------------------------

(defun ascii-cube-close ()
  (interactive)
  (if ascii-cube-new-window
      (kill-buffer-and-window)
    (kill-buffer "ascii-cube")))


(setq ascii-cube-keymap (make-sparse-keymap))
(define-key ascii-cube-keymap "w" #'ascii-cube-animate-up)
(define-key ascii-cube-keymap "s" #'ascii-cube-animate-down)
(define-key ascii-cube-keymap "a" #'ascii-cube-animate-z-left)
(define-key ascii-cube-keymap "d" #'ascii-cube-animate-z-right)
(define-key ascii-cube-keymap "q" #'ascii-cube-animate-left)
(define-key ascii-cube-keymap "e" #'ascii-cube-animate-right)
(define-key ascii-cube-keymap "x" #'ascii-cube-close)


;; (setq ascii-cube-keymap (define-keymap
;;                      "w" #'ascii-cube-animate-up
;;                      "s" #'ascii-cube-animate-down
;;                      "a" #'ascii-cube-animate-z-left
;;                      "d" #'ascii-cube-animate-z-right
;;                      "q" #'ascii-cube-animate-left
;;                      "e" #'ascii-cube-animate-right
;;                      "x" #'kill-buffer-and-window))

(defun ascii-cube ()
  (interactive)
  ;; (kill-buffer "ascii-cube")
  (delete-other-windows)
  (if ascii-cube-new-window
      (progn (split-window nil (- (window-total-width) (* 2 ascii-cube-half-screen-width) 30) t)
             (other-window 1)))
  (get-buffer-create "ascii-cube")
  (set-window-buffer nil "ascii-cube")
  (ascii-cube-rotate ascii-cube 1)
  (ascii-cube-rotate ascii-cube 1)
  (ascii-cube-rotate ascii-cube 5)
  (ascii-cube-rotate ascii-cube 5)
  (ascii-cube-draw-frame ascii-cube))

(provide 'ascii-cube)
;;; ascii-cube.el ends here
