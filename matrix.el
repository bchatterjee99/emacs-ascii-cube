;;; matrix.el --- Matrices -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 12, 2025
;; Modified: June 12, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/matrix
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


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

;; TESTING GROUNDS -------------------------------------------------------

;; (ascii-cube-matrix-create '(0 0 0 0) 2 2)

(provide 'matrix)
;;; matrix.el ends here
