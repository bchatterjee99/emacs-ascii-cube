;;; debug.el --- debugging -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 11, 2025
;; Modified: June 11, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/debug
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun debug-start ()
  (generate-new-buffer "error")
  (evil-window-vsplit))

(defun debug-main (list)
  (with-current-buffer "error"
    (end-of-buffer)
    (let (value)
      (dolist (element list value)
        (if (stringp element)
            (progn (insert element)
                   (insert " "))
          (progn (insert (number-to-string element))
                 (insert " ")))))))

(defun debug-nl (list)
  (with-current-buffer "error"
    (end-of-buffer)
    (let (value)
      (dolist (element list value)
        (if (stringp element)
            (progn (insert element)
                   (insert " "))
          (progn (insert (number-to-string element))
                 (insert " ")))))
    (insert "\n")))


(defun debug-matrix (A row col)
  (dotimes (i row)
    (dotimes (j col)
      (debug-main (list (mref A row col i j))))
    (debug-nl ()))
  (debug-nl (list "-----")))

(defun debug-triangle (T)
  (debug-matrix (nth 0 T) 1 3)
  (debug-matrix (nth 1 T) 1 3)
  (debug-matrix (nth 2 T) 1 3)
  (debug-nl (list  "================\n"))
  )


;;; debug.el ends here
