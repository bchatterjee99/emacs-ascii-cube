;;; faltu.el --- Main file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 11, 2025
;; Modified: June 11, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/faltu
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


;; TESTING GROUNDS ----------------------------------------------------------------------

(setq row 3)
(setq col 3)
(setq mat1 [1 2
           4 5])
(setq mat2 [1 3
           0 1])
(setq mat3 (matrix-mult mat1 mat2 2 2 2 2))


(debug-matrix mat1 2 2)
(debug-matrix mat2 2 2)
(debug-matrix mat3 2 2)



(provide 'faltu)
;;; faltu.el ends here
