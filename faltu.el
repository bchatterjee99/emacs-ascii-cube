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

;; TESTING GROUNDS ----------------------------------------------------------------------

;; set garbage collection threshold to 1 GB
;; FROM: https://akrl.sdf.org/#orgc15a10d
(setq gc-cons-threshold #x40000000)

gc-elapsed

(garbage-collect)


(setq size 10.0)
(setq shift 20.0)

(setq T (triangle-create (list (- size) (- size) (+ shift)
                                (- size) (+ size) (+ shift)
                                (+ size) (+ size) (+ shift))))

(dolist (v T)
  (print  v))

(garbage-collect)



(provide 'faltu)
;;; faltu.el ends here
