;;; garbage.el --- Permanent Garbage Memory -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 17, 2025
;; Modified: June 17, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/garbage
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Permanent Garbage Memory
;;
;;; Code:

;; centroid calculated in animation frame
(setq centroid (make-vector 3 0.0))

;; tmp centroid for trangle-inside or triangle-shade
(setq tmp-centroid (make-vector 3 0.0))

;; projected triangle
(setq projected-triangle '([0.0 0.0 0.0]
                           [0.0 0.0 0.0]
                           [0.0 0.0 0.0]))

;; temporary vectors
(setq vec1 [0.0 0.0 0.0])
(setq vec2 [0.0 0.0 0.0])

;; cross product storage
(setq cross [0.0 0.0 0.0])

;; camera-dir
(setq camera-dir [0.0 0.0 0.0])

;; normal
(setq normal [0.0 0.0 0.0])

;; universal iterator
(setq itr 0)

;; z co-ord lagbe kothau
(setq z 12.0)

(provide 'garbage)
;;; garbage.el ends here
