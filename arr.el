;;; arr.el --- Array experiments -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Bikshan Chatterjee
;;
;; Author: Bikshan Chatterjee <bikhon@pop-os>
;; Maintainer: Bikshan Chatterjee <bikhon@pop-os>
;; Created: June 16, 2025
;; Modified: June 16, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bikhon/arr
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun aaa ()
  (setq arr [0 0 0])
  (print arr)
  (aset arr 0 (+ (aref arr 0) 1))
  arr
  )

(aaa)

(provide 'arr)
;;; arr.el ends here
