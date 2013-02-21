;;; scratch-pop --- popup scratch

;; Copyright (C) 2012 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Version: 1.0.0
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; To install, put code like
;;
;;   (require 'scratch-pop)
;;
;; in your .emacs file. You can popup scratch with "M-x scratch-pop". If
;; scratch is already displayed, another scratch is made. You may bind
;; "scratch-pop" to some keys, if you want.

;;; Change Log:

;; 1.0.0 first released

;;; Code:

;; * constants

(defconst scratch-pop-version "1.0.1")

;; * configures

(defconst scratch-pop-popwin-available (require 'popwin nil t))

;; * making another scratch

(defvar scratch-pop-another-scratch nil)

(defun scratch-pop-another-scratch ()
  (when (not (and scratch-pop-another-scratch
                  (buffer-live-p scratch-pop-another-scratch)))
    (setq scratch-pop-another-scratch (get-buffer-create "*scratch2*"))
    (set-buffer scratch-pop-another-scratch)
    (lisp-interaction-mode))
  scratch-pop-another-scratch)

;; * popup command

(if scratch-pop-popwin-available

    ;; popwin version
    (defun scratch-pop ()
      (interactive)
      (if (and popwin:popup-buffer
               (member (buffer-name popwin:popup-buffer)
                       '("*scratch*" "*scratch2*")))
          (popwin:close-popup-window)
        (popwin:popup-buffer
         (if (member "*scratch*"
                     (mapcar (lambda (w) (buffer-name (window-buffer w)))
                             (window-list)))
             (scratch-pop-another-scratch) "*scratch*"))))

  ;; display-buffer version
  (defun scratch-pop ()
    (interactive)
    (select-window
     (display-buffer
      (if (member "*scratch*"
                  (mapcar (lambda (w) (buffer-name (window-buffer w)))
                          (window-list)))
          (scratch-pop-another-scratch) "*scratch*"))))
  )

;; * provide

(provide 'scratch-pop)

;; scratch-pop.el ends here
