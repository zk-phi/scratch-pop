;;; scratch-pop.el --- popup "scratch"es

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

;; Version: 1.0.1
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; To install, put code like
;;
;;   (require 'scratch-pop)
;;
;; in your .emacs file. You can popup scratch with "M-x scratch-pop". If
;; a scratch is already displayed, another is made. You may bind some keys
;; to "scratch-pop", if you want.

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 better management of multiple scratches
;;       automatically yank region

;;; Code:

;; * constants

(defconst scratch-pop-version "1.0.1")

;; * make another scratch

(defun scratch-pop-get-buffer-create (bufname)
  (or (get-buffer bufname)
      (with-current-buffer (generate-new-buffer bufname)
        (lisp-interaction-mode)
        (current-buffer))))

(defun scratch-pop-get-scratch ()
  (let ((id 1)
        (buflst (mapcar (lambda (w) (window-buffer w)) (window-list)))
        buffer)
    (while (and (setq buffer
                      (scratch-pop-get-buffer-create
                       (concat "*scratch"
                               (unless (= id 1) (int-to-string id))
                               "*")))
                (member buffer buflst))
      (setq id (1+ id)))
    buffer))

;; * popup command

(if (require 'popwin nil t)

;;;###autoload
    (defun scratch-pop ()
      (interactive)
      (let (str)
        (when (use-region-p)
          (setq str (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end))
          (deactivate-mark))
        (popwin:popup-buffer (scratch-pop-get-scratch))
        (goto-char (point-max))
        (when str
          (insert (concat "\n" str "\n")))))

;;;###autoload
  (defun scratch-pop ()
    (interactive)
    (let (str)
      (when (use-region-p)
        (setq str (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (select-window
       (display-buffer (scratch-pop-get-scratch)))
      (goto-char (point-max))
      (when str
        (insert (concat "\n" str "\n")))))
  )

;; * provide

(provide 'scratch-pop)

;;; scratch-pop.el ends here
