;;; scratch-pop.el --- popup "scratch"es

;; Copyright (C) 2012-2015 zk_phi

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

;; Version: 2.0.0
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Package-Requires: ((popwin "0.7.0alpha"))

;;; Commentary:

;; Load this script
;;
;;   (require 'scratch-pop)
;;
;; and you can popup scratch with "M-x scratch-pop". If a scratch is
;; already displayed, another scratch buffer is created. You may also
;; bind some keys to "scratch-pop", if you want.
;;
;;   (global-set-key "C-M-s" 'scratch-pop)
;;
;; When called with region, the region is yanked to the scratch.

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 better management of multiple scratches
;;       automatically yank region
;; 1.0.2 better handling of popup window
;; 1.0.3 require popwin
;; 2.0.0 change scratch buffer selection algorithm

;;; Code:

(require 'popwin)
(require 'edmacro)

(defconst scratch-pop-version "2.0.0")

;; internal vars
(defvar scratch-pop--next-scratch-id nil) ; Int
(defvar scratch-pop--visible-buffers nil) ; List[Buffer]

(defun scratch-pop--get-next-scratch ()
  (let* ((name (concat "*scratch"
                       (unless (= scratch-pop--next-scratch-id 1)
                         (int-to-string scratch-pop--next-scratch-id))
                       "*"))
         (buf (get-buffer name)))
    (setq scratch-pop--next-scratch-id (1+ scratch-pop--next-scratch-id))
    (cond ((null buf)
           (with-current-buffer (generate-new-buffer name)
             (funcall initial-major-mode)
             (current-buffer)))
          ((memq buf scratch-pop--visible-buffers)
           (scratch-pop--get-next-scratch))
          (t
           buf))))

;;;###autoload
(defun scratch-pop ()
  (interactive)
  (setq scratch-pop--next-scratch-id 1
        scratch-pop--visible-buffers (mapcar 'window-buffer (window-list)))
  (let ((str (when (use-region-p)
               (prog1 (buffer-substring (region-beginning) (region-end))
                 (delete-region (region-beginning) (region-end))
                 (deactivate-mark))))
        (repeat-key (vector last-input-event)))
    (popwin:popup-buffer (scratch-pop--get-next-scratch))
    (when str
      (goto-char (point-max))
      (insert (concat "\n" str "\n")))
    (message "(Type %s to repeat)" (edmacro-format-keys repeat-key))
    (set-temporary-overlay-map
     (let ((km (make-sparse-keymap))
           (cycle-fn (lambda ()
                       (interactive)
                       (with-selected-window popwin:popup-window
                         (switch-to-buffer (scratch-pop--get-next-scratch))))))
       (define-key km repeat-key cycle-fn)
       km) t)))

(provide 'scratch-pop)

;;; scratch-pop.el ends here
