;;; org-rich-yank.el --- paste with org-mode markup and link to source -*- lexical-binding: t -*-

;; Copyright (C) 2017 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, hypermedia, org

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Do you often yank source code into your org files, manually
;;; surrounding it in #+BEGIN_SRC blocks? This package will give you a
;;; new way of pasting that automatically surrounds the snippet in
;;; blocks, marked with the major mode of where the code came from,
;;; and adds a link to the source file after the block.

;;; To use, require and bind whatever keys you prefer to the
;;; interactive functions:
;;;
;;; (require 'org-rich-yank)
;;; (define-key org-mode-map (kbd "C-M-y") #'org-rich-yank)
;;;

;;; If you prefer `use-package', the above settings would be:
;;;
;;; (use-package org-rich-yank
;;;   :ensure t
;;;   :config
;;;   (define-key org-mode-map (kbd "C-M-y") #'org-rich-yank))



;;; Code:

(defvar org-rich-yank--buffer nil)

(defun org-rich-yank--store (&rest _args)
  "Store current buffer in `org-rich-yank--buffer'.
ARGS ignored."
  (setq org-rich-yank--buffer (current-buffer)))

(advice-add #'kill-append :after #'org-rich-yank--store)
(advice-add #'kill-new :after #'org-rich-yank--store)

(defun org-rich-yank--trim-nl (str)
  "Trim surrounding newlines from STR."
  (replace-regexp-in-string "\\`[\n\r]+\\|[\n\r]+\\'"
                            ""
                            str))

(defun org-rich-yank--link ()
  "Get an org-link to the current kill."
  (with-current-buffer org-rich-yank--buffer
    (let ((link (with-demoted-errors (org-store-link nil))))
      ;; TODO: make it (file-relative-name … dir-of-org-file) if
      ;; they're in the same projectile-project
      (when link (concat link "\n")))))

(defun org-rich-yank ()
  "Yank, surrounded by #+BEGIN_SRC block with major mode of originating buffer."
  (interactive)
  (insert
   (concat
    (with-current-buffer org-rich-yank--buffer
      (format "#+BEGIN_SRC %s\n"
              (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
    (org-rich-yank--trim-nl (current-kill 0))
    (format "\n#+END_SRC\n")
    (org-rich-yank--link))))


(provide 'org-rich-yank)
;;; org-rich-yank.el ends here
