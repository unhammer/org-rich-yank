;;; org-rich-yank.el --- Paste with org-mode markup and link to source -*- lexical-binding: t -*-

;; Copyright (C) 2018-2026 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.4.0
;; URL: https://github.com/unhammer/org-rich-yank
;; Package-Requires: ((emacs "25.1"))
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
;;; blocks, marked with the language of the major mode of where the
;;; code came from, and adds a link to the source file after the
;;; block.

;;; To use, require and bind whatever keys you prefer to the
;;; interactive functions:
;;;
;;; (require 'org-rich-yank)
;;; (eval-after-load 'org
;;;   '(define-key org-mode-map (kbd "C-M-y") #'org-rich-yank)))
;;;

;;; If you prefer `use-package', the above settings would be:
;;;
;;; (use-package org-rich-yank
;;;   :ensure t
;;;   :config
;;;   (eval-after-load 'org
;;;     '(define-key org-mode-map (kbd "C-M-y") #'org-rich-yank)))
;;;
;;; Note that we eagerly load `org-rich-yank', so we can capture yanks
;;; that happen before `org' is loaded.

;;; This package can also store links coming from outside,
;;; e.g. Firefox or LibreOffice.  Note that if you modify
;;; `interprogram-paste-function', you have to enable `org-rich-yank'
;;; *after* doing so (or at least call `org-rich-yank-enable' again
;;; after modifying `interprogram-paste-function').


;;; Code:

(autoload 'org-store-link "ol")
(autoload 'org-escape-code-in-string "org-src")
(autoload 'org-src--on-datum-p "org-src")
(autoload 'org-element-at-point "org-element")
(autoload 'org-element-type "org-element")
(autoload 'org-element-property "org-element")
(autoload 'org-download-clipboard "org-download")
(autoload 'yank-media-types--format "yank-media") ; optional; appeared in emacs-29.0.90

(defgroup org-rich-yank nil
  "Options for `org-rich-yank'."
  :tag "org-rich-yank"
  :group 'org)

(defcustom org-rich-yank-add-target-indent t
  "Give all lines of paste the same indentation as the first one.
If this variable is non-nil and point is indented before pasting,
all lines below will also get that indentation."
  :group 'org-rich-yank
  :type 'boolean)

(defcustom org-rich-yank-format-paste #'org-rich-yank--format-paste-default
  "A function to format current paste as an org source block.
See `org-rich-yank--format-paste-default' for example and expected arguments."
  :group 'org-rich-yank
  :type 'function)

(defcustom org-rich-yank-guess-interprogram-lang #'org-rich-yank--guess-interprogram-lang
  "A function to guess the language of a paste if from outside this Emacs.
Takes a single argument, the current paste.  Should return either a
string with the language to use in the org src block, or nil to treat
the paste as a quote instead of src."
  :group 'org-rich-yank
  :type 'function)

(defcustom org-rich-yank-download-image t
  "Whether to use `org-download-clipboard' when clipboard contains image."
  :group 'org-rich-yank
  :type 'boolean)

(defcustom org-rich-yank--clipboard-link-mime-types
  '(text/x-moz-url-priv
    chromium/x-source-url
    application/x-openoffice-link\;windows_formatname=\"Link\"
    text/uri-list)
  "Mime types used by common programs to store link/url metadata in X CLIPBOARD."
  :group 'org-rich-yank
  :type '(repeat symbol))

(defun org-rich-yank--get-lang ()
  "Find source language of current kill.
Typically language of buffer major mode, but org source blocks
should for example use the mode of their block, instead of
\"org\"."
  ;; Needs to be done when copying, not when pasting, since we look for org src block language
  (if-let* ((element (and (eq major-mode 'org-mode)
                          (org-element-at-point)))
            (type (and (org-src--on-datum-p element) ; o/w takes effect after #+end_src too
                       (org-element-type element)))
            (lang (and (eq type 'src-block)
                       (org-element-property :language element))))
      lang
    (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))

(defun org-rich-yank--guess-interprogram-lang (string)
  "Ignores STRING and returns nil (treat as quote instead of src).
Used as default for `org-rich-yank-guess-interprogram-lang'."
  nil)

(defun org-rich-yank--store (&rest _args)
  "Store metadata on head of `kill-ring'.

Current buffer is stored in `org-rich-yank-buffer', major mode in
`org-rich-yank-lang'.

Stores nothing on it if it already has data (e.g. from
`org-rich-yank--wrap-interprogram-paste')."
  (when-let* ((head (and kill-ring
                         (stringp (car kill-ring))
                         (car kill-ring))))
    ;; Only if no existing origin property:
    (unless (get-text-property 0 'org-rich-yank-origin head)
      (add-text-properties
       0 (length head)
       (list 'org-rich-yank-origin 'this-emacs
             'org-rich-yank-buffer (current-buffer) ; if from this emacs, store buffer
             'org-rich-yank-lang (org-rich-yank--get-lang))
       head))))


(defun org-rich-yank--wrap-interprogram-paste (orig-fun)
  "Store that the current paste has interprogram origin.
Used as advice where ORIG-FUN is `interprogram-paste-function'."
  (when-let* ((res (funcall orig-fun))
              (strings (if (listp res)
                           res
                         (list res))))
    (mapcar (lambda (string)
              (add-text-properties 0
                                   (length string)
                                   (list 'org-rich-yank-origin 'interprogram
                                         'org-rich-yank-lang (funcall org-rich-yank-guess-interprogram-lang string)
                                         'org-rich-yank-link (org-rich-yank--get-X-clipboard-link))
                                   string)
              string)
            strings)))

;;;###autoload
(defun org-rich-yank-enable ()
  "Add the advices that store the buffer of the current kill."
  (advice-add interprogram-paste-function :around #'org-rich-yank--wrap-interprogram-paste)
  (advice-add #'kill-append :after #'org-rich-yank--store)
  (advice-add #'kill-new :after #'org-rich-yank--store))

;; Always do this on load – safe to run multiple times
(org-rich-yank-enable)

(defun org-rich-yank-disable ()
  "Remove the advices that store the buffer of the current kill."
  (advice-remove interprogram-paste-function #'org-rich-yank--wrap-interprogram-paste)
  (advice-remove #'kill-append #'org-rich-yank--store)
  (advice-remove #'kill-new #'org-rich-yank--store))

(defun org-rich-yank--trim-nl (str)
  "Trim surrounding newlines from STR."
  (replace-regexp-in-string "\\`[\n\r]+\\|[\n\r]+\\'"
                            ""
                            str))

(declare-function diff-goto-source "diff-mode")
(declare-function gnus-article-show-summary "gnus-art")

(defun org-rich-yank--store-link ()
  "Store the link using `org-store-link' without erroring out."
  ;; TODO: make it (file-relative-name … dir-of-org-file) if they're
  ;; in the same project
  (with-demoted-errors "Error in org-rich-yank--store-link: %S"
      (cond ((and (eq major-mode 'gnus-article-mode)
                  (fboundp #'gnus-article-show-summary))
             ;; Workaround for possible bug in org-gnus-store-link: If
             ;; you've moved point in the summary, org-store-link from
             ;; the article will give the wrong link
             (save-window-excursion (gnus-article-show-summary)
                                    (org-store-link nil)))
            ((and (eq major-mode 'diff-mode))
             (save-window-excursion
               (diff-goto-source)
               (org-store-link nil)))
            ;; org-store-link doesn't do eww-mode yet as of 8.2.10 at least:
            ((and (eq major-mode 'eww-mode)
                  (boundp 'eww-data)
                  (plist-get eww-data :url))
             (format "[[%s][%s]]"
                     (plist-get eww-data :url)
                     (or (plist-get eww-data :title)
                         (plist-get eww-data :url))))
            (t (org-store-link nil)))))

(defun org-rich-yank--get-X-clipboard-link ()
  "Search X gui CLIPBOARD selection for data with an url mime type.
Common url mime types defined in `org-rich-yank--clipboard-link-mime-types'.

If found, sets `org-rich-yank--lang' to nil, for quote formatting."
  (when-let* ((data-types (gui-get-selection 'CLIPBOARD 'TARGETS))
              (data-type (and (vectorp data-types)
                              (seq-find
                               (lambda (dt) (memq dt org-rich-yank--clipboard-link-mime-types))
                               data-types)))
              (data (gui-get-selection 'CLIPBOARD data-type))
              (link-data (yank-media-types--format data-type data))
              ;; TODO: Customizable list of GUI link-cleaner functions:
              (formatted-link (if (eq data-type 'application/x-openoffice-link\;windows_formatname=\"Link\")
                                  (when-let* ((path (nth 1 (split-string link-data "\0"))))
                                    (format "[[file://%s]]" path))
                                link-data)))
    formatted-link))

(defun org-rich-yank--link (kill)
  "Get an org-link to KILL."
  (or
   ;; If this emacs instance has recorded a link, prefer that:
   (when-let* ((link (get-text-property 0 'org-rich-yank-link kill)))
     (concat link "\n"))
   ;; If we have a buffer, use org-mode to store a link and use that:
   (when-let* ((buffer (get-text-property 0 'org-rich-yank-buffer kill)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when-let* ((link (org-rich-yank--store-link)))
           (concat link "\n")))))
   ;; Don't insert "nil" if no link found:
   ""))

(defun org-rich-yank-indent (paste)
  "Prepend current indentation to each line of PASTE."
  (let* ((s (buffer-substring (line-beginning-position) (point)))
         (indent (progn (string-match "\\s *$" s)
                        (match-string 0 s))))
    (replace-regexp-in-string "\n"
                              (concat "\n" indent)
                              paste)))

(defun org-rich-yank--format-paste-default (language contents link)
  "Format LANGUAGE, CONTENTS and LINK as an `org-mode' source block."
  (let ((trimmed-contents (org-rich-yank--trim-nl contents)))
    (if language
        (format "#+begin_src %s\n%s\n#+end_src\n%s"
                language
                trimmed-contents
                link)
      (format "#+begin_quote\n%s\n#+end_quote\n%s"
              trimmed-contents
              link))))

(defun org-rich-yank--treat-as-image ()
  "Non-nil if clipboard contents contain image, and `org-download' feature enabled."
  (and org-rich-yank-download-image
       (require 'org-download nil 'noerror)
       (gui-backend-get-selection 'CLIPBOARD 'image/png)))

;;;###autoload
(defun org-rich-yank ()
  "Yank, surrounded by #+BEGIN_SRC block with major mode of originating buffer."
  (interactive)
  (if (org-rich-yank--treat-as-image)
      (org-download-clipboard)
    (let* ((raw-kill (current-kill 0 t))
           (link (org-rich-yank--link raw-kill))
           (lang (get-text-property 0 'org-rich-yank-lang raw-kill))
           (escaped-kill (org-escape-code-in-string raw-kill))
           (needs-initial-newline
            (save-excursion
              (re-search-backward "\\S " (line-beginning-position) 'noerror)))
           (paste (funcall org-rich-yank-format-paste
                           lang
                           escaped-kill
                           link)))
      (when needs-initial-newline
        (insert "\n"))
      (insert
       (if org-rich-yank-add-target-indent
           (org-rich-yank-indent paste)
         paste)))))


(provide 'org-rich-yank)
;;; org-rich-yank.el ends here
