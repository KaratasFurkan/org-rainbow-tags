;;; org-rainbow-tags.el --- Colorize org tags automatically -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Furkan Karataş

;; Author: Furkan Karataş <furkan.karatas02@gmail.com>
;; URL: https://github.com/KaratasFurkan/org-rainbow-tags
;; Version: 0.1-pre
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces, outlines

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds random colors to your org tags. In order to make colors
;; random but consistent between same tags, colors are generated from the hash
;; of the tag names.

;; Since it's random, results may not make you happy, but there are some custom
;; fields that you can use as ~seed~ to generate different colors. If you are
;; really picky, there is already a built-in solution for you, please see
;; [[https://orgmode.org/manual/Tags.html][org-tag-faces]]. This package aims to
;; get rid of setting and updating ~org-tag-faces~ manually for each tag you
;; use.

;;;; Installation

;;;;; Manual

;; 1. Clone this repository
;; 2. Add these two lines to your init file:

;; (add-to-list 'load-path "/path/to/org-rainbow-tags/")
;; (require 'org-rainbow-tags)

;;;;; Using ~straight.el~ and ~use-package~

;; (use-package org-rainbow-tags
;;   :straight (:host github :repo "KaratasFurkan/org-rainbow-tags"))

;;;;; MELPA

;; You need to enable package installations from MELPA if you didn't already.
;; /(See: https://melpa.org/#/getting-started)/

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)


;;;;;; Interactively

;; ~M-x package-install RET org-rainbow-tags RET~

;;;;;; With ~init.el~ or ~.emacs~

;; (unless (package-installed-p 'org-rainbow-tags)
;;   (package-install 'org-rainbow-tags))
;; (require 'org-rainbow-tags)


;;;;;; With ~use-package~

;; (use-package org-rainbow-tags
;;   :ensure t)


;;;; Usage

;; You can run ~org-rainbow-tags-mode~ command in the buffer you wanna colorize
;; the tags.

;; If you wanna run this minor mode on ~org~ files automatically, you can add a
;; hook:

;; (add-hook 'org-mode-hook 'org-rainbow-tags-mode)

;; To see customization options, you can run ~M-x customize-group RET
;; org-rainbow-tags RET~ or you can check ~(defcustom ...)~ lines in
;; ~org-rainbow-tags.el~.

;; If you don't like the auto-generated colors for your favorite tags, you can
;; change the value of ~org-rainbow-tags-hash-start-index~ between 0-20. This
;; variable decides which 12 characters of the hash of the tag should be taken
;; to generate the color.

;; Example:

;; (setq org-rainbow-tags-hash-start-index 10)

;; Full ~use-package~ example:

;; (use-package org-rainbow-tags
;;   :ensure t
;;   :custom
;;   (org-rainbow-tags-hash-start-index 10)
;;   (org-rainbow-tags-extra-face-attributes
;;    ;; Default is '(:weight 'bold)
;;    '(:inverse-video t :box t :weight 'bold))
;;   :hook
;;   (org-mode . org-rainbow-tags-mode))

;;;; Known Issues
;; ~org-rainbow-tags-mode~ colorizes org tags when it's activated and also when
;; a new tag is added/updated with ~org-set-tags-command~ or with ~C-c C-c~ on
;; the headline. However, colors will not updated when you edit the tags
;; manually. If you wanna update colors in every circumstances, you can add this
;; line to your configuration:

;; (add-hook 'org-mode-hook (lambda ()
;;                            (add-hook 'post-command-hook
;;                                      'org-rainbow-tags--apply-overlays nil t)))

;; This is not default because it may cause performance issues on org files. You
;; can try it and decide if it's okay or not.

;;; Code:

;;;; Requirements

(require 'color)

;;;; Customization

(defgroup org-rainbow-tags nil
  "Colorize org tags automatically."
  :group 'faces
  :prefix "org-rainbow-tags-")

(defcustom org-rainbow-tags-extra-face-attributes '(:weight 'bold)
  "Face attributes for auto-generated org tag faces.
Should be a list of ATTRIBUTE VALUE pairs like in `set-face-attribute'
function."
  :group 'org-rainbow-tags
  :type 'list)

(defcustom org-rainbow-tags-hash-start-index 0
  "Start from this index when extracting substring from the hash of the tag.
This can be useful when you don't like the auto-generated colors and want to
change them. Should be between 0-20."
  :group 'org-rainbow-tags
  :type 'integer)

(defcustom org-rainbow-tags-adjust-color-percent 20
  "Lighten or darken auto-generated colors by this percent if necessary.
This value is used when the auto-generated color is similar to the current
background color. This can be useful when you don't like the auto-generated
colors and want to change them. Should be between 0-100."
  :group 'org-rainbow-tags
  :type 'integer)

;;;; Variables

(defvar org-rainbow-tags--org-tag-regexp "[^\n]:\\([[:alnum:]_@#%]+\\):"
  "Regexp matching org tags.")

(defvar org-rainbow-tags--filetags-regexp
  (rx (seq bol "#+filetags:" (1+ " ")
           (group (0+ any)) eol))
  "Regular expression to match org filetags lines.")

(defvar org-rainbow-tags--org-clocktable-regexp
  (rx bol (0+ " ") "#+BEGIN:" (0+ " ") "clocktable")
  "Regular expression matching the start of a clocktable block.")

(defvar org-rainbow-tags--single-clocktable-tag-regexp
  (rx (group (regexp org-tag-re))
      (group (opt (or (seq "," (0+ " ")) (seq (0+ " ") "|")))))
  "Regular expression matching a single tag in an org table cell.")

(defvar org-rainbow-tags--clocktable-tags-cell-regexp
  (rx (seq "|" (0+ " ") (1+ (regexp org-rainbow-tags--single-clocktable-tag-regexp))))
  "Regular expression matching a set of tags in an org table cell.")

(defvar org-rainbow-tags--tag-sections-regexp
  (rx (or (regexp org-tag-line-re)
          (regexp org-rainbow-tags--org-clocktable-regexp)))
  "Regular expression matching all tag sections.")

(defvar org-rainbow-tags--overlays '()
  "Variable to store overlays.")

;;;; Commands

;;;###autoload
(define-minor-mode org-rainbow-tags-mode
  "Colorize org tags automatically to make them visually distinguishable."
  :require 'org-rainbow-tags
  :group 'org-rainbow-tags
  (if org-rainbow-tags-mode
      (progn
        (org-rainbow-tags--apply-overlays)
        (add-hook 'org-after-tags-change-hook
                  'org-rainbow-tags--apply-overlays))
    (org-rainbow-tags--delete-overlays)
    (remove-hook 'org-after-tags-change-hook
                 'org-rainbow-tags--apply-overlays)))

;;;; Functions

;;;;; Public

;;;;; Private

(defun org-rainbow-tags--adjust-color (hex-color)
  "Adjust HEX-COLOR according to active theme."
  (let ((background-color (face-attribute 'default :background)))
    (cond
     ;; Lighten when both dark
     ((and (color-dark-p (color-name-to-rgb background-color))
           (color-dark-p (color-name-to-rgb hex-color)))
      (color-lighten-name hex-color org-rainbow-tags-adjust-color-percent))
     ;; Darken when both light
     ((and (not (color-dark-p (color-name-to-rgb background-color)))
           (not (color-dark-p (color-name-to-rgb hex-color))))
      (color-darken-name hex-color org-rainbow-tags-adjust-color-percent))
     (t
      hex-color))))

(defun org-rainbow-tags--str-to-color (str)
  "Generate a hex color from STR."
  (let* ((hash (secure-hash 'md5 str))
         (from org-rainbow-tags-hash-start-index)
         (to (+ (% org-rainbow-tags-hash-start-index 21) 12))
         (color (concat "#" (substring hash from to))))
    (org-rainbow-tags--adjust-color color)))

(defun org-rainbow-tags--generate-face (name)
  "Generate a face with NAME."
  `(defface ,(intern name)
     '((t (:inherit 'org-tag)))
     "Auto-generated org tag face."))

(defun org-rainbow-tags--set-face (name color)
  "Set face attributes of face NAME.
COLOR is used in :foreground attribute by default but it can be used as
background by adding `:inverse-video t' to
`org-rainbow-tags-extra-face-attributes'."
  `(set-face-attribute ',(intern name) nil
                       :foreground ,color
                       ,@org-rainbow-tags-extra-face-attributes))

(defun org-rainbow-tags--get-face (tag)
  "Generate a face with the hash of the TAG."
  (let* ((tag (if (wholenump tag) (match-string-no-properties tag) tag))
         (color (org-rainbow-tags--str-to-color tag))
         (face-name (concat "org-rainbow-tags--" tag)))
    (unless (facep face-name)
      ;; TODO: try to make this without `eval'
      (eval (org-rainbow-tags--generate-face face-name)))
    ;; Set face attributes separately to be able to change colors with
    ;; `org-rainbow-tags-hash-start-index' or
    ;; `org-rainbow-tags-adjust-color-percent'.
    ;; TODO: try to make this without `eval'
    (eval (org-rainbow-tags--set-face face-name color))
    face-name))

(defun org-rainbow-tags--apply-overlay (beg end)
  "Create and apply `org-rainbow-tags' overlay to the text between BEG and END.

All created overlays are added to the `org-rainbow-tags--overlays' list for tracking purposes."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face (org-rainbow-tags--get-face 1))
    (push overlay org-rainbow-tags--overlays)))

(defun org-rainbow-tags--apply-overlays-to-matched-tag-section ()
  "Apply a font face overlay to matched tag sections in the current buffer.

For dynamic blocks containing clocktables, the function will apply overlays
to each tag in each cell of the clocktable column with the header `Tags'.

For normal Org tag groups (e.g., ':tag1:tag2:'), the function will apply overlays
to each tag in the group."
  (let ((end-of-tag-section-match (match-end 0)))
    (goto-char (match-beginning 0))
    (cond
     ;; Case for dynamic blocks that contain clocktables
     ((looking-at-p org-dblock-start-re)
      (let ((table-block-end (save-excursion
                               (when (re-search-forward org-dblock-end-re (point-max) t)
                                 (beginning-of-line)
                                 (point))))
            (tags-column-start nil)
            (end-of-field nil))
        (cond
         (table-block-end
          (when (re-search-forward org-table-hline-regexp table-block-end t)
            (forward-line -1)
            (when (search-forward "Tags" (line-end-position) t)
              (search-backward "|")
              (setq tags-column-start (current-column))
              ;; Using these two is so much faster than `next-line' that I feel
              ;; like I have to be missing a built-in that does this.
              (forward-line 1)
              (move-to-column tags-column-start)
              ;; Loop through the table to find and apply overlays to tags
              (while (< (point) table-block-end)
                (forward-line 1)
                (move-to-column tags-column-start)
                (when (looking-at org-rainbow-tags--clocktable-tags-cell-regexp)
                  (save-excursion
                    (setq end-of-field (match-end 0))
                    (while (re-search-forward org-rainbow-tags--single-clocktable-tag-regexp end-of-field t)
                      (org-rainbow-tags--apply-overlay (match-beginning 1) (match-end 1))))))))
          (goto-char table-block-end))
         (t
          (goto-char end-of-tag-section-match)))))
     ;; Case for normal Org tag groups
     ((re-search-forward org-tag-group-re (line-end-position) t)
      (goto-char (match-beginning 0))
      (while (re-search-forward org-rainbow-tags--org-tag-regexp (line-end-position) t)
        (org-rainbow-tags--apply-overlay (match-beginning 1) (match-end 1))
        (backward-char 2))
      (forward-line 1))
     (t
      (goto-char end-of-tag-section-match)))))

(defun org-rainbow-tags--apply-overlays ()
  "Add the auto-generated tag faces."
  (org-rainbow-tags--delete-overlays)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           org-rainbow-tags--filetags-regexp
           (save-excursion (re-search-forward org-element-headline-re nil t) (beginning-of-line) (point))
           t)
      (org-rainbow-tags--apply-overlays-to-matched-tag-section))
    (while (re-search-forward org-rainbow-tags--tag-sections-regexp nil t)
      (org-rainbow-tags--apply-overlays-to-matched-tag-section))))

(defun org-rainbow-tags--delete-overlays ()
  "Remove the auto-generated tag overlays."
  (mapc 'delete-overlay org-rainbow-tags--overlays)
  (setq-local org-rainbow-tags--overlays '()))

;;;; Footer

(provide 'org-rainbow-tags)

;;; org-rainbow-tags.el ends here
