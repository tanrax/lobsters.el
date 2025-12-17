;;; lobsters-ui.el --- A Lobsters client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Andros Fenollosa <hi@andros.dev>
;; Version: 1.2
;; URL: https://github.com/tanrax/lobsters.el
;; Package-Requires: ((emacs "25.1") (request "0.2.0") (visual-fill-column "2.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; User interface functions for the Lobsters client.

;;; Code:

(require 'lobsters-variables)
(require 'widget)
(require 'wid-edit)
(require 'eww)
(require 'cl-lib)
(require 'visual-fill-column)

;; Forward declarations to avoid circular dependencies
(declare-function lobsters-feed--get-all-stories "lobsters-feed" ())
(declare-function lobsters-feed--fetch-stories-async "lobsters-feed" (endpoint feed-type))
(declare-function lobsters-feed--refresh-current-feed "lobsters-feed" ())

;; Define keymap for lobsters-mode
(defvar lobsters-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "n") 'lobsters-ui--goto-next-story)
    (define-key map (kbd "p") 'lobsters-ui--goto-previous-story)
    (define-key map (kbd "r") 'lobsters-feed--refresh-current-feed)
    (define-key map (kbd "q") 'lobsters-ui--quit)
    (define-key map (kbd "g") 'lobsters-feed--refresh-current-feed)
    (define-key map (kbd "b") 'lobsters-ui--toggle-browser)
    map)
  "Keymap for `lobsters-mode'.")

;; Define the lobsters-mode
(define-derived-mode lobsters-mode special-mode "Lobsters"
  "Major mode for viewing lobsters stories."
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 80)
  (visual-fill-column-mode 1)
  (use-local-map lobsters-mode-map))

;; UI Variables
(defconst lobsters-ui--char-separator ?-)

(defun lobsters-ui--insert-formatted-text (text &optional size font-color background-color)
  "Insert TEXT with optional formatting SIZE, FONT-COLOR, and BACKGROUND-COLOR."
  (let ((start (point)))
    (insert text)
    (let ((end (point))
	  (props (list)))
      (when size
	(push `(:height ,size) props))
      (when font-color
	(push `(:foreground ,font-color) props))
      (when background-color
	(push `(:background ,background-color) props))
      (when props
	(put-text-property start end 'face (apply #'append props))))))

(defun lobsters-ui--insert-logo ()
  "Insert the Lobsters logo/header."
  (lobsters-ui--insert-formatted-text "\n🦞 " 1.5 "#d2691e")
  (lobsters-ui--insert-formatted-text "Lobsters" 1.3 "#d2691e")
  (lobsters-ui--insert-formatted-text "\n\n"))

(defun lobsters-ui--string-separator ()
  "Return a string with the separator character."
  (make-string lobsters--max-width lobsters-ui--char-separator))

(defun lobsters-ui--insert-separator ()
  "Insert a horizontal separator line."
  (lobsters-ui--insert-formatted-text "\n")
  (lobsters-ui--insert-formatted-text (lobsters-ui--string-separator) nil "#666666")
  (lobsters-ui--insert-formatted-text "\n\n"))

(defun lobsters-ui--format-tags (tags)
  "Format TAGS list for display."
  (if tags
      (concat "[" (mapconcat #'identity tags ", ") "]")
    ""))

(defun lobsters-ui--browse-url (url)
  "Browse URL using the configured browser function."
  (if url
      (funcall lobsters-variables-browser-function url)
    (message "No URL available")))

(defun lobsters-ui--format-relative-time (timestamp)
  "Format TIMESTAMP as relative time."
  (let* ((time (date-to-time timestamp))
	 (diff (float-time (time-subtract (current-time) time)))
	 (days (floor (/ diff 86400)))
	 (hours (floor (/ (mod diff 86400) 3600)))
	 (minutes (floor (/ (mod diff 3600) 60))))
    (cond
     ((> days 0) (format "%d day%s ago" days (if (= days 1) "" "s")))
     ((> hours 0) (format "%d hour%s ago" hours (if (= hours 1) "" "s")))
     ((> minutes 0) (format "%d minute%s ago" minutes (if (= minutes 1) "" "s")))
     (t "just now"))))

(defun lobsters-ui--story-component (story)
  "Insert a story component for STORY."
  (let-alist story
    ;; Title (make it a clickable link)
    (widget-create 'push-button
		   :notify (lambda (&rest _)
			     (lobsters-ui--browse-url .url))
		   :help-echo (if .url (format "Open: %s" .url) "No URL")
		   :format "%[%v%]"
		   .title)

    (lobsters-ui--insert-formatted-text "\n")

    ;; Score and comments info
    (lobsters-ui--insert-formatted-text "  ")
    (lobsters-ui--insert-formatted-text (format "↑%d" .score) nil "#ff6600")
    (lobsters-ui--insert-formatted-text " | ")
    (lobsters-ui--insert-formatted-text (format "%d comment%s"
		                                .comment-count
		                                (if (= .comment-count 1) "" "s")) nil "#666666")
    (lobsters-ui--insert-formatted-text " | by ")
    (lobsters-ui--insert-formatted-text .submitter nil "#0066cc")
    (lobsters-ui--insert-formatted-text " | ")
    (lobsters-ui--insert-formatted-text (lobsters-ui--format-relative-time .created-at) nil "#666666")

    ;; Tags
    (when .tags
      (lobsters-ui--insert-formatted-text "\n  ")
      (lobsters-ui--insert-formatted-text (lobsters-ui--format-tags .tags) nil "#008000"))

    ;; Description (if available)
    (when (and .description (not (string-empty-p .description)))
      (lobsters-ui--insert-formatted-text "\n  ")
      (lobsters-ui--insert-formatted-text .description nil "#333333"))

    ;; Comments link
    (lobsters-ui--insert-formatted-text "\n  ")
    (widget-create 'push-button
		   :notify (lambda (&rest _)
			     (lobsters-ui--browse-url .comments-url))
		   :help-echo (format "View comments: %s" .comments-url)
		   " 💬 Comments ")

    ;; URL link (if different from comments)
    (when .url
      (lobsters-ui--insert-formatted-text " ")
      (widget-create 'push-button
		     :notify (lambda (&rest _)
			       (lobsters-ui--browse-url .url))
		     :help-echo (format "Open link: %s" .url)
		     " 🔗 Link "))

    (lobsters-ui--insert-formatted-text "\n")
    (lobsters-ui--insert-separator)))

(defun lobsters-ui--insert-header (feed-type)
  "Insert the header for FEED-TYPE."
  (lobsters-ui--insert-logo)

  ;; Navigation buttons
  (widget-create 'push-button
	         :notify (lambda (&rest _)
		           (lobsters-feed--fetch-stories-async lobsters-variables--hottest-endpoint 'hottest))
	         :help-echo "View hottest stories"
	         " 🔥 Hottest ")

  (lobsters-ui--insert-formatted-text " ")

  (widget-create 'push-button
	         :notify (lambda (&rest _)
		           (lobsters-feed--fetch-stories-async lobsters-variables--newest-endpoint 'newest))
	         :help-echo "View newest stories"
	         " 🆕 Newest ")

  (lobsters-ui--insert-formatted-text " ")

  (widget-create 'push-button
	         :notify (lambda (&rest _)
		           (lobsters-feed--refresh-current-feed))
	         :help-echo "Refresh current feed"
	         " ↻ Refresh ")

  ;; Current feed indicator
  (lobsters-ui--insert-formatted-text (format "\n\nShowing: %s stories\n"
	                                      (if (eq feed-type 'hottest) "Hottest" "Newest"))
	                              nil "#d2691e")

  ;; Keyboard shortcuts help
  (lobsters-ui--insert-formatted-text "Keyboard: (n) Next story | (p) Previous story | (r) Refresh | (q) Quit | (b) Browser toggle\n")

  (lobsters-ui--insert-separator))

(defun lobsters-ui--insert-stories ()
  "Insert all stories."
  (let ((stories (lobsters-feed--get-all-stories)))
    (if stories
	(dolist (story stories)
	  (lobsters-ui--story-component story))
      (lobsters-ui--insert-formatted-text "No stories available.\n" nil "#ff0000"))))

(defun lobsters-ui--goto-next-story ()
  "Go to the next story."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (lobsters-ui--string-separator)) "$")))
    (if (search-forward-regexp separator-regex nil t)
	(progn
	  (forward-line 2)
	  (recenter-top-bottom))
      (message "No more stories"))))

(defun lobsters-ui--goto-previous-story ()
  "Go to the previous story."
  (interactive)
  (let ((separator-regex (concat "^" (regexp-quote (lobsters-ui--string-separator)) "$")))
    (search-backward-regexp separator-regex nil t)
    (unless (search-backward-regexp separator-regex nil t)
      (goto-char (point-min)))
    (forward-line 2)
    (recenter-top-bottom)))

(defun lobsters-ui--toggle-browser ()
  "Toggle between eww and system browser."
  (interactive)
  (setq lobsters-variables-browser-function
	(if (eq lobsters-variables-browser-function 'eww)
	    'browse-url-default-browser
	  'eww))
  (message "Browser set to: %s"
	   (if (eq lobsters-variables-browser-function 'eww)
	       "eww (internal)"
	     "system browser")))

(defun lobsters-ui--quit ()
  "Quit all Lobsters buffers."
  (interactive)
  ;; Kill both hottest and newest buffers if they exist
  (when (get-buffer lobsters-variables--hottest-buffer-name)
    (kill-buffer lobsters-variables--hottest-buffer-name))
  (when (get-buffer lobsters-variables--newest-buffer-name)
    (kill-buffer lobsters-variables--newest-buffer-name)))

(defun lobsters-ui--display-stories (feed-type)
  "Display stories for FEED-TYPE."
  (let ((buffer-name (if (eq feed-type 'hottest)
	                 lobsters-variables--hottest-buffer-name
	               lobsters-variables--newest-buffer-name)))
    (switch-to-buffer buffer-name)
    (kill-all-local-variables)

    ;; Disable read-only mode BEFORE trying to modify the buffer
    (read-only-mode -1)

    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)

    ;; Insert content
    (lobsters-ui--insert-header feed-type)
    (lobsters-ui--insert-stories)

    ;; Set up the buffer with lobsters-mode
    (lobsters-mode)
    (widget-setup)
    (display-line-numbers-mode 0)
    (goto-char (point-min))
    (widget-forward 1)

    ;; Enable read-only mode AFTER all modifications are done
    (read-only-mode 1)))

(provide 'lobsters-ui)
;;; lobsters-ui.el ends here
