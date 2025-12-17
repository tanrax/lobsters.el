;;; lobsters-feed.el --- A Lobsters client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; Functions for fetching and processing Lobsters stories.

;;; Code:

(require 'lobsters-variables)
(require 'request)
(require 'json)
(require 'seq)

;; Forward declarations to avoid circular dependencies
(declare-function lobsters-ui--display-stories "lobsters-ui" (feed-type))

;; Hooks
(defvar lobsters-feed-after-fetch-stories-hook nil
  "Hook run after stories are successfully fetched.")

(defun lobsters-feed--fetch-stories-async (endpoint feed-type)
  "Fetch stories from ENDPOINT and display them.
FEED-TYPE should be `hottest or `newest."
  (setq lobsters-variables--loading t)
  (setq lobsters-variables--current-feed-type feed-type)
  (message "Fetching Lobsters stories...")

  (request endpoint
    :timeout 10
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq lobsters-variables--loading nil)
		(setq lobsters-variables--stories (lobsters-feed--process-stories data))
		(message "Lobsters stories loaded!")
		(lobsters-ui--display-stories feed-type)
		(run-hooks 'lobsters-feed-after-fetch-stories-hook)))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (setq lobsters-variables--loading nil)
	      (message "Error fetching Lobsters stories: %S" error-thrown)))))

(defun lobsters-feed--process-stories (raw-stories)
  "Process RAW-STORIES from the API into a more usable format."
  (mapcar (lambda (story)
	    (let-alist story
	      (list
	       (cons 'short-id .short_id)
	       (cons 'title (lobsters-feed--clean-string .title))
	       (cons 'url .url)
	       (cons 'score .score)
	       (cons 'comment-count .comment_count)
	       (cons 'submitter (lobsters-feed--clean-string .submitter_user))
	       (cons 'tags .tags)
	       (cons 'created-at .created_at)
	       (cons 'comments-url .comments_url)
	       (cons 'short-id-url .short_id_url)
	       (cons 'description (lobsters-feed--clean-string (or .description_plain ""))))))
	  raw-stories))

(defun lobsters-feed--clean-string (str)
  "Clean STR by removing carriage return and other unwanted characters."
  (when str
    (replace-regexp-in-string "\r" "" (string-trim str))))

(defun lobsters-feed--get-all-stories ()
  "Get all stories."
  lobsters-variables--stories)

(defun lobsters-feed--refresh-current-feed ()
  "Refresh the current feed."
  (interactive)
  (when lobsters-variables--current-feed-type
    (let ((endpoint (if (eq lobsters-variables--current-feed-type 'hottest)
		        lobsters-variables--hottest-endpoint
		      lobsters-variables--newest-endpoint)))
      (lobsters-feed--fetch-stories-async endpoint lobsters-variables--current-feed-type))))

(provide 'lobsters-feed)
;;; lobsters-feed.el ends here
