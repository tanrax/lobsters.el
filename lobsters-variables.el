;;; lobsters-variables.el --- Variables for the Lobsters client -*- lexical-binding: t -*- -*- coding: utf-8 -*-

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

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Configuration variables for the Lobsters Emacs client.

;;; Code:

(defun lobsters-variables--get-hottest-endpoint ()
  "Get the hottest endpoint URL."
  lobsters-variables--hottest-endpoint)

(defun lobsters-variables--get-newest-endpoint ()
  "Get the newest endpoint URL."
  lobsters-variables--newest-endpoint)

(defcustom lobsters-variables-stories-per-page nil
  "Number of stories to display per page. Set to nil to show all stories."
  :type '(choice (const :tag "Show all" nil)
                 (integer :tag "Stories per page"))
  :group 'lobsters)

(defcustom lobsters-variables-auto-refresh-interval nil
  "Automatic refresh interval in seconds. Set to nil to disable."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'lobsters)

(defcustom lobsters-variables-browser-function 'eww
  "Function to use for browsing URLs."
  :type '(choice (const :tag "Use eww (internal browser)" eww)
                 (const :tag "Use system browser" browse-url-default-browser)
                 (function :tag "Custom browser function"))
  :group 'lobsters)

(defconst lobsters--max-width 80)

;; API endpoints
(defconst lobsters-variables--hottest-endpoint "https://lobste.rs/hottest.json")
(defconst lobsters-variables--newest-endpoint "https://lobste.rs/newest.json")

;; Buffer names
(defconst lobsters-variables--hottest-buffer-name "*Lobsters - Hottest*")
(defconst lobsters-variables--newest-buffer-name "*Lobsters - Newest*")

;; Variables for state management
(defvar lobsters-variables--stories nil
  "List of currently loaded stories.")

(defvar lobsters-variables--current-feed-type nil
  "Current feed type being displayed (`hottest or `newest).")

(defvar lobsters-variables--loading nil
  "Whether we're currently loading stories.")

(provide 'lobsters-variables)
;;; lobsters-variables.el ends here
