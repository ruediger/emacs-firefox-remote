;;; firefox-remote --- Support for Firefox' remote debugging protocol ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>

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

;; * https://wiki.mozilla.org/Remote_Debugging_Protocol
;; * https://wiki.mozilla.org/DevTools/Features/Debugger/Notes
;; * https://github.com/paulrouget/firefox-remote-styleEditors
;; * https://github.com/jimblandy/DebuggerDocs

;;; Code:

(require 'json)

(defvar firefox-remote-buffer-name "*FirefoxRemote*"
  "Name for process buffer.")

(defvar firefox-remote--callbacks nil
  "List of callbacks called from filter.  This should be process local.")

(defvar firefox-remote--last-json-point nil
  "Read next JSON object from here.  This should be process local.")

(defun firefox-remote--sentinel (proc change)
  (message "ff--sentinel %s %s" proc change))

(defun firefox-remote--filter (proc data)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert data))
    (goto-char firefox-remote--last-json-point)
    (when (looking-at "[[:digit:]]+:")
      (goto-char (match-end 0)))
    (let ((obj (json-read)))
      (setq firefox-remote--last-json-point (point))
      (funcall (pop firefox-remote--callbacks) obj))))

(defun firefox-remote--handle-init (obj)
  (message "Init %s" obj))

(defun firefox-remote-connect (host service)
  "Connect to FIREFOX on HOST with SERVICE."
  (let ((proc (make-network-process :name "firefox-remote"
                                    :buffer firefox-remote-buffer-name
                                    :host host
                                    :service service
                                    :sentinel #'firefox-remote--sentinel
                                    :filter #'firefox-remote--filter
                                    ;; :keepalive t
                                    )))
    (with-current-buffer (process-buffer proc)
      (set (make-local-variable 'firefox-remote--callbacks) '(firefox-remote--handle-init))
      (set (make-local-variable 'firefox-remote--last-json-point) (point-min)))
    proc))

(defun firefox-remote-disconnect (con)
  (with-current-buffer (process-buffer con)
    (delete-process con)
    (kill-buffer)))

(defun firefox-remote-send (con msg callback)
  "Send MSG to CON and call CALLBACK with reply.
MSG is encoded as JSON and send to Firefox.
CALLBACK gets called with the parsed JSON object."
  (with-current-buffer (process-buffer con)
    (setq firefox-remote--callbacks
          (append firefox-remote--callbacks (list callback))))
  (let ((json-data (json-encode msg)))
   (process-send-string con (format "%s:%s" (length json-data) json-data))))

(provide 'firefox-remote)

;; firefox-remote.el ends here
