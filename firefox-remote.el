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

;;; Low Level Connection Handling

(defvar firefox-remote--handler-alist nil
  "Alist mapping message types to handlers.  This should be process local.
Use `firefox-remote-set-handler' to manipulate it.")

(defun firefox-remote-set-handler (connection type handler)
  "Set HANDLER for message TYPE on CONNECTION."
  (with-current-buffer (process-buffer connection)
    (let ((oldhandler (assq type firefox-remote--handler-alist)))
      (if oldhandler
          (setcdr oldhandler handler)
        (setq firefox-remote--handler-alist
              (cons (cons type handler) firefox-remote--handler-alist))))))

(defun firefox-remote-get-handler (connection type)
  "Get handler for message TYPE on CONNECTION."
  (with-current-buffer (process-buffer connection)
    (cdr (assq type firefox-remote--handler-alist))))

(defvar firefox-remote--error-handler-alist nil
  "Alist mapping errpr types to handlers.  This should be process local.
Use `firefox-remote-set-error-handler' to manipulate it.")

(defun firefox-remote-set-error-handler (connection type handler)
  "Set HANDLER for error TYPE on CONNECTION."
  (with-current-buffer (process-buffer connection)
    (let ((oldhandler (assq type firefox-remote--error-handler-alist)))
      (if oldhandler
          (setcdr oldhandler handler)
        (setq firefox-remote--error-handler-alist
              (cons (cons type handler) firefox-remote--error-handler-alist))))))

(defun firefox-remote-get-error-handler (connection type)
  "Get handler for error TYPE on CONNECTION."
  (with-current-buffer (process-buffer connection)
    (cdr (assq type firefox-remote--error-handler-alist))))

(defvar firefox-remote--default-handler nil
  "Handler called if no other handler is found and message is not a reply.
This should be process local.  Use `firefox-remote-set-default-handler'
to set it.")

(defun firefox-remote-set-default-handler (connection handler)
  "Set default HANDLER for CONNECTION."
  (with-current-buffer (process-buffer connection)
    (setq firefox-remote--default-handler handler)))

(defun firefox-remote-get-default-handler (connection)
  "Get default handler for CONNECTION."
  (with-current-buffer (process-buffer connection)
    firefox-remote--default-handler))

(defvar firefox-remote-buffer-name "*FirefoxRemote*"
  "Name for process buffer.")

(defvar firefox-remote--callbacks nil
  "List of callbacks called from filter.  This should be process local.")

(defvar firefox-remote--last-json-point nil
  "Read next JSON object from here.  This should be process local.")

(defun firefox-remote--sentinel (proc change)
  (message "ff--sentinel %s: %s" proc change))

(defun firefox-remote--handle-json-object (proc)
  "Read the JSON object from the buffer and call the matching callback.
The JSON objects are received in the following form:

    <size>:<json object>

PROC is the firefox remote process."
  (goto-char firefox-remote--last-json-point)
  (when (looking-at "[[:digit:]]+:")
    (goto-char (match-end 0)))
  (let ((obj (ignore-errors (json-read))))
    (when obj
      (insert "\n")
      (setq firefox-remote--last-json-point (point))
      (let ((callback (or
                       (pop firefox-remote--callbacks)
                       (cond
                        ((assq 'type obj) (firefox-remote-get-handler
                                           proc
                                           (cdr (assq 'type obj))))
                        ((assq 'error obj) (firefox-remote-get-error-handler
                                            proc
                                            (cdr (assq 'error obj)))))
                       (firefox-remote-get-default-handler proc))))
        (when callback
          (funcall callback obj))))))

(defun firefox-remote--filter (proc data)
  "Insert DATA into PROC's buffer and handle the json objects."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert data))
    (goto-char firefox-remote--last-json-point)
    (while (/= (point) (point-max))     ; read until all objects are handled
      (firefox-remote--handle-json-object proc))))

(defun firefox-remote--handle-init (obj)
  (message "Init %s" obj))

(defun firefox-remote-connect (host service)
  "Connect to Firefox on HOST with SERVICE."
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
      (set (make-local-variable 'firefox-remote--last-json-point) (point-min))
      (set (make-local-variable 'firefox-remote--handler-alist) nil)
      (set (make-local-variable 'firefox-remote--error-handler-alist) nil)
      (set (make-local-variable 'firefox-remote--default-handler) nil))
    proc))

(defun firefox-remote-disconnect (con)
  "Disconnect firefox remote connection CON."
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

;;;

(defun firefox-remote-get-list-of-tabs (con callback)
  "Ask CON for a list of tabs.
CALLBACK gets called with an array of tabs."
  (firefox-remote-send
   con '((to . root) (type . listTabs))
   (lambda (data)
     (let ((cb callback))
       (let ((selected (cdr (assq 'selected data))))
         (push '(selected . t) (aref (cdr (assq 'tabs foo)) selected))
         (funcall cb (cdr (assq 'tabs foo))))))))

;;;

(defun firefox-remote-get-stylesheets (con tab callback)
  (if (or (not tab) (eq tab 'selected))
    (firefox-remote-send ;; Use selected tab
     con '((to . root) (type . listTabs))
     (lambda (data)
       (let ((con con) (cb callback))
        (funcall #'firefox-remote-get-stylesheets
                 con
                 (aref (cdr (assq 'tabs data))
                       (cdr (assq 'selected data)))
                 cb))))
    (let ((actor (cdr (assq 'styleEditorActor tab))))
      (firefox-remote-send
       con `((to . ,actor)
             (type . getStyleSheets))
       callback))))

(defun firefox-remote--get-selected (tabs)
  (aref (cdr (assq 'tabs tabs))
        (cdr (assq 'selected tabs))))

(provide 'firefox-remote)

;;; firefox-remote.el ends here
