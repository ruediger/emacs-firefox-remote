;; This is just a test/example case!

(require 'firefox-remote)

(defvar ~con nil)
(defvar ~tab nil)
(defvar ~thread nil)
(defvar ~reply nil)
(defvar ~background nil)

(defun ~reply-handler (x)
  (setq ~reply x))

(defun ~background-handler (x)
  (message "Firefox Remote: New background message!")
  (setq ~background (append ~background (list x))))

(progn ;; Init new connection
  (when ~con
    (ignore-errors (firefox-remote-disconnect ~con))
    (setq ~con nil))

  (setq ~con (firefox-remote-connect "localhost" 6000))
  (firefox-remote-set-default-handler #'~background-handler))

;; Set ~tab to selected tab
(firefox-remote-send ~con '((to . root) (type . listTabs))
                     (lambda (x)
                       (setq ~tab (cdr (assq 'actor (firefox-remote--get-selected x))))))

;; Attach to ~tab and set ~thread
(firefox-remote-send ~con `((to . ,~tab) (type . attach))
                     (lambda (x)
                       (~reply-handler x)
                       (setq ~thread (cdr (assq 'threadActor x)))))

(firefox-remote-send ~con `((to . ,~thread) (type . attach)) #'~reply-handler)

;; Interrupt thread
(firefox-remote-send ~con `((to . ,~thread) (type . interrupt)) #'~reply-handler)

;; Request frames
(firefox-remote-send ~con `((to . ,~thread) (type . frames)) #'~reply-handler)

;; Breakpoint
(let ((loc '((url . "file://") (line . 4))))
 (firefox-remote-send ~con `((to . ~thread) (type . setBreakpoint) (location . ,loc))
                      #'~reply-handler))

;; Resume thread
(firefox-remote-send ~con `((to . ,~thread) (type . resume)) #'~reply-handler)
