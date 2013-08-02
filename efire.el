;;; efire.el --- Use campfire from emacs

;; Copyright (C) 2013  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, tools

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

;; Set:
;;
;;  (setq efire-token "yourtoken")
;;  (setq efire-host "yourhost.campfirenow.com")
;;
;; And then M-x campfire-join-room
;;

;;; Code:

(require 'lui)
(require 'json)
(require 'url)



;;; Setup and authentication
;;;
(defvar efire-token nil)
(defvar efire-host nil)


;;; Internal vars
;;;
(defvar efire--rooms nil)
(defvar efire--timer nil)
(defvar efire--whoami nil)



;;; Interactive stuff
;;;
(defun efire-join-room (room)
  (interactive
   (progn
     (efire--message "getting room names...")
     (setq efire--rooms (efire--get 'rooms (efire--request "rooms.json")))
     (setq efire--whoami (efire--get 'user (efire--request "users/me.json")))
     (list
      (cl-find (completing-read
                (efire--format "join which room? ")
                (loop for room across efire--rooms
                      collect (efire--get 'name room)))
               efire--rooms
               :key #'(lambda (room) (efire--get 'name room))
               :test #'string=))))
  (with-current-buffer (efire--room-buffer room)
    (unless (eq major-mode 'efire-mode)
      (efire-mode))
    (when (or (not efire--timer)
              (not (memq efire--timer timer-list)))
      (set (make-local-variable 'efire--timer)
           (efire--setup-room room)))
    (pop-to-buffer (current-buffer))))


;;; Helpers
;;;
(defun efire--room-buffer (room)
  (get-buffer-create (format "*campfire: %s*" (efire--get 'name room))))

(define-derived-mode efire-mode lui-mode "efire"
  "A major mode for campfire rooms"
  (lui-set-prompt "\n: "))


;;; Main loop
;;;
;;; This is javascript-inspired crazyness to play around with lexical
;;; binding. But it's not good, emacs has buffer-local vars and many
;;; find-definition and code-reloading facilities that are rendered
;;; useless by this approach. To be refactored.
;;;
(defun efire--setup-room (room)
  (let* (;;; data
         last-message
         (buffer (current-buffer))
         timer
         (known-users (make-hash-table))
         (room-id (efire--get 'id room))
         ;;; functions
         (teardown-fn
          #'(lambda (reason)
              (efire--warning "Tearing down room %s in buffer %s because %s" room-id buffer reason)
              (tracking-remove-buffer buffer)
              (cancel-timer timer)))
         (oops-fn
          #'(lambda (reason)
              (efire--warning "Non-fatal oops in room %s in buffer %s: %s" room-id buffer reason)))
         (irrelevant-fn
          #'(lambda (whatever)
              (efire--trace "Whatever: %s" whatever)))
         (register-user-fn
          #'(lambda (user)
              (puthash (efire--get 'id user)
                       user
                       known-users)))
         (message-fn
          #'(lambda (message)
              (let ((message-id (efire--get 'id message))
                    (last-message-id (and last-message
                                          (efire--get 'id last-message))))
                (efire--trace "Inserting message id=%s" )
                (when (or (not last-message-id)
                          (> message-id last-message-id))
                  (efire--insert-message
                   (efire--find-user (efire--get 'user_id message) known-users register-user-fn)
                   message)))
              (setq last-message message)))
         (own-message-fn message-fn)
         (get-recent-messages-fn
          #'(lambda ()
              (efire--trace "Getting recent messages for room id=%s" room-id)
              (efire--get-recent-messages room-id
                                          (and last-message
                                               (efire--get 'id last-message))
                                          message-fn
                                          irrelevant-fn
                                          oops-fn)))
         (timer-fn
          #'(lambda ()
              (efire--trace "Timer fired for room id=%s in buffer %s" room-id buffer)
              (if (buffer-live-p buffer)
                  (funcall get-recent-messages-fn)
                  (funcall teardown-fn "buffer was killed"))))
         (start-timer-fn
          #'(lambda ()
              (efire--info "Got %s users, starting timer" (hash-table-count known-users))
              (efire--trace "Starting timer for room id=%s" room-id)
              (setq timer (run-at-time nil 2 timer-fn))))
         (joined-fn
          #'(lambda (_data)
              (efire--get-users room-id
                                register-user-fn
                                start-timer-fn
                                teardown-fn)))
         (send-message-fn
          #'(lambda (input)
              (efire--send-message room input
                                   own-message-fn
                                   oops-fn))))

    (set-buffer-multibyte t)
    (set (make-local-variable 'lui-input-function) send-message-fn)
    (tracking-add-buffer buffer)
    (efire--join-room room-id joined-fn teardown-fn)))

(defun efire--find-user (user-id known-users register-user-fn)
  (let* ((user (gethash user-id known-users)))
    (cond ((and user-id
                (not user))
           (efire--warning "user %s not known, trying to find it asynch" user-id)
           (efire--request-async (format "users/%d.json" user-id)
                                 #'(lambda (obj)
                                     (let ((user (efire--get 'user obj)))
                                       (efire--info "found user %s to be %s" user-id user)
                                       (funcall register-user-fn (efire--get 'user obj))))
                                 #'(lambda ()
                                     (efire--warning "couldn't find user %s" user-id)))
           :still-looking)
          (user-id
           (efire--trace "user %s (%s) found in table"
                         user-id
                         (efire--get 'name user))
           user))))

(defun efire--send-message (room input message-sent-callback error-callback)
  (let* ((message-type (if (string-match "\n" input)
                           (progn
                             (replace-regexp-in-string "\n"
                                                       "&#xA;"
                                                       input)
                             "PasteMessage")
                         "TextMessage"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode `((message  (body . ,input)
                                   (type . ,message-type))))))
    (efire--request-async (format "room/%d/speak.json" (efire--get 'id room))
                          #'(lambda (data)
                              (efire--trace "posted message sucessfully")
                              (let ((message (efire--get 'message data)))
                                (funcall message-sent-callback message)))
                          #'(lambda (err)
                              (efire--trace "oops could not send your message because %s" err)
                              (funcall error-callback err)))))

(defun efire--get-recent-messages (room-id last-message-id message-callback done-callback error-callback)
  (let ((base-url (format "room/%d/recent.json" room-id)))
    (efire--request-async
     (if last-message-id
         (format "%s?since_message_id=%d"
                 base-url last-message-id)
       base-url)
     #'(lambda (response)
         (efire--iterate (efire--get 'messages response)
                         message-callback
                         error-callback)
         (funcall done-callback response))
     #'(lambda (err)
         (efire--error "error getting messages for room-id=%s" room-id)
         (funcall error-callback err)))))

(defun efire--join-room (room-id done-callback error-callback)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json"))))
    (efire--request-async (format "room/%d/join.json" room-id)
                          #'(lambda (data)
                              (efire--info "sucessfully joined room-id=%d (got data %s)" room-id data)
                              (funcall done-callback data))
                          #'(lambda (err)
                              (efire--error "error joining room-id=%d" room-id)
                              (funcall error-callback err)))))

(defun efire--get-users (room-id user-callback done-callback error-callback)
  (efire--request-async (format "room/%d.json" room-id)
                        #'(lambda (data)
                            (efire--info "sucessfully got room-id=%d (got data %s)" room-id data)
                            (let* ((room (efire--get 'room data))
                                   (users (and room
                                               (efire--get 'users room))))
                              (efire--iterate users user-callback error-callback))
                            (funcall done-callback))
                        #'(lambda (err)
                            (efire--error "error joining room-id=%s" room-id)
                            (funcall error-callback err))))

(defun efire--insert-message (user message)
  (let* ((type (efire--get 'type message))
         (face (if  (and efire--whoami
                         (eq (efire--get 'id efire--whoami)
                             (efire--get 'id user)))
                   'font-lock-keyword-face
                 'font-lock-function-name-face))
         (user-name (if (consp user)
                        (propertize (efire--get 'name user)
                                    'face (or face
                                              'font-lock-keyword-face))
                      (format "looking for %s" (efire--get 'user_id message))))
         (type-sym (intern type))
         (lui-fill-type (if (eq type-sym 'PasteMessage)
                            nil
                          lui-fill-type))
         (body (efire--get 'body message)))
    (cond ((memq type-sym
                 '(TextMessage PasteMessage))
           (lui-insert (format "%s:%s%s"
                               user-name
                               (if (eq type-sym 'PasteMessage) "\n" " ")
                               (if (eq type-sym 'PasteMessage)
                                   (propertize body 'face 'font-lock-doc-face)
                                   body))))
          (t
           (efire--info "unknown message type %s" type)))))

(defun efire--iterate (objects callback error-callback)
  (loop with stop = nil
        for object across objects
        while (not stop)
        do (efire--with-error-checking
               #'(lambda (err)
                   (when error-callback
                     (funcall error-callback err))
                   (setq stop t))
               (format "processing object id=%s, which is %s" (efire--get 'id object) object)
             (funcall callback object))))


;;; Processing json objects returned by campfire
;;;
(defun efire--url (path)
  (format "https://%s:X@%s/%s"
          efire-token efire-host path))

(defun efire--get (key object)
  (cdr (assoc key object)))

(defun efire--request (path)
  (let ((buffer
         (efire--ignoring-errors-maybe
          (url-retrieve-synchronously (efire--url path)))))
    (and buffer
         (with-current-buffer buffer
           (efire--read-object)))))

(defun efire--request-async (path callback &optional error-callback)
  (let ((url (url-encode-url
              (efire--url path)))
        (saved-buffer (current-buffer)))
    (efire--with-error-checking
        error-callback
        (format "getting url %s" url)
      (url-retrieve url
                    #'(lambda (status)
                        (cond ((null status)
                               (efire--trace "calling the url-retrieve callback")
                               ;; notice that the object is read in the http
                               ;; buffer, but the callback must be called in the
                               ;; original buffer.
                               ;;
                               (let ((object (efire--read-object)))
                                 (with-current-buffer saved-buffer
                                   (funcall callback object))))
                              (t
                               (efire--error "status was %s" status)
                               (with-current-buffer saved-buffer
                                 (when error-callback
                                   (funcall error-callback status)))))
                        (kill-buffer (current-buffer)))
                    nil
                    'silent
                    'inhibit-cookies))))

;; curl -u 605b32dd:X -H 'Content-Type: application/json' \
;; -d '{"message":{"body":"Hello"}}' https://sample.campfirenow.com/room/1/speak.json

(defun efire--read-object ()
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (let ((data (decode-coding-string (buffer-substring (point)
                                                      (point-max))
                                    'utf-8)))
    (efire--ignoring-errors-maybe
     (json-read-from-string data))))



;;; Automatic tests
;;;



;;; Logging machinery
;;;
(defvar efire-verbosity 2
  "Log level for `efire--message' 4 means trace most anything, 0 means nothing.")

;;; (setq efire-verbosity 2)

(defun efire--log (level message &rest args)
  "When LEVEL is above `efire-verbosity-level', log MESSAGE and ARGS."
  (when (>= efire-verbosity level)
    (message "%s" (apply #'efire--format message args))))

(defun efire--message (message &rest args)
  (apply #'efire--log 1 message args))

(defun efire--error (message &rest args)
  (apply #'efire--log 1 (format "error: %s" message) args))

(defun efire--warning (message &rest args)
  (apply #'efire--log 2 (format "warning: %s" message) args))

(defun efire--info (message &rest args)
  (apply #'efire--log 3 (format "info: %s" message) args))

(defun efire--trace (message &rest args)
  (apply #'efire--log 4 (format "trace: %s" message) args))

(defun efire--format (format-control &rest format-args)
  (apply #'format (concat "[efire] " format-control) format-args))


;;; Debugging
;;;
(defvar efire--debug nil)
;; (setq efire--debug nil)

(defmacro efire--ignoring-errors-maybe (&rest body)
  `(let ((fn #'(lambda () ,@body)))
     (if efire--debug
         (funcall fn)
       (ignore-errors
         (funcall fn)))))

(defmacro efire--with-error-checking (error-callback doing-what &rest body)
  (declare (indent 2))
  (let ((err-sym (cl-gensym)))
    `(condition-case ,err-sym
         (progn
           (efire--trace "%s" ,doing-what)
           (funcall #'(lambda () ,@body)))
       (error
        (efire--error "something went wrong %s" ,doing-what)
        (when ,error-callback
          (funcall ,error-callback ,err-sym))
        (when efire--debug
          (signal 'error (cdr ,err-sym)))))))


(provide 'efire)
;;; efire.el ends here
;; Local Variables:
;; lexical-binding: t
;; End:
