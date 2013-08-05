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
;; And then M-x efire-join-room
;;

;;; Code:

(require 'lui)
(require 'json)
(require 'url)



;;; Setup and authentication
;;;
(defvar efire-token nil)
(defvar efire-host nil)


;;; Internal vars, global
;;;
(defvar efire--rooms nil)
(defvar efire--whoami nil)


;;; Bound dynamically
(defvar efire--timer nil)
(defvar efire--room-id nil)



;;; Internal vars, buffer-local
;;;
(defvar efire--last-message nil)
(defvar efire--recently-inserted-own-messages nil)
(defvar efire--buffer nil)
(defvar efire--known-users nil)

(make-variable-buffer-local 'efire--last-message)
(make-variable-buffer-local 'efire--recently-inserted-own-messages)
(make-variable-buffer-local 'efire--buffer)
(make-variable-buffer-local 'efire--known-users)



;;; Interactive stuff
;;;
;;;###autoload
(defun efire-join-room (room-name)
  (interactive
   (progn
     (unless (and efire--rooms
                  efire--whoami)
       (efire-init)
       (while (not (and efire--rooms efire--whoami))
         (message "waiting for some info from the server...")
         (sit-for 0.5)))
     (list
      (completing-read
       (efire--format "join which room? ")
       (loop for room across efire--rooms
             collect (efire--get 'name room))))))

  (let ((room
         (cl-find room-name
                  efire--rooms
                  :key #'(lambda (room) (efire--get 'name room))
                  :test #'string=)))
    (with-current-buffer (efire--room-buffer room-name)

      (set (make-local-variable 'efire--room-id) (efire--get 'id room))
      (efire--message "ok this is efire--room-id %s in buffer %s" efire--room-id (current-buffer))
      (unless efire--room-id
        (error "what the fuck!!! %s" room))

      (unless (eq major-mode 'efire-mode)
        (efire-mode)
        (efire--setup-room)
        (efire--join-room #'efire--joined
                          #'efire--teardown))

      (pop-to-buffer (current-buffer)))))




(defun efire-init ()
  (interactive)
  (let ((no-rooms-fn #'(lambda (reason)
                         (efire--error "couldn't get rooms from server because %s" reason)))
        (no-identity-fn #'(lambda (reason)
                            (efire--error "couldn't get own identity from server because %s" reason))))
    (efire--request-async "rooms.json"
                          #'(lambda (obj)
                              (let ((rooms (efire--get 'rooms obj)))
                                (cond (rooms
                                       (setq efire--rooms rooms)
                                       (efire--message "got %s rooms" (length rooms)))
                                      (t
                                       (funcall no-rooms-fn "no rooms in reply")))))
                          no-rooms-fn)
    (efire--request-async "users/me.json"
                          #'(lambda (obj)
                              (let ((whoami (efire--get 'user obj)))
                                (cond (whoami
                                       (setq efire--whoami whoami)
                                       (efire--message "got own identity id=%s" (efire--get 'id whoami)))
                                      (t
                                       (funcall no-identity-fn "no user in reply")))))
                          no-identity-fn)))



;;; Helpers
;;;
(defun efire--room-buffer (room-name)
  (get-buffer-create (format "*campfire: %s*" room-name)))

(defmacro efire--with-error-checking (error-callback doing-what &rest body)
  (declare (indent 2))
  (let ((err-sym (cl-gensym)))
    `(condition-case ,err-sym
         (progn
           (efire--trace "%s" ,doing-what)
           (funcall #'(lambda () ,@body)))
       (error
        (efire--error "something went wrong %s" ,doing-what)
        (funcall ,error-callback ,err-sym)
        (when efire--debug
          (signal 'error (cdr ,err-sym)))))))


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
(defun efire--setup-room ()
  (setq efire--buffer (current-buffer)
        efire--known-users (make-hash-table)
        lui-input-function #'efire--send-message)

  (set-buffer-multibyte t)
  (tracking-add-buffer (current-buffer)))

(defun efire--teardown (reason)
  (efire--warning "Tearing down room %s in buffer %s because %s"
                  efire--room-id
                  (current-buffer)
                  reason)
  (tracking-remove-buffer (current-buffer))
  (cancel-timer efire--timer))

(defun efire--oops (reason)
  (efire--warning "Non-fatal oops in room %s in buffer %s: %s"
                  efire--room-id
                  (current-buffer)
                  reason))

(defun efire--irrelevant (whatever)
  (efire--trace "Whatever: %s" whatever))

(defun efire--register-user (user)
  (puthash (efire--get 'id user)
           user
           efire--known-users))

(defun efire--message-received (message)
  (efire--trace "Inserting message id=%s" (efire--get 'id message))
  (unless (cl-find message
                   efire--recently-inserted-own-messages
                   :key #'(lambda (message) (efire--get 'id message))
                   :test #'equal)
    (efire--insert-message (efire--find-user (efire--get 'user_id message)
                                             #'efire--register-user)
                           message))
  (setq efire--recently-inserted-own-messages '()
        efire--last-message message))

(defun efire--own-message-sent (message)
  (efire--insert-message efire--whoami message)
  (push message efire--recently-inserted-own-messages))

(defun efire--fire-timer (buffer)
  (efire--trace "Timer fired for room id=%s in buffer %s" efire--room-id buffer)
  (if (buffer-live-p buffer)
      (efire--get-recent-messages #'efire--message-received
                                  #'efire--irrelevant
                                  #'efire--oops)
    (efire--teardown "buffer was killed")))

(defun efire--start-timer ()
  (efire--info "Got %s users, starting timer" (hash-table-count efire--known-users))
  (efire--trace "Starting timer for room id=%s" efire--room-id)
  (let* ((saved-buffer (current-buffer))
         (timer (timer-create))
         (room-id efire--room-id))
    (timer-set-function timer
                        #'(lambda ()
                            (let ((efire--timer timer)
                                  (efire--room-id room-id))
                              (efire--fire-timer saved-buffer))))
    (timer-set-time timer (current-time) 2)
    (timer-activate timer)))

(defun efire--joined (_data)
  (efire--get-users efire--room-id
                    #'efire--register-user
                    #'efire--start-timer
                    #'efire--teardown))

(defun efire--input-function (input)
  (efire--send-message input
                       #'efire--sent-own-message
                       #'efire--oops))



;;; Helpers
;;;
;;;
(defun efire--find-user (user-id register-user-fn)
  (let* ((user (gethash user-id efire--known-users)))
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

(defun efire--send-message (input message-sent-callback error-callback)
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
    (efire--request-async efire--room-id
                          #'(lambda (data)
                              (efire--trace "posted message sucessfully")
                              (let ((message (efire--get 'message data)))
                                (funcall message-sent-callback message)))
                          #'(lambda (err)
                              (efire--trace "oops could not send your message because %s" err)
                              (funcall error-callback err)))))

(defun efire--get-recent-messages (message-callback done-callback error-callback)
  (let ((base-url (format "room/%d/recent.json" efire--room-id)))
    (efire--request-async
     (if efire--last-message
         (format "%s?since_message_id=%d"
                 base-url (efire--get 'id efire--last-message))
       base-url)
     #'(lambda (response)
         (efire--iterate (efire--get 'messages response)
                         message-callback
                         error-callback)
         (funcall done-callback response))
     #'(lambda (err)
         (efire--error "error getting messages for room-id=%s" efire--room-id)
         (funcall error-callback err)))))

(defun efire--join-room (done-callback error-callback)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json"))))
    (efire--message "again this is efire--room-id %s in buffer %s" efire--room-id (current-buffer))
    (efire--request-async (format "room/%d/join.json" efire--room-id)
                          #'(lambda (data)
                              (efire--info "sucessfully joined room-id=%d (got data %s)" efire--room-id data)
                              (funcall done-callback data))
                          #'(lambda (err)
                              (efire--error "error joining room-id=%d" efire--room-id)
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
                      (format "looking for %s" user)))
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
                   (funcall error-callback err)
                   (setq stop t))
               (format "processing object id=%s, which is %s" (efire--get 'id object) object)
             (funcall callback object))))


;;; Funky stuff
;;;
(defun efire--get-image (url)
  (let ((file (make-temp-file "efire-img")))
    (url-copy-file url file 'ok-if-already-exists)
    (let ((image (create-image file)))
      (if (image-animated-p image)
          (image-animate image nil 60))
      image)))

(defun efire--insert-image-maybe (message)
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       message)
    (setq message (replace-match "" t t message)))
  (when (string-match "^\\(https?\\|ftp\\)://\\([^?\r\n]+\\)\\.\\(gif\\|jpg\\|png\\|jpeg\\)$"
                      message)
    ()
    message))



;;; Processing json objects returned by campfire
;;;
(defun efire--url (path)
  (format "https://%s:X@%s/%s"
          efire-token efire-host path))

(defun efire--get (key object)
  (cdr (assoc key object)))

(defun efire--request-async (path callback error-callback)
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
                                 (funcall error-callback status))))
                        (unless efire--debug
                          (kill-buffer (current-buffer))
                          (efire--trace "leaving request buffer %s alive" (current-buffer))))
                    nil
                    'silent
                    'inhibit-cookies))))

;; curl -u 605b32dd:X -H 'Content-Type: application/json' \
;; -d '{"message":{"body":"Hello"}}' https://sample.campfirenow.com/room/1/speak.json

(defun efire--read-object ()
  (goto-char (point-min))
  (search-forward-regexp "\n\n[[:space:]]*" nil t)
  (cond ((eq (point) (point-max))
         (efire--trace "no json object in reply"))
        (t
         (let ((data (efire--chomp (decode-coding-string (buffer-substring (point)
                                                                           (point-max))
                                                         'utf-8))))


           (condition-case _err
               (json-read-from-string data)
             (error (when efire--debug
                      (efire--error "unable to read a json object from this data %s" data)
                      (pop-to-buffer (current-buffer)))))))))

(defun efire--chomp (str)
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)



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




(provide 'efire)
;;; efire.el ends here
;; Local Variables:
;; lexical-binding: t
;; End:
