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



;;; Interactive stuff
;;;
(defun efire-join-room (room)
  (interactive
   (progn
     (efire--message "getting room names...")
     (setq efire--rooms (efire--object 'rooms (efire--request "rooms.json")))
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
           (efire--setup-timer room)))
    (pop-to-buffer (current-buffer))))


;;; Helpers
;;;
(defun efire--room-buffer (room)
  (get-buffer-create (format "*campfire: %s*" (efire--get 'name room))))

(define-derived-mode efire-mode lui-mode "efire"
  "A major mode for campfire rooms"
  (lui-set-prompt "\n: "))

(defun efire--setup-timer (room)
  (let* (last-message
         (buffer (current-buffer))
         timer
         (cancel-fn #'(lambda (&optional _dummy)
                        (efire--warning "cancelling timer in buffer %s" buffer)
                        (cancel-timer timer)))
         (known-users (make-hash-table)))
    (set-buffer-multibyte t)

    (set (make-local-variable 'lui-input-function)
         #'(lambda (input)
             (efire--send-message room input)))

    (setq timer
          (run-at-time
           nil 2
           #'(lambda ()
               (unless (buffer-live-p buffer) (funcall cancel-fn))
               (efire--get-recent-messages (efire--get 'id room)
                                           (and last-message
                                                (efire--get 'id last-message))
                                           #'(lambda (message)
                                               (with-current-buffer buffer
                                                 (efire--insert-message
                                                  (efire--find-user message known-users)
                                                  message))
                                               (setq last-message message))
                                           cancel-fn))))))

(defun efire--find-user (message known-users)
  (let* ((user-id (efire--get 'user_id message))
         (user (gethash user-id known-users)))
    (efire--info "looking up user %s and got %s" user-id user)
    (cond ((and user-id
                (not user))
           (puthash user-id
                    (efire--get 'user (efire--request (format "users/%s.json" user-id)))
                    known-users))
          (t
           user))))

(defun efire--send-message (room input)
  (let* ((message-type (if (string-match "\n" input)
                           (progn
                             (replace-regexp-in-string "\n"
                                                       "&#xA;"
                                                       input)
                             "PasteMessage")
                         "TextMessage"))
         (url-request-method "POST")
         ;; (url-mime-charset-string nil)
         ;; (url-extensions-header nil)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ;; ("User-Agent" . "efire (joaotavora@gmail.com)")
            ))
         (url-request-data
          (json-encode `((message  (body . ,input)
                                   (type . ,message-type))))))
    (efire--request-async (format "room/%d/speak.json" (efire--get 'id room))
                          #'(lambda (data)
                              (efire--info "posted sucessfully and server replied with %s" data))
                          #'(lambda (err)
                              (efire--error "oops could not send your message because %s" err)))))

(defun efire--get-recent-messages (room-id last-message-id callback &optional error-callback)
  (let ((base-url (format "room/%d/recent.json" room-id)))
    (efire--request-async
     (if last-message-id
         (format "%s?since_message_id=%d"
                 base-url last-message-id)
       base-url)
     #'(lambda (response)
         (loop with stop = nil
               for message across (efire--get 'messages response)
               while (not stop)
               do (efire--with-error-checking
                      #'(lambda (err)
                          (when error-callback
                            (funcall error-callback err))
                          (setq stop t))
                      (format "processing message id %s" (efire--get 'id message))
                    (funcall callback message))))
     #'(lambda (err)
         (efire--error "error getting messages for room-id=%s" room-id)
         (when error-callback (funcall error-callback err))))))

(defun efire--insert-message (user message)
  (let* ((type (efire--get 'type message))
         (user-name (if user
                        (propertize (efire--get 'name user)
                                    'face 'font-lock-keyword-face)
                      (format "looking for %s" (efire--get 'user_id message))))
         (type-sym (intern type))
         (lui-fill-type (if (eq type-sym 'PasteMessage)
                            nil
                          lui-fill-type)))
    (cond ((memq type-sym
                 '(TextMessage PasteMessage))
           (lui-insert (format "%s: %s%s"
                               user-name
                               (if (eq type-sym 'PasteMessage)
                                   "\n"
                                 "")
                               (efire--get 'body message)))))))

(defun efire--get (key object)
  (cdr (assoc key object)))


;;; Processing json objects returned by campfire
;;;
(defun efire--url (path)
  (format "https://%s:X@%s/%s"
          efire-token efire-host path))

(defun efire--object (key json-object)
  (cdr (assoc key json-object)))

(defun efire--request (path)
  (let ((buffer
         (efire--ignoring-errors-maybe
          (url-retrieve-synchronously (efire--url path)))))
    (and buffer
         (with-current-buffer buffer
           (efire--read-object)))))

(defun efire--request-async (path callback &optional error-callback)
  (let ((url (url-encode-url
              (efire--url path))))
    (efire--with-error-checking
        error-callback
        (format "getting url %s" url)
      (url-retrieve url
                    #'(lambda (status)
                        (cond ((null status)
                               (efire--info "calling the url-retrieve callback")
                               (funcall callback (efire--read-object)))
                              (t
                               (efire--error "status was %s" status)
                               (when error-callback
                                 (funcall error-callback status))))
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
         (funcall #'(lambda () ,@body))
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
