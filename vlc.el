;;; vlc.el --- Control a VLC player through its telnet interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <finalyugi@sapo.pt>
;; Keywords: tools, multimedia

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

;; 

;;; Code:

(require 'cl-lib)

(cl-defstruct vlc-connection (host "localhost") (port 4212) (timeout 10) (password "admin") debug telnet)

(defun vlc--process-send-line (process string)
  "Send string to process. Appends a \n at the end of string."
  (process-send-string process (concat string "\n")))

(defun vlc--send-line (vlc-connection string)
  (let ((telnet-interface (vlc-connection-telnet vlc-connection)))
    (vlc--process-send-line telnet-interface string)))

(defun vlc--read-output (vlc-connection)
  "Read output from vlc-connection process"
  (let ((telnet-interface (vlc-connection-telnet vlc-connection)))
    (vlc--)))

(defun vlc-cmd-login (vlc-connection)
  (let ((host (vlc-connection-host vlc-connection))
         (port (vlc-connection-port vlc-connection))
         (timeout (vlc-connection-timeout vlc-connection))
         (password (vlc-connection-password vlc-connection))
         (debug (vlc-connection-debug vlc-connection))
         (telnet (vlc-connection-telnet vlc-connection)))
    (unless telnet
      (setf (vlc-connection-telnet vlc-connection)
        (open-protocol-stream "vlc-connection-telnet"
          (get-buffer-create " *vlc-connection-telnet*")
          (vlc-connection-host vlc-connection)
          (vlc-connection-port vlc-connection))))
    ;; Perform the initial login
    (vlc--process-send-line (vlc-connection-telnet vlc-connection) "admin")))




(defun vlc--send-cmd (vlc-connection cmd)
  "Send command to vlc connection and return the output."
  (let ((point-in-process-buffer (with-current-buffer " *vlc-connection-telnet*"
                                   (point))))
    (vlc--send-line vlc-connection cmd)
    (with-current-buffer " *vlc-connection-telnet*"
      (while (= point-in-process-buffer (point))
        ;; Need to add for the process-buffer to receive some thing from vlc
        ;; TODO: Wait only for the timeout
        (sleep-for 0.5))
      (let ((command-output (buffer-substring-no-properties point-in-process-buffer (point))))
        ;; Remove the trailing ^M characters
        (setf command-output (replace-regexp-in-string "$" "" command-output))
        ;; Remove the "> " from the last line
        (setf command-output (replace-regexp-in-string "^> $" "" command-output))
        ;; Remove the last newline if needed
        (if (string= (substring command-output -1) "\n")
          (substring command-output 0 -1)
          command-output)))))



(provide 'vlc)
;;; vlc.el ends here

