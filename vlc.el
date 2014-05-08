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


(defun vlc--clean-output (output)
  "Removes some junk from the output returned by VLC."
  (let ((command-output output))
    ;; Remove the trailing ^M characters
    (setf command-output (replace-regexp-in-string "$" "" command-output))
    ;; Remove the "> " from the last line
    (setf command-output (replace-regexp-in-string "^> $" "" command-output))
    ;; Remove the last newline if needed
    ;; Only perform this step if string isn't empty
    (if (and (> (length command-output) 0)
          (string= (substring-no-properties command-output -1) "\n"))
      (substring command-output 0 -1)   ; Trim the last character
      command-output)))

(defun vlc--send-cmd (vlc-connection cmd)
  "Send command to vlc connection and return the output."
  (let ((point-in-process-buffer (with-current-buffer " *vlc-connection-telnet*"
                                   (point))))
    (vlc--send-line vlc-connection cmd)
    (with-current-buffer " *vlc-connection-telnet*"
      (while (= point-in-process-buffer (point))
        ;; Need to wait for the buffer's process to receive something from vlc
        ;; TODO: Wait only for the timeout
        (sleep-for 0.1))
      ;; Clean the output returned by vlc
      (let ((command-output (buffer-substring-no-properties point-in-process-buffer (point))))
        (vlc--clean-output command-output)))))

(defun vlc-cmd-logout (vlc-connection)
  "Logout of the VLC instance of VLC-CONNECTION"
  (vlc--send-cmd vlc-connection "exit")
  ;; Need to call `process-send-eof' because VLC doesn't close the
  ;; connection when the client exits with "exit".
  ;;
  ;; This doesn't need to be used for `vlc-cmd-shutdown' and
  ;; `vlc-cmd-quit'.
  (process-send-eof (vlc-connection-telnet vlc-connection))
  (setf (vlc-connection-telnet vlc-connection) nil))

(defun vlc-cmd-shutdown (vlc-connection)
  "Close the VLC instance of VLC-CONNECTION"
  (vlc--send-cmd vlc-connection "shutdown")
  (setf (vlc-connection-telnet vlc-connection) nil))

(defun vlc-cmd-quit (vlc-connection)
  "Close the VLC connection of VLC-CONNECTION."
  (vlc--send-cmd vlc-connection "quit")
  (setf (vlc-connection-telnet vlc-connection) nil))



(provide 'vlc)
;;; vlc.el ends here

