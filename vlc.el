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
  "Send STRING to the telnet connection associated to VLC-CONNECTION.

String should not contain the trailing new line."
  ;; VLC-CONNECTION must have an active telnet process
  (unless (vlc-connection-telnet vlc-connection)
    (error "VLC-CONNECTION does not have a telnet process, please run `vlc-cmd-login' first"))
  (let ((telnet-interface (vlc-connection-telnet vlc-connection)))
    (vlc--process-send-line telnet-interface string)))


(defun vlc-cmd-login (vlc-connection)
  "Login into the VLC specified in VLC-CONNECTION using the telnet interface.

Creates a new telnet connection if needed.

Throws a `file-error' signal if the connection can't be open.

Doesn't return anything."
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
          host port)))
    ;; Perform the initial login
    (vlc--process-send-line (vlc-connection-telnet vlc-connection) password)))


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
  "Send command to vlc connection and return the output.

CMD should be a string similar to \"add foobar\"."
  (let ((point-in-process-buffer (with-current-buffer " *vlc-connection-telnet*"
                                   (point))))
    (vlc--send-line vlc-connection cmd)
    (with-current-buffer " *vlc-connection-telnet*"
      (while (= point-in-process-buffer (point))
        ;; Need to wait for the buffer's process to receive something from vlc
        ;; This wait is here due to the asynchronous nature of the process
        ;; communication framework used by Emacs.
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


(defun vlc--name-of-optional-argument (argument)
  ;; Optional arguments are represented as a single symbol inside a vector e.g. [foo]
  ;;
  ;; The name of the optional argument is the name of the symbol
  ;; inside that vector
  (unless (vlc--optional-argument-p argument)
    (error "%s is not an optional argument list" argument))
  (symbol-name (aref argument 0)))

(defun vlc--optional-argument-p (argument-list)
  (vectorp argument-list))

(defun vlc--convert-function-name-to-vlc-command (symbol)
  ;; Extract the name of a VLC command from the a name of a command
  (unless (string-prefix-p "vlc-cmd-" (symbol-name symbol))
    (error "%s is not a vlc command" symbol))
  (let ((vlc-command (cl-subseq (symbol-name symbol) (length "vlc-cmd-"))))
    (substring-no-properties vlc-command)))



(defun vlc--last-item (list)
  "Return the last `car' of LIST."
  (car (last list)))


(defmacro vlc--defcommand (name &rest rest)
  "Create a function that sends command NAME to a vlc-connection."
  ;; `fixed-name' contains `name' except with the "_" characters converted into
  ;; "-". This is done so that instead of this macro creating the function
  ;; `vlc-cmd-get_time', it will create `vlc-cmd-get-time'.
  (let ((fixed-name (intern (replace-regexp-in-string "_" "-" (symbol-name name)))))
    (if (and (vlc--optional-argument-p (car rest))
          (= (length (car rest)) 3))
      `(vlc--defcommand-with-on-or-off ,fixed-name ,@rest)
      `(vlc--defcommand-normal ,fixed-name ,@rest))))


(defmacro vlc--defcommand-normal (name &rest rest)
  (let* ((command-name (intern (format "vlc-cmd-%s" name)))
          (command-args (butlast rest))
          (has-optional-arguments (cl-some #'vlc--optional-argument-p command-args))
          (docstring (vlc--last-item rest)))
    ;; Add the &optional symbol to `command-args' if the vlc command contains an optional argument
    (when has-optional-arguments
      (push '&optional command-args)
      (setf command-args (mapcar (lambda (arg)
                                   (if (vlc--optional-argument-p arg)
                                     (intern (vlc--name-of-optional-argument arg))
                                     arg))
                           command-args)))
    `(defun ,command-name (vl ,@command-args)
       ,docstring
       (vlc--send-cmd vl (concat ,(vlc--convert-function-name-to-vlc-command command-name) " " ,(vlc--last-item command-args))))))



(defmacro vlc--defcommand-with-on-or-off (name args docstring)
  (declare (ignore args)) ;; we know that `args' is always the vector [on :or off]
  (let ((command-name (intern (format "vlc-cmd-%s" name))))
    `(defun ,command-name (vl &optional switch)
       ,docstring
       (if switch
         (if (member switch '(on off))
           (vlc--send-cmd vl (concat ,(vlc--convert-function-name-to-vlc-command command-name)
                             " " (symbol-name switch)))
           (error "argument `switch' doesn't have the value \"on\" or \"off\". Its value is: ~A" switch))
         ;; Don't use "(symbol-name switch)" since that would send a string "nil" to VLC
         (cmd vl (concat ,(vlc--convert-function-name-to-vlc-command command-name)))))))



;;; Create the wrappers around the VLC commands
(progn
  (vlc--defcommand add XYZ "add XYZ to playlist")
  (vlc--defcommand enqueue XYZ "queue XYX to playlist")
  (vlc--defcommand playlist "show items currently in playlist")
  (vlc--defcommand search [string] "search for items in playlist (or reset search)")
  (vlc--defcommand sort key "sort the playlist")
  (vlc--defcommand sd [sd] "show services discovery or toggle")
  (vlc--defcommand play "play stream")
  (vlc--defcommand stop "stop stream")
  (vlc--defcommand next "next playlist item")
  (vlc--defcommand prev "previous playlist item")
  (vlc--defcommand goto "goto item at index")
  (vlc--defcommand gotoitem "goto item at index")
  (vlc--defcommand repeat [on :or off] "toggle playlist repeat")
  (vlc--defcommand loop [on :or off] "toggle playlist loop")
  (vlc--defcommand random [on :or off] "toggle playlist random")
  (vlc--defcommand clear "clear the playlist")
  (vlc--defcommand status "current playlist status")
  (vlc--defcommand title [x] "set/get title in current item")
  (vlc--defcommand title_n "next title in current item")
  (vlc--defcommand title_p "previous title in current item")
  (vlc--defcommand chapter [x] "set/get chapter in current item")
  (vlc--defcommand chapter_n "next chapter in current item")
  (vlc--defcommand chapter_p "previous chapter in current item")
  (vlc--defcommand seek X "seek in seconds, for instance `seek 12'")
  (vlc--defcommand pause "toggle pause")
  (vlc--defcommand fastforward "set to maximum rate")
  (vlc--defcommand rewind "set to minimum rate")
  (vlc--defcommand faster "faster playing of stream")
  (vlc--defcommand slower "slower playing of stream")
  (vlc--defcommand normal "normal playing of stream")
  (vlc--defcommand rate [playback-rate] "set playback rate to value")
  (vlc--defcommand frame "play frame by frame")
  (vlc--defcommand fullscreen [on :or off] "toggle fullscreen")
  (vlc--defcommand f [on :or off] "toggle fullscreen")
  (vlc--defcommand info "information about the current stream")
  (vlc--defcommand stats "show statistical information")
  (vlc--defcommand get_time "seconds elapsed since stream's beginning")
  (vlc--defcommand is_playing "1 if a stream plays, 0 otherwise")
  (vlc--defcommand get_title "the title of the current stream")
  (vlc--defcommand get_length "the length of the current stream")
  (vlc--defcommand volume [X] "set/get audio volume")
  (vlc--defcommand volup [X] "raise audio volume X steps")
  (vlc--defcommand voldown [X] "lower audio volume X steps")
  (vlc--defcommand adev [X] "set/get audio device")
  (vlc--defcommand achan [X] "set/get audio channels")
  (vlc--defcommand atrack [X] "set/get audio track")
  (vlc--defcommand vtrack [X] "set/get video track")
  (vlc--defcommand vratio [X] "set/get video aspect ratio")
  (vlc--defcommand vcrop [X] "set/get video crop")
  (vlc--defcommand crop [X] "set/get video crop")
  (vlc--defcommand vzoom [X] "set/get video zoom")
  (vlc--defcommand zoom [X] "set/get video zoom")
  (vlc--defcommand vdeinterlace [X] "set/get video deinterlace")
  (vlc--defcommand vdeinterlace_mode [X] "set/get video deinterlace mode")
  (vlc--defcommand snapshot "take video snapshot")
  (vlc--defcommand strack [X] "set/get subtitles track")
  (vlc--defcommand vlm "load the VLM")
  (vlc--defcommand description "describe this module")
  (vlc--defcommand help [pattern] "a help message") ; Do not use the alias "?"
  (vlc--defcommand longhelp [pattern] "a longer help message")
  (vlc--defcommand lock "lock the telnet prompt")
  ;; These commands need some extra processing (i.e. closing the telnet connections so they're
  ;; defined as normal lisp functions instead of using the `defcommand' macro
  ;; (vlc--defcommand quit "quit VLC (or logout if in a socket connection)")
  ;; (vlc--defcommand shutdown "shutdown VLC")
  ;; (vlc--defcommand logout "exit (if in a socket connection)")
  )


;;; List of VLC commands as obtained from using the "help" command in the terminal
;; +----[ CLI commands ]
;; | add XYZ  . . . . . . . . . . . . . . . . . . . . add XYZ to playlist
;; | enqueue XYZ  . . . . . . . . . . . . . . . . . queue XYZ to playlist
;; | playlist . . . . . . . . . . . . . .show items currently in playlist
;; | search [string]  . .  search for items in playlist (or reset search)
;; | sort key . . . . . . . . . . . . . . . . . . . . . sort the playlist
;; | sd [sd]  . . . . . . . . . . . . . show services discovery or toggle
;; | play . . . . . . . . . . . . . . . . . . . . . . . . . . play stream
;; | stop . . . . . . . . . . . . . . . . . . . . . . . . . . stop stream
;; | next . . . . . . . . . . . . . . . . . . . . . .  next playlist item
;; | prev . . . . . . . . . . . . . . . . . . . .  previous playlist item
;; | goto, gotoitem . . . . . . . . . . . . . . . . . .goto item at index
;; | repeat [on|off]  . . . . . . . . . . . . . .  toggle playlist repeat
;; | loop [on|off]  . . . . . . . . . . . . . . . .  toggle playlist loop
;; | random [on|off]  . . . . . . . . . . . . . .  toggle playlist random
;; | clear  . . . . . . . . . . . . . . . . . . . . . .clear the playlist
;; | status . . . . . . . . . . . . . . . . . . . current playlist status
;; | title [X]  . . . . . . . . . . . . . . set/get title in current item
;; | title_n  . . . . . . . . . . . . . . . .  next title in current item
;; | title_p  . . . . . . . . . . . . . .  previous title in current item
;; | chapter [X]  . . . . . . . . . . . . set/get chapter in current item
;; | chapter_n  . . . . . . . . . . . . . .  next chapter in current item
;; | chapter_p  . . . . . . . . . . . .  previous chapter in current item
;; |
;; | seek X . . . . . . . . . . . seek in seconds, for instance `seek 12'
;; | pause  . . . . . . . . . . . . . . . . . . . . . . . .  toggle pause
;; | fastforward  . . . . . . . . . . . . . . . . . . set to maximum rate
;; | rewind . . . . . . . . . . . . . . . . . . . . . set to minimum rate
;; | faster . . . . . . . . . . . . . . . . . .  faster playing of stream
;; | slower . . . . . . . . . . . . . . . . . .  slower playing of stream
;; | normal . . . . . . . . . . . . . . . . . .  normal playing of stream
;; | rate [playback rate] . . . . . . . . . .  set playback rate to value
;; | frame  . . . . . . . . . . . . . . . . . . . . . play frame by frame
;; | fullscreen, f, F [on|off]  . . . . . . . . . . . . toggle fullscreen
;; | info . . . . . . . . . . . . . .information about the current stream
;; | stats  . . . . . . . . . . . . . . . .  show statistical information
;; | get_time . . . . . . . . . .seconds elapsed since stream's beginning
;; | is_playing . . . . . . . . . . . .  1 if a stream plays, 0 otherwise
;; | get_title  . . . . . . . . . . . . . the title of the current stream
;; | get_length . . . . . . . . . . . .  the length of the current stream
;; |
;; | volume [X] . . . . . . . . . . . . . . . . . .  set/get audio volume
;; | volup [X]  . . . . . . . . . . . . . . . .raise audio volume X steps
;; | voldown [X]  . . . . . . . . . . . . . .  lower audio volume X steps
;; | adev [X] . . . . . . . . . . . . . . . . . . . .set/get audio device
;; | achan [X]  . . . . . . . . . . . . . . . . . .set/get audio channels
;; | atrack [X] . . . . . . . . . . . . . . . . . . . set/get audio track
;; | vtrack [X] . . . . . . . . . . . . . . . . . . . set/get video track
;; | vratio [X] . . . . . . . . . . . . . . . .set/get video aspect ratio
;; | vcrop, crop [X]  . . . . . . . . . . . . . . . .  set/get video crop
;; | vzoom, zoom [X]  . . . . . . . . . . . . . . . .  set/get video zoom
;; | vdeinterlace [X] . . . . . . . . . . . . . .set/get video deintelace
;; | vdeinterlace_mode [X]  . . . . . . . . set/get video deintelace mode
;; | snapshot . . . . . . . . . . . . . . . . . . . . take video snapshot
;; | strack [X] . . . . . . . . . . . . . . . . . set/get subtitles track
;; |
;; | vlm  . . . . . . . . . . . . . . . . . . . . . . . . . .load the VLM
;; | description  . . . . . . . . . . . . . . . . . .describe this module
;; | help, ? [pattern]  . . . . . . . . . . . . . . . . . .a help message
;; | longhelp [pattern] . . . . . . . . . . . . . . a longer help message
;; | lock . . . . . . . . . . . . . . . . . . . .  lock the telnet prompt
;; | logout . . . . . . . . . . . . . .  exit (if in a socket connection)
;; | quit . . . . . . . .  quit VLC (or logout if in a socket connection)
;; | shutdown . . . . . . . . . . . . . . . . . . . . . . . .shutdown VLC
;; +----[ end of help ]
;;>




(provide 'vlc)
;;; vlc.el ends here

