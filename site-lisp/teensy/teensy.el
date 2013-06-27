;;; teensy.el --- Autocomplete and stub projects for teensy/teensyduino

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: c, hardware, vc
;; Package-Requires: ((dollaro . "0.2") (s . "1.6.0"))

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

(require 'dollaro)

(defgroup teensy nil
  "Some helpers to simplify teensy/teensyduino development on emacs.")

(defcustom teensy:sketch-directory user-emacs-directory
  "The path to the directory containing your arduino sketches."
  :group 'teensy
  :type 'directory)

(defcustom teensy:gateway-port 28541
  "The port where teensy-gateway is running on localhost."
  :group 'teensy
  :type 'integer)

(defcustom teensy:arduino-directory (getenv "TEENSYDUINO_HOME")
  "The path to the arduino/teensyduino installation."
  :group 'teensy
  :type 'directory)

(defvar teensy::template-directory
  (concat (file-name-directory load-file-name) "templates/"))

(defcustom teensy:arduino-sketch-template (concat teensy::template-directory "sketch.pde.$")
  "The dollaro template file for arduino sketches."
  :group 'teensy
  :type 'integer)

(defcustom teensy:clang-ignored-flags '("-mmcu" "-fno-exceptions")
  "A list of c++ flags that should not be passed to the
completion process."
  :type '(repeat string)
  :group 'teensy)

(defvar teensy::arduino-platform (case system-type
                                   (gnu/linux  "Linux")
                                   (darwin  "Darwin")))

(defvar teensy::gateway-host "localhost"
  "The host were the teensy board is connected to.")

(defvar teensy::teensy-gateway-process nil
  "The teensy gateway process. teensy_gateway will be run when
  issuing `teensy:arduino-monitor'")

(defvar teensy::arduino-monitor-buffer-name "Teensy Monitor"
  "The name of the teensy arduino monitor buffer.")

(defvar teensy:board-type nil)
(make-variable-buffer-local 'teensy:board-type)

(defun teensy::teensy-gateway-running-p ()
  "Returns non nil if teensy_gateway is running."
  (and teensy::teensy-gateway-process
       (processp teensy::teensy-gateway-process)
       (process-live-p teensy::teensy-gateway-process)))

(defun teensy::teensy-gateway-command ()
  "Returns the absolute path of the teensy_gateway command."
  (concat teensy:arduino-directory "hardware/tools/teensy_gateway"))

(defun teensy::run-teensy-gateway ()
  "Starts the teensy_gateway process."
  (setq teensy::teensy-gateway-process
        (start-process "teensy-gateway" " teensy-gateway" (teensy::teensy-gateway-command))))

(defun teensy::maybe-kill-telnet-buffer ()
  "Kills the monitor buffer if the teensy_gateway process is not
running."
  (let* ((buffer (get-buffer teensy::arduino-monitor-buffer-name))
         (process (get-buffer-process buffer)))
    (when (and buffer (or (null process)
                          (not (process-live-p process))))
      (kill-buffer buffer))))

;;;###autoload
(defun teensy:arduino-monitor ()
  "Shows the arduino monitor buffer."
  (interactive)
  (when (not (teensy::teensy-gateway-running-p))
    (teensy::run-teensy-gateway)
    (sleep-for 0 500))
  (teensy::maybe-kill-telnet-buffer)
  (if (get-buffer teensy::arduino-monitor-buffer-name)
      (switch-to-buffer-other-window teensy::arduino-monitor-buffer-name)
    (telnet teensy::gateway-host teensy:gateway-port)
    (let* ((hname (concat teensy::gateway-host ":" (format "%s" teensy:gateway-port)))
           (name (concat telnet-program "-" (comint-arguments hname 0 nil) ))
           (buffer (get-buffer (concat "*" name "*")))
           (process (get-buffer-process buffer)))
      (with-current-buffer buffer (rename-buffer "Teensy Monitor")))))

(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode))

(defun teensy::format-env-vars-for-make ()
  (unless teensy:board-type
    (setq teensy:board-type "2"))
  (format "TEENSY=%s ARDUINO_%s=%s "
          teensy:board-type
          teensy::arduino-platform
          teensy:arduino-directory))


(defun teensy::clang-flags ()
  "Retrieves the flags that will be given to the completion
process by running make cppflags."
  (let ((dir (file-name-directory (buffer-file-name))))
    (when (file-exists-p "Makefile")
      (let ((cppflags (shell-command-to-string (concat (teensy::format-env-vars-for-make)
                                                       "make cppflags"))))
        (remove-if (lambda (el)
                     (member (first (split-string el "="))
                             teensy:clang-ignored-flags))
                   (mapcar 's-trim (split-string cppflags " ")))))))


;; Add the flags needed for the completion process to find the arduino
;; libraries.
(defun teensy::add-teensyduino-cppflags ()
  (when (s-match "^.*\\.pde$" (buffer-name))
    (make-local-variable 'ac-clang-flags)
    (setq ac-clang-flags  (append ac-clang-flags (ignore-errors (teensy::clang-flags))))))

(add-hook 'c++-mode-hook
          'teensy::add-teensyduino-cppflags)

;;; TODO: functions to create a teensyduino template project
;;;       integrate makefile with projectile

;; TODO: the user should be able to select the board type
;; TODO: per sketch directory, fix the code below
(defun teensy::ensure-sketch-directory-exists (sketch-name)
  (let ((sketch-directory (concat teensy:sketch-directory sketch-name "/")))
    (unless (file-exists-p sketch-directory)
      (make-directory sketch-directory t))
    (unless (file-exists-p (concat sketch-directory "teensyduino.mk"))
      (copy-file (concat teensy::template-directory "teensyduino.mk") sketch-directory))
    (unless (file-exists-p (concat sketch-directory "Makefile"))
      (copy-file (concat teensy::template-directory "Makefile") sketch-directory))
    sketch-directory))

(defun teensy:new-sketch (sketch-name)
  (interactive "sSketch name: ")
  (let ((sketch-directory (teensy::ensure-sketch-directory-exists sketch-name)))
    (let ((destination-file (concat sketch-directory sketch-name)))
      (unless (s-ends-with? ".pde" destination-file)
        (setq destination-file (concat destination-file ".pde")))
      (cond ((or (not (file-exists-p destination-file))
                 (yes-or-no-p (format "The sketch '%S' already exists in your sketch directory, overwrite?"
                                      sketch-name)))
             ($:fill-template-from-file teensy:arduino-sketch-template
                                        destination-file
                                        `((filename . ,(file-name-base destination-file))
                                          (user-name . ,(if (and user-full-name (s-blank? user-full-name))
                                                            user-full-name
                                                          user-login-name))
                                          (user-email . ,user-mail-address)
                                          (date . ,(format-time-string "%Y-%m-%d"))))
             (find-file destination-file))
            (t (message "Canceled by user."))))))

(defun teensy::compile ()
  (interactive)
  (compile (concat (teensy::format-env-vars-for-make)
                   (format "TARGET=%s make clean upload"
                           (file-name-base (buffer-file-name))))
           t))

(provide 'teensy)
;;; teensy.el ends here
