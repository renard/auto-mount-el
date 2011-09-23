;;; auto-mounter.el --- Manage automounts using dbus.

;; Copyright © 2011 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2011-09-22
;; Last changed: 2011-09-23 09:05:38
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(defcustom am:usdisk-bin "udisks"
  "udisks binary file.")


(defcustom am:disks-mount "--mount"
  "Option for udisks monitor. See `am:usdisk-bin'.")
(defcustom am:disks-unmount "--unmount"
  "Option for udisks monitor. See `am:usdisk-bin'.")

(defcustom am:pre-mount-hook nil "")
(defcustom am:post-mount-hook nil "")
(defcustom am:pre-unmount-hook nil "")
(defcustom am:post-unmount-hook nil "")


(defvar am:mounted-devices nil
  "List of mounted devices")


;; (defun am:get-added-device (string)
;;   ""
;;   (save-match-data
;;     (when
;; 	(string-match 
;; 	 "^added:\\s-+/org/freedesktop/UDisks/devices/\\(.+\\)" 
;; 	 string)
;;     (format "/dev/%s" (match-string 1 string)))))


;; (defun am:filter (proc string)
;;   ""
;;   (let ((device (am:get-added-device string)))
;;     (save-match-data
;;       (cond
;;        ((string-match 
;; 	 "^added:\\s-+/org/freedesktop/UDisks/devices/\\(.+\\)" 
;; 	 string)
;; 	(let ((device (format "/dev/%s" (match-string 1 string))))
;; 	  (run-hook-with-args am:pre-mount-hook device)
;; 	  (am:mount-device device)))
;;        ((string-match 
;; 	 "^removed:\\s-+/org/freedesktop/UDisks/devices/\\(.+\\)" 
;; 	 string)
;; 	(let ((device (format "/dev/%s" (match-string 1 string))))
;; 	  ;;remove device from am:mounted-devices
;; 	  (setq am:mounted-devices
;; 		(assq-delete-all device am:mounted-devices))))))))

;; (defun am:start ()
;;   "Start udisk monitoring."
;;   (interactive)
;;   (let* ((cmd-line (list am:usdisk-bin am:disks-monitor))
;; 	 (cmd-buf (get-buffer-create "Udisk daemon"))
;; 	 (proc (apply 'start-process (car cmd-line)
;; 		      cmd-buf (car cmd-line) (cdr cmd-line))))
;;     (process-put proc :cmd-buf cmd-buf)
;;     (set-process-filter proc 'am:filter)
;;     (set-process-sentinel proc 'am:sentinel)
;;     (switch-to-buffer cmd-buf)))


;; (defun am:mount-filter (proc string)
;;   (save-match-data
;;     (when (string-match "^Mounted .+ at \\(.+\\)" string)
;;       (let ((mount-point (match-string 1 string))
;; 	    (device (process-get proc :device)))
;; 	(add-to-list 'am:mounted-devices `(,(intern device) . ,mount-point))
;; 	(run-hook-with-args am:post-mount-hook device mount-point)))))

;; (defun am:mount-sentinel (proc change)
;;   "Sentinel in charge of running next process if previous one succeeded."
;;   (when (eq (process-status proc) 'exit)
;;     (let ((status  (process-exit-status proc))
;; 	  (cmd-buf (process-get proc :cmd-buf))
;; 	  (device (process-get proc :device)))
;;       (kill-buffer cmd-buf))))

;; (defun am:mount-device (device)
;;   "Try to mount DEVICE using `am:usdisk-bin'."
;;   (let* ((cmd-line (list am:usdisk-bin am:disks-mount device))
;; 	 (cmd-buf (get-buffer-create (format "Udisk mounting %s" device)))
;; 	 (proc (apply 'start-process (car cmd-line)
;; 		      cmd-buf (car cmd-line) (cdr cmd-line))))
;;     (process-put proc :cmd-buf cmd-buf)
;;     (process-put proc :device device)
;;     (set-process-filter proc 'am:mount-filter)
;;     (set-process-sentinel proc 'am:mount-sentinel)))
  













(defun am:post-mount-sentinel (proc change)
  "Sentinel in charge of running next process if previous one succeeded."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cmd-buf (process-get proc :cmd-buf))
	  (description (process-get proc :description))
	  (device (process-get proc :device))
	  (uuid (process-get proc :uuid))
	  (mount-point (process-get proc :mount-point)))
      (add-to-list 'am:mounted-devices `(,mount-point . ,description))
      (run-hook-with-args 'am:post-mount-hook description device uuid mount-point)
      (kill-buffer cmd-buf))))



(defun am:mount-signal-handler (str1 str2 msg)
  (let* ((description (cadr msg))
	 (entries (nthcdr (- (length msg) 1) msg))
	 (device (cadaar entries))
	 (uuid (car (cdadar entries)))
	 (mount-point (format "/media/%s" uuid)))
    (run-hook-with-args 'am:pre-mount-hook description device uuid mount-point)
    ;; start async process to mount drive
    (let* ((cmd-line (list am:usdisk-bin am:disks-mount device))
	   (cmd-buf (get-buffer-create (format "Udisk mounting %s" device)))
	   (proc (apply 'start-process (car cmd-line)
			cmd-buf (car cmd-line) (cdr cmd-line))))
      (process-put proc :cmd-buf cmd-buf)
      (process-put proc :description description)
      (process-put proc :device device)
      (process-put proc :uuid uuid)
      (process-put proc :mount-point mount-point)
      (set-process-sentinel proc 'am:post-mount-sentinel))))


(defun am:open-in-dired (description device uuid mount-point)
  ""
  (dired mount-point))
(add-hook 'am:post-mount-hook 'am:open-in-dired)


(defun am:unmount-signal-handler (str1 str2 msg)
  (message (format "D-Bus message: %S" msg))

  (let* ((description (cadr msg))
	 (url (car (nthcdr 4 msg)))
	 (mount-point (url-filename (url-generic-parse-url url))))
    
    (setq am:mounted-devices 
	  (loop for m in am:mounted-devices
		when (not (string= mount-point (car m)))
		collect m))
    ;;Add post umount hooks
))




(defun am:unmount ()
  (interactive)
  (let* ((wanted (completing-read "Unmount: "
				 (append 
				  (mapcar 'car am:mounted-devices)
				  (mapcar 'cdr am:mounted-devices))
				 nil
				 t))
	 (mount-point (or (car (assoc wanted am:mounted-devices))
			  (car (rassoc wanted am:mounted-devices)))))
    ;; REally do the unmount
    (message (format "Unmounting %s" mount-point))))

  


(defun am:dbus-register ()
  ""
  (dbus-register-signal :session nil "/org/gtk/Private/RemoteVolumeMonitor"
			"org.gtk.Private.RemoteVolumeMonitor"
			"VolumeAdded" 'am:mount-signal-handler)

  (dbus-register-signal :session nil "/org/gtk/Private/RemoteVolumeMonitor"
			"org.gtk.Private.RemoteVolumeMonitor"
			"MountRemoved" 'am:unmount-signal-handler))

(provide 'auto-mounter)
