;;; auto-mounter.el --- Manage automounts using dbus.

;; Copyright © 2011 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2011-09-22
;; Last changed: 2011-09-23 17:57:01
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-when-compile (require 'dbus))

(defcustom am:usdisk-bin "udisks"
  "udisks binary file.")

(defcustom am:disks-mount "--mount"
  "Option for udisks monitor. See `am:usdisk-bin'.")

(defcustom am:pre-mount-hook nil
  "Hooks to be called before mounting the drive.

Arguments are:

DESCRIPTION for the drive description given by D-Bus.

DEVICE for the device name such as /desv/sdb1.

UUID drive uuid.

MOUNT-POINT where the drive is mounted. Usually /media/UUID but
may be different.")

(defcustom am:post-mount-hook nil
  "Hooks to be called after mounting the drive.

See `am:pre-mount-hook' for arguments.")

(defcustom am:pre-unmount-hook nil
  "Hooks to be called before unmounting the drive with
MOUNT-POINT as argument.")

(defcustom am:unmount-failed-hook nil
  "Hooks to be called when unmount fails (usually when a resource
 is busy) with the MOINT-POINT as argument.")

(defcustom am:post-unmount-hook nil
  "Hooks to be called when unmount succeeded with the MOINT-POINT
as argument.")

(defvar am:mounted-devices nil
  "List of mounted devices.")

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



(defun am:unmount-sentinel (proc change)
  "Sentinel in charge of running next process if previous one succeeded."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cmd-buf (process-get proc :cmd-buf))
	  (mount-point (process-get proc :mount-point)))

      (if (not (eq 0 status))
	  (progn
	    (set-buffer cmd-buf)
	    (message (format "Could not unmount %s:\n%s"
			     mount-point (buffer-string)))
	    (kill-buffer cmd-buf)
	    (run-hook-with-args am:unmount-failed-hook mount-point))
	(run-hook-with-args 'am:post-unmount-hook mount-point)
	(kill-buffer cmd-buf)))))

(defun am:unmount (&optional drive)
  (interactive)
  (let* ((wanted (or drive
		     (completing-read "Unmount: "
				      (append
				       (mapcar 'car am:mounted-devices)
				       (mapcar 'cdr am:mounted-devices))
				      nil
				      t)))
	 (mount-point (or drive
			  (car (assoc wanted am:mounted-devices))
			  (car (rassoc wanted am:mounted-devices)))))

    (run-hook-with-args 'am:pre-unmount-hook mount-point)
    ;; TODO: Close all buffers under mount-point
    (let* ((cmd-line (list "umount" mount-point))
	   (cmd-buf (get-buffer-create (format "Udisk unmounting %s" mount-point)))
	   (proc (apply 'start-process (car cmd-line)
			cmd-buf (car cmd-line) (cdr cmd-line))))
      (process-put proc :cmd-buf cmd-buf)
      (process-put proc :mount-point mount-point)
      (set-process-sentinel proc 'am:unmount-sentinel))))

(defun am:dbus-register ()
  ""
  (dbus-register-signal :session nil "/org/gtk/Private/RemoteVolumeMonitor"
			"org.gtk.Private.RemoteVolumeMonitor"
			"VolumeAdded" 'am:mount-signal-handler)

  (dbus-register-signal :session nil "/org/gtk/Private/RemoteVolumeMonitor"
			"org.gtk.Private.RemoteVolumeMonitor"
			"MountRemoved" 'am:unmount-signal-handler))

(provide 'auto-mounter)
