;;; magit-view-file.el --- View git file through history

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-07-18
;; Last changed: 2014-03-26 09:00:26
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(require 'magit)
(provide 'magit-view-file)


(defvar magit-view-file-log-map
  (let ((map (make-sparse-keymap "Magit:VFLog")))
    (define-key map (kbd "RET") 'magit-view-file-at-commit)
    (define-key map (kbd "c") 'magit-view-file-show-commit)
    (define-key map (kbd "v") 'magit-view-file-at-commit)
    (define-key map (kbd "q") 'magit-view-file-log-mode)
    map)
  "Keymap for an annotated section.\\{magit-view-file-map}")


(defun magit-view-file-parse-log(file)
  "Get log for FILE. Return a list suitable to be displayed in
file history buffer."
  (with-temp-buffer
    (magit-git-insert
     "log" "--pretty=format:%H%x00%at%x00%an%x00%s" file)
    (loop for l in (split-string
                    (buffer-substring-no-properties
                     (point-min) (point-max)) "\n")
          collect (split-string l (char-to-string 0)))))

;;;###autoload
(defun magit-view-file-history ()
  "Show history of current file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (file (magit-buffer-file-name t))
         (lines (magit-view-file-parse-log (file-name-nondirectory file)))
         (blank " "))
    (with-current-buffer
        (get-buffer-create (format "HISTORY:%s" file-name))
      (setq buffer-read-only nil)
      (erase-buffer)
      (loop for l in lines
            for sha1 = (nth 0 l)
            for date = (format-time-string "%Y-%m-%d %H:%M:%S %z"
                                           (seconds-to-time
                                            (string-to-number (nth 1 l))))
            for author = (nth 2 l)
            for subject = (nth 3 l)
            do (let ((log (concat
                           (propertize (substring sha1 0 magit-sha1-abbrev-length)
                                       'face 'magit-log-sha1)
                           blank
                           (propertize date
                                       'face 'magit-log-message)
                           blank
                           (propertize (format "%-20s" author)
                                       'face 'magit-branch)
                           blank
                           (propertize subject)))
                     (pos (point))
                     ov)
                 (insert log "\n")
                 (setq ov (make-overlay pos (point)))
                 (overlay-put ov :sha1 sha1)
                 (overlay-put ov :file file)
                 (overlay-put ov :file-name file-name)
                 ))
      (delete-char -1 nil)
      (setq buffer-read-only t)
      (magit-view-file-log-mode 1)
      (goto-char (point-min))
      (switch-to-buffer-other-window (current-buffer)))))

                          

(defun magit-view-file-get-properties ()
  "Get overlay properties for file at point in file history
buffer."
  (save-excursion
    (goto-char (point-at-bol))
    (loop for ov in (overlays-at (point))
          for sha1 =  (overlay-get ov :sha1)
          when sha1
          return (overlay-properties ov))))


(defun magit-view-file-show-commit ()
  "Show commit at point in file history mode."
  (interactive)
  (let* ((values (magit-view-file-get-properties))
         (sha1 (plist-get values :sha1)))
    (magit-show-commit sha1)))

(defun magit-view-file-at-commit ()
  "View FILE at COMMIT in file history buffer."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (let* ((values (magit-view-file-get-properties))
           (mode (with-current-buffer
                     (find-buffer-visiting (plist-get values :file-name))
                   major-mode))
           (default-directory (file-name-directory (plist-get values :file-name)))
           (new-buffer-name (format "%s:%s"
                                     (substring (plist-get values :sha1) 0 magit-sha1-abbrev-length)
                                    (plist-get values :file))))
      
      (with-current-buffer (generate-new-buffer new-buffer-name)
        (setq buffer-read-only nil)
        (magit-git-insert "show" new-buffer-name)
        (set-buffer-modified-p nil)
        (funcall mode)
        (view-buffer-other-window (current-buffer) nil 'kill-buffer)))))


(define-minor-mode magit-view-file-log-mode
  "Display file log information inline."
  :keymap magit-view-file-log-map
  :lighter " Magit:VFLog"

  (if magit-view-file-log-mode
      (progn
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))
    (set-buffer-modified-p nil)
    (kill-buffer)))

                                  
    
         
    
    

;; magit-view-file.el ends here
