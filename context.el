(require 'cl)

(defvar navel--timer nil)
(defvar navel--context-map (make-sparse-keymap))

(defvar context-orig-pos nil)
(defvar context-base-pos nil)

(defvar navel--jumplist-list-prev '())
(defvar navel--jumplist-list-next '())

(defvar navel-get-context 'navel-elisp-get-func)

(defvar navel-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "left-arrow" 'navel-jumplist-backward 'Backward map
                         :vert-only t)
    (tool-bar-local-item "right-arrow" 'navel-jumplist-forward 'Forward map
                         :vert-only t)
    (tool-bar-local-item "prev-node" 'spacemacs/enter-ahs-backward 'PrevSymb map
                         :vert-only t)
    (tool-bar-local-item "next-node" 'spacemacs/enter-ahs-forward 'NextSymb map
                         :vert-only t)
    map))

(defun navel-elisp-get-func ()
  (let* ((recentf-exclude '((lambda (f) t)))
         (func-symb (elisp--current-symbol)))
    (or (condition-case nil
            (save-excursion
              (let ((buffer-point (find-function-noselect func-symb t)))
                (set-marker (point-marker) (cdr buffer-point) (car buffer-point))))
          (error nil))
        (let* ((temp-buf (get-buffer-create (concat "doc:" (symbol-name func-symb))))
               (standard-output temp-buf))
          (condition-case nil
              (funcall #'(lambda (help-str)
                           (with-current-buffer temp-buf
                             (erase-buffer)
                             (insert help-str)
                             (goto-char 0)
                             (current-buffer)))
                       (with-output-to-string
                         (prin1 func-symb)
                         (princ " is ")
                         (describe-function-1 func-symb)))
            (error (kill-buffer temp-buf)))))))

(defcustom navel-idle-update-delay idle-update-delay
  "Idle time delay before automatically updating the context buffer."
  :group 'navel
  :type 'number
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (when navel--timer (navel-start-timer)))))

(defun navel-schedule-timer ()
  (or (and navel--timer
           (memq navel--timer timer-idle-list))
      (setq navel--timer
            (run-with-idle-timer navel-idle-update-delay nil
                                 #'navel-update))))

(defun navel-update ()
  (and navel-edit-minor-mode
       (eldoc--message-command-p last-command)
       (navel-display-context (funcall navel-get-context))
       (setq context-orig-pos (point-marker))))

(defun navel-position-get-buffer (position)
  (cond ((markerp position) (marker-buffer position))
        ((bufferp position) position)
        (t nil)))

(defun navel-buffer-in-edit (buf)
  (let* ((tabset-alist-elf
          (assq (window-number (navel-window-get 'edit))
                tabbar-window-alist))
         (buflist (cdr tabset-alist-elf)))
    (assq buf buflist)))

(defun navel-display-context (position)
  (let* ((context-same-buffer t)
         (new-buffer (navel-position-get-buffer position))
         (new-buffer-name (concat " " (buffer-name new-buffer)))
         (new-point (and (markerp position)
                         (marker-position position)))
         (context-window (navel-window-get 'context))
         (context-base-buffer (navel-position-get-buffer context-base-pos)))
    (when new-buffer
      (unless (equal context-base-buffer new-buffer)
        (setq context-same-buffer nil)
        (when context-base-buffer
          (kill-buffer (window-buffer context-window))
          (unless (or (navel-buffer-in-edit context-base-buffer)
                      (get-buffer-window-list context-base-buffer nil t))
            (kill-buffer context-base-buffer)))
        (save-window-excursion
          (make-indirect-buffer new-buffer new-buffer-name t)))
      (with-selected-window context-window
        (switch-to-buffer new-buffer-name t t)
        (setq header-line-format nil)
        (when new-point
          (use-local-map navel--context-map)
          (unless (and context-same-buffer
                       (equal context-base-pos position))
            (goto-char new-point)
            (recenter 2)))
        (setq context-base-pos position))
      t)))

(defun navel-sync-context-to-edit ()
  (interactive)
  (select-window (navel-window-get 'edit))
  (switch-to-buffer (navel-position-get-buffer context-base-pos) nil t)
  (goto-char context-base-pos)
  (recenter 6)
  (and (markerp context-orig-pos)
       (navel-jumplist-push-marker context-orig-pos))
  (navel-jumplist-push-marker (point-marker))
  (navel-edit-buffer-init)
  (set (make-local-variable 'tool-bar-map) navel-tool-bar-map))

(defun navel-jumplist-push-marker (marker)
  (setq navel--jumplist-list-next '())
  (push marker navel--jumplist-list-prev))

(defun navel-jumplist-jump-marker (marker)
  (and (marker-buffer marker)
       (switch-to-buffer (marker-buffer marker))
       (goto-char marker)
       (recenter 6)))

(defun navel-jumplist-backward ()
  (interactive)
  (setq navel--jumplist-list-prev
        (cl-remove-if-not #'marker-buffer navel--jumplist-list-prev))
  (cl-block nil
    (while (> (length navel--jumplist-list-prev) 0)
      (let ((prevm (pop navel--jumplist-list-prev)))
        (push prevm navel--jumplist-list-next)
        (unless (or (not (navel-buffer-in-edit (marker-buffer prevm)))
                    (and (equal (marker-buffer prevm) (current-buffer))
                         (< (count-lines (point-marker) prevm) 10)))
          (navel-jumplist-jump-marker prevm)
          (return t))))))

(defun navel-jumplist-forward ()
  (interactive)
  (setq navel--jumplist-list-next
        (cl-remove-if-not #'marker-buffer navel--jumplist-list-next))
  (cl-block nil
    (while (> (length navel--jumplist-list-next) 0)
      (let ((nextm (pop navel--jumplist-list-next)))
        (push nextm navel--jumplist-list-prev)
        (unless (or (not (navel-buffer-in-edit (marker-buffer nextm)))
                    (and (equal (marker-buffer nextm) (current-buffer))
                         (< (count-lines (point-marker) nextm) 10)))
          (navel-jumplist-jump-marker nextm)
          (return t))))))

(defun navel-window-get (type &optional frame)
  (car (cl-remove-if-not #'(lambda (window)
                             (eql type (window-parameter window 'navel)))
                         (window-list frame))))

(defun navel-disable-persp-frame (make-frame-func &rest args)
  (let ((persp-init-new-frame-behaviour-override #'(lambda (&rest args) nil)))
    (apply make-frame-func args)))

(defun navel-tabbar-init ()
  (require 'aquamacs-tabbar)
  (advice-add 'helm--generic-read-file-name
              :before-until
              #'(lambda (&rest args)
                  (when (next-read-file-uses-dialog-p)
                    (apply 'read-file-name-default args))))
  (advice-add #'tabbar-new-frame-with-clicked-buffer
              :around #'navel-disable-persp-frame)
  (advice-add #'tabbar-move-clicked-buffer-to-new-frame
              :around #'navel-disable-persp-frame)
  (fset 'ns-frame-is-on-active-space-p #'(lambda (frame) t))
  (fset 'old-delete-window 'delete-window)
  (fset 'frame-iconified-p 'frame-live-p)
  (tabbar-init-tabsets-store)
  (remove-hook 'first-change-hook 'tabbar-window-update-tabsets-when-idle))

(defun navel-edit-buffer-init ()
  (navel-edit-minor-mode t)
  (setq header-line-format tabbar-header-line-format))

(defun navel-layout-init ()
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil ( / ( * (window-height) 3) 4))))
    (set-window-parameter win0 'navel 'edit)
    (set-window-parameter win1 'navel 'context)
    (with-selected-window win0
      (navel-edit-buffer-init)
      (navel-display-context (point-marker)))))

(defun navel-context-init ()
  (interactive)
  (navel-tabbar-init)

  (define-key navel--context-map (kbd "<mouse-1>") 'ignore)
  (define-key navel--context-map
    (kbd "<double-down-mouse-1>") 'navel-sync-context-to-edit)

  (navel-layout-init))

;;;###autoload
(define-minor-mode navel-edit-minor-mode nil
  :group 'navel
  (cond
   ((not (equal (window-parameter (selected-window) 'navel) 'edit))
    (message "navel-edit-minor-mode can only be enabled in edit window")
    (setq navel-edit-minor-mode nil))
   (navel-edit-minor-mode
    (set (make-local-variable 'tool-bar-map) navel-tool-bar-map)
    (add-hook 'post-command-hook #'navel-schedule-timer))
   (t
    (kill-local-variable 'tool-bar-map)
    (remove-hook 'post-command-hook #'navel-schedule-timer))))
