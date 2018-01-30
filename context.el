(require 'window-purpose)

(defvar navel--horizontal-windoww-layout
  '(t
    (0 0 100 67)
    (:purpose edit :purpose-dedicated t :width 1.00 :height 0.67
              :edges (0.00 0.00 1.00 0.67))
    (:purpose navel-context :purpose-dedicated t :width 1.00 :height 0.33
              :edges (0.00 0.67 1.00 1.00)))
  "Horizontal window layout for navel")

(defconst navel-function-definition-name "*Function Definition*")

(defvar navel--timer nil)
(defvar navel--context-map (make-sparse-keymap))

(defvar context-orig-pos nil)
(defvar context-base-pos nil)

(defvar edit-window-buffer-list nil)

(defvar navel--jumplist-list '())
(defvar navel--jumplist-idx 0)

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
        (let* ((temp-buf (get-buffer-create " *Navel Temp*"))
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
            (error nil))))))

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

(defun navel-display-context (position)
  (let ((context-same-buffer t)
        (new-buffer (navel-position-get-buffer position))
        (new-point (and (markerp position)
                        (marker-position position)))
        (context-base-buffer (navel-position-get-buffer context-base-pos)))
    (when new-buffer
      (unless (equal context-base-buffer new-buffer)
        (setq context-same-buffer nil)
        (when context-base-buffer
          (kill-buffer navel-function-definition-name)
          (unless (member context-base-buffer edit-window-buffer-list)
            (kill-buffer context-base-buffer)))
        (save-window-excursion
          (make-indirect-buffer new-buffer navel-function-definition-name t)))
      (save-selected-window
        (switch-to-buffer navel-function-definition-name t)
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
  (switch-to-buffer (navel-position-get-buffer context-base-pos))
  (goto-char context-base-pos)
  (recenter 6)
  (and (markerp context-orig-pos)
       (navel-jumplist-push-marker context-orig-pos))
  (navel-jumplist-push-marker (point-marker))
  (navel-edit-minor-mode t)
  (set (make-local-variable 'tool-bar-map) navel-tool-bar-map)
  (add-to-list 'edit-window-buffer-list (current-buffer)))

(defun navel-jumplist-push-marker (marker)
  (setq navel--jumplist-list
        (nthcdr navel--jumplist-idx navel--jumplist-list))
  (setq navel--jumplist-idx 0)
  (push marker navel--jumplist-list))

(defun navel-jumplist-jump-idx (idx)
  (let ((jumplist-curr (nth idx navel--jumplist-list)))
    (and (marker-buffer jumplist-curr)
         (switch-to-buffer (marker-buffer jumplist-curr))
         (goto-char jumplist-curr))))

(defun navel-jumplist-backward ()
  (interactive)
  (when (< navel--jumplist-idx (- (length navel--jumplist-list) 1))
    (setq navel--jumplist-idx (+ navel--jumplist-idx 1))
    (navel-jumplist-jump-idx navel--jumplist-idx)))

(defun navel-jumplist-forward ()
  (interactive)
  (when (> navel--jumplist-idx 0)
    (setq navel--jumplist-idx (- navel--jumplist-idx 1))
    (navel-jumplist-jump-idx navel--jumplist-idx)))

(defun navel-close-buffer (buffer)
  (setq edit-window-buffer-list
        (delq buffer edit-window-buffer-list))
  (or (equal buffer
             (navel-position-get-buffer context-base-pos))
      (kill-buffer buffer)))

(defun navel-tabbar-line-tab-hook (orig-fun tab)
  (concat (funcall orig-fun tab)
          (propertize
           (with-temp-buffer
             (insert (make-string 1 #x00D7))
             (insert " ")
             (buffer-string))
           'face (cond ((and (tabbar-selected-p tab (tabbar-current-tabset))
                             (tabbar-modified-p tab (tabbar-current-tabset)))
                        'tabbar-selected-modified)
                       ((tabbar-selected-p tab (tabbar-current-tabset))
                        'tabbar-selected)
                       ((tabbar-modified-p tab (tabbar-current-tabset))
                        'tabbar-modified)
                       (t 'tabbar-unselected))
           'pointer 'hand
           'tabbar-tab tab
           'local-map (tabbar-make-tab-keymap tab)
           'tabbar-action 'close-tab)))

(defun navel-tabbar-select-tab-callback-hook (event)
       (interactive "@e")
       (when (tabbar-click-p event)
         (let ((target (posn-string (event-start event))))
           (tabbar-click-on-tab
            (get-text-property (cdr target) 'tabbar-tab (car target))
            event
            (get-text-property (cdr target) 'tabbar-action (car target))))))

(defun navel-tabbar-click-on-tab-hook (tab &optional type action)
  (if (eq action 'close-tab)
      (navel-close-buffer (tabbar-tab-value tab))
    (when tabbar-select-tab-function
      (funcall tabbar-select-tab-function
               (tabbar-make-mouse-event type) tab)
      (tabbar-display-update))))

(defun navel-tabbar-init ()
  (require 'tabbar)
  (setq tabbar-buffer-list-function
        (lambda () edit-window-buffer-list))
  (setq tabbar-buffer-groups-function
        (lambda () (list (symbol-name (purpose-buffer-purpose (current-buffer))))))
  (setq tabbar-inhibit-functions
        (list (lambda () (not (equal (purpose-window-purpose (selected-window)) 'edit)))))

  (advice-add 'tabbar-line-tab :around #'navel-tabbar-line-tab-hook)
  (advice-add 'tabbar-select-tab-callback :override #'navel-tabbar-select-tab-callback-hook)
  (advice-add 'tabbar-click-on-tab :override #'navel-tabbar-click-on-tab-hook)

  (tabbar-mode))

(defun navel-context-init ()
  (interactive)
  (purpose-set-window-layout navel--horizontal-windoww-layout)

  (add-to-list 'purpose-user-name-purposes
               (cons navel-function-definition-name 'navel-context))
  (purpose-compile-user-configuration)

  (define-key navel--context-map (kbd "<mouse-1>") 'ignore)
  (define-key navel--context-map
    (kbd "<double-down-mouse-1>") 'navel-sync-context-to-edit)

  (add-to-list 'edit-window-buffer-list (current-buffer))
  (navel-tabbar-init))

;;;###autoload
(define-minor-mode navel-edit-minor-mode nil
  :group 'navel
  (cond
   ((not (equal (purpose-window-purpose (selected-window)) 'edit))
    (message "navel-edit-minor-mode can only be enabled in edit window")
    (setq navel-edit-minor-mode nil))
   (navel-edit-minor-mode
    (set (make-local-variable 'tool-bar-map) navel-tool-bar-map)
    (add-hook 'post-command-hook #'navel-schedule-timer))
   (t
    (kill-local-variable 'tool-bar-map)
    (remove-hook 'post-command-hook #'navel-schedule-timer))))
