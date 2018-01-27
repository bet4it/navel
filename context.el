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

(defvar context-base-buffer nil)
(defvar context-base-point nil)

(defvar edit-window-buffer-list nil)

(defvar navel-get-context 'navel-elisp-get-func)

(defvar navel-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "left-arrow" 'evil-jump-backward 'Backward map
                         :vert-only t)
    (tool-bar-local-item "right-arrow" 'evil-jump-forward 'Forward map
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
              (find-function-noselect func-symb t))
          (error nil))
        (let* ((temp-buf (get-buffer-create " *Navel Temp*"))
               (standard-output temp-buf))
          (condition-case nil
              (funcall #'(lambda (help-str)
                           (with-current-buffer temp-buf
                             (erase-buffer)
                             (insert help-str)
                             (goto-char 0)
                             (cons (current-buffer) nil)))
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
       (navel-display-context (funcall navel-get-context))))

(defun navel-display-context (buffer-point)
  (let ((context-same-buffer t))
    (when buffer-point
      (unless (equal context-base-buffer (car buffer-point))
        (setq context-same-buffer nil)
        (when context-base-buffer
          (kill-buffer navel-function-definition-name)
          (unless (member context-base-buffer edit-window-buffer-list)
            (kill-buffer context-base-buffer)
            (setq context-base-buffer nil)))
        (save-window-excursion
          (setq context-base-buffer (car buffer-point))
          (make-indirect-buffer context-base-buffer navel-function-definition-name t)))
      (save-selected-window
        (switch-to-buffer navel-function-definition-name t)
        (when (cdr buffer-point)
          (use-local-map navel--context-map)
          (unless (and context-same-buffer
                       (equal context-base-point (cdr buffer-point)))
            (goto-char (cdr buffer-point))
            (recenter 2)))
        (setq context-base-point (cdr buffer-point))))))

(defun navel-sync-context-to-edit ()
  (interactive)
  (switch-to-buffer context-base-buffer)
  (goto-char context-base-point)
  (recenter 6)
  (navel-edit-minor-mode t)
  (set (make-local-variable 'tool-bar-map) navel-tool-bar-map)
  (add-to-list 'edit-window-buffer-list (current-buffer)))

(defun navel-context-init ()
  (interactive)
  (purpose-set-window-layout navel--horizontal-windoww-layout)

  (add-to-list 'purpose-user-name-purposes
               (cons navel-function-definition-name 'navel-context))
  (purpose-compile-user-configuration)

  (define-key navel--context-map
    (kbd "<double-down-mouse-1>") 'navel-sync-context-to-edit)

  (add-to-list 'edit-window-buffer-list (current-buffer)))

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
