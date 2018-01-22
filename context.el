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

(defvar context-base-buffer nil)

(defvar edit-window-buffer-list nil)

(defvar navel-get-symbol 'function-called-at-point)
(defvar navel-find-symbol (lambda (symb-name)
                            (let ((buffer-point
                                   (find-function-noselect symb-name t)))
                              (switch-to-buffer (car buffer-point) t)
                              (goto-char (cdr buffer-point)))))

(defcustom navel-idle-update-delay idle-update-delay
  "Idle time delay before automatically updating the context buffer."
  :group 'navel
  :type 'number
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (when navel--timer (navel-start-timer)))))

(defun navel-start-timer ()
  (interactive)
  (navel-stop-timer)
  (setq navel--timer
        (run-with-idle-timer navel-idle-update-delay t
                             #'navel-update)))

(defun navel-stop-timer ()
  (interactive)
  (when navel--timer
    (cancel-timer navel--timer)
    (setq navel--timer nil)))

(defun navel-update ()
  (let ((func-name (funcall navel-get-symbol)))
    (navel-display-symbol-context func-name)))

(defun navel-display-symbol-context (symb)
  (let ((buffer-point (condition-case nil
                          (save-excursion
                            (save-window-excursion
                             (funcall navel-find-symbol symb)
                             (cons (current-buffer) (point))))
                        (error nil))))
    (when buffer-point
      (unless (equal context-base-buffer (car buffer-point))
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
        (goto-char (cdr buffer-point))
        (recenter 0)))))

(defun navel-context-init ()
  (interactive)
  (purpose-set-window-layout navel--horizontal-windoww-layout)

  (add-to-list 'purpose-user-name-purposes
               (cons navel-function-definition-name 'navel-context))
  (purpose-compile-user-configuration)

  (add-to-list 'edit-window-buffer-list (current-buffer)))
