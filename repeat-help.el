;; Scratch buffer for: emacs-lisp-mode  -*- lexical-binding: t; -*-

(setq repeat-echo-function #'ignore)

(advice-add 'repeat-post-hook :after
            #'repeat-help--embark-activate-auto)

(advice-remove 'repeat-post-hook
            #'repeat-help--embark-activate-auto)

(advice-add 'repeat-post-hook :after #'repeat-help--embark-activate)
(advice-remove 'repeat-post-hook 'repeat-help--embark-activate)

;; Which-key indicator support for repeat-mode


;; Embark indicator support for repeat-mode
(defun repeat-help--embark-activate ()
  (when repeat-mode
    (if-let* ((rep-map-sym (or repeat-map
                               (repeat--command-property 'repeat-map)))
                (keymap (and (symbolp rep-map-sym)
                             (symbol-value rep-map-sym))))
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map keymap)
           (define-key map (kbd "C-h") (repeat-help-embark-dispatch rep-map-sym))
           (define-key map [remap scroll-other-window] (repeat-help--no-quit #'scroll-other-window))
           (define-key map [remap scroll-other-window-down] (repeat-help--no-quit #'scroll-other-window-down))
           (define-key map [remap recenter-top-bottom] (repeat-help--no-quit #'recenter-top-bottom '(4)))
           map))
      (when-let ((win (get-buffer-window
                       "*Repeat Commands*" 'visible)))
        (quit-window 'kill-buffer win)))))

(defun repeat-help--embark-activate-auto ()
  (if-let ((_ (or this-command real-this-command))
           (keymap (or repeat-map
                       (repeat--command-property 'repeat-map))))
      (run-at-time 0 nil #'repeat-help--embark-indicate keymap)
    (when-let ((win
                (get-buffer-window
                 "*Repeat Commands*" 'visible)))
      (quit-window 'kill-buffer win))))

(defun repeat-help--embark-indicate (keymap)
  (let* ((bufname "*Repeat Commands*")
         (embark-verbose-indicator-buffer-sections
         '(bindings))
        (embark--verbose-indicator-buffer bufname)
        (embark-verbose-indicator-display-action
         '(display-buffer-at-bottom
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)))
           (body-function . (lambda (win)
                              (with-selected-window win
                               (setq-local mode-line-format nil)))))))
    (funcall
     (embark-verbose-indicator)
     (symbol-value keymap))
    (setq other-window-scroll-buffer (get-buffer bufname))))

(defun repeat-help--no-quit (cmd &optional prefix)
  (lambda (arg)
    (interactive "p")
    (setq this-command last-command)
    (let ((current-prefix-arg (or prefix arg)))
      (call-interactively cmd))))

(defun repeat-help-embark-dispatch (keymap)
  (lambda ()
    (interactive)
    (setq this-command last-command)
    (if-let ((win (get-buffer-window
                   "*Repeat Commands*" 'visible)))
        (quit-window nil win)
      (repeat-help--embark-indicate keymap))))

