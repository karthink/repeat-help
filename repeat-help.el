;; Scratch buffer for: emacs-lisp-mode  -*- lexical-binding: t; -*-

(defcustom repeat-help-auto nil
  "Boolean to determine the auto-prompting behavior of repeat-help.

When true, repeat-help displays a prompts automatically when
repeating commands with `repeat-mode'.

When nil, the help prompt can be invoked by pressing the
`repeat-help-key', C-h by default. This is a Boolean."
  :group 'repeat-help
  :type 'boolean)

(defcustom repeat-help-key "C-h"
  "Keybinding to invoke the repeat-help prompt when repeating commands with `repeat-mode'.

This is a string suitable for input to `kbd'."
  :group 'repeat-help
  :type 'string)

(defcustom repeat-help-prompt-type 'embark
  "The backend for displaying the repeat-help prompt.

This is a symbol. The choices are embark, which-key or t. The
latter will fall back on the echo area message built into
`repeat-mode'."
  :group 'repeat-help
  :type '(choice
          (const :tag "Embark indicator" 'embark)
          (const :tag "Which Key indicator" 'which-key)
          (const :tag "Default indicator" t)))

(defvar repeat-help--echo-function #'ignore)

(defvar repeat-help-persist-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap scroll-other-window]
      (repeat-help--no-quit #'scroll-other-window))
    (define-key map [remap scroll-other-window-down]
      (repeat-help--no-quit #'scroll-other-window-down))
    (define-key map [remap recenter-top-bottom]
      (repeat-help--no-quit #'recenter-top-bottom '(4)))
    map))

(defun repeat-help--embark-activate ()
  "Allow displaying a help prompt for the active `repeat-map'.

The key to toggle the prompt (\"C-h\" by default) is customizable
via `repeat-help-key'."
  (when repeat-mode
    (if-let* ((rep-map-sym (or repeat-map
                               (repeat--command-property 'repeat-map)))
                (keymap (and (symbolp rep-map-sym)
                             (symbol-value rep-map-sym))))
        (set-transient-map
         (make-composed-keymap
          (list (let ((map (make-sparse-keymap)))
                  (define-key map (kbd repeat-help-key) (repeat-help--prompt-function))
                  map)
                repeat-help-persist-map
                keymap)))
      (funcall (repeat-help--abort-function)))))

(defun repeat-help--embark-activate-auto ()
  "Auto-activate a keymap indicator/popup for the active `repeat-map'."
  (if-let ((_ (or this-command real-this-command))
           (keymap (or repeat-map
                       (repeat--command-property 'repeat-map))))
      (run-at-time 0 nil (repeat-help--autoprompt-function) keymap)
    (funcall (repeat-help--abort-function))))

(defun repeat-help--no-quit (cmd &optional prefix)
  "Return a function to Call CMD without breaking the repeat state.

Optional PREFIX is supplied as the prefix arg to CMD."
  (lambda (arg)
    (interactive "p")
    (setq this-command last-command)
    (let ((current-prefix-arg (or prefix arg)))
      (call-interactively cmd))))

(define-minor-mode repeat-help-mode
  "Repeat Help Mode enables a prompt with available keys and actions
when repeating commands using `repeat-mode'.

The prompt can be displayed using Embark (default) or a Which Key
menu. If neither package is available, it falls back on the echo
area message built into `repeat-mode'."
  :global t
  :lighter nil
  :keymap nil
  (if repeat-help-mode
      (progn 
        (setq repeat-help--echo-function repeat-echo-function)
        (advice-add 'repeat-post-hook :after
                    (if repeat-help-auto
                        #'repeat-help--embark-activate-auto
                      #'repeat-help--embark-activate)))
    (setq repeat-echo-function repeat-help--echo-function)
    (advice-remove 'repeat-post-hook #'repeat-help--embark-activate-auto)
    (advice-remove 'repeat-post-hook #'repeat-help--embark-activate)))

;; Choose between Embark and Which Key when dispatching
;;; Manual activation
(defsubst repeat-help--prompt-function ()
  (pcase repeat-help-prompt-type
    ('embark #'repeat-help-embark-dispatch)
    ('which-key #'repeat-help-which-key-dispatch)
    (_ #'repeat-echo-message)))

;;; Auto activation
(defsubst repeat-help--autoprompt-function ()
  (pcase repeat-help-prompt-type
    ('embark #'repeat-help--embark-indicate)
    ('which-key #'repeat-help--which-key-popup)
    (_ #'repeat-echo-message)))

(defsubst repeat-help--abort-function ()
  (pcase repeat-help-prompt-type
    ('embark #'repeat-help--embark-abort)
    ('which-key #'which-key--hide-popup)
    (_ (lambda () (repeat-echo-message nil)))))

;; Which-key specific code
(defun repeat-help-which-key-dispatch (keymap)
  "Toggle the Which Key popup for keymap."
  (interactive (list (or repeat-map
                         (let ((this-command last-command))
                           (repeat--command-property 'repeat-map)))))
  (setq this-command last-command)
  (if (which-key--popup-showing-p)
      (which-key--hide-popup)
    (repeat-help--which-key-popup keymap)))

(defsubst repeat-help--which-key-popup (keymap)
  "Display a Which Key popup for keymap."
  (which-key--create-buffer-and-show
   nil (symbol-value keymap)))

;; Embark-specific code
(defun repeat-help-embark-dispatch (keymap)
  "Toggle the Embark verbose key indicator for keymap."
  (interactive (list (or repeat-map
                         (let ((this-command last-command))
                           (repeat--command-property 'repeat-map)))))
  (setq this-command last-command)
  (if-let ((win (get-buffer-window
                 "*Repeat Commands*" 'visible)))
      (quit-window nil win)
    (repeat-help--embark-indicate keymap)))

(defun repeat-help--embark-indicate (keymap)
  "Display an Embark verbose key indicator for keymap."
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

(defun repeat-help--embark-abort ()
  (when-let ((win
              (get-buffer-window
               "*Repeat Commands*" 'visible)))
    (quit-window 'kill-buffer win)))
