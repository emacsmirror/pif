;;; pif.el --- Prevent Initial Flash of Light -*- lexical-binding: t -*-

;;; Code:

(defgroup pif-group ()
  "Prevent Initial Flash of Light"
  :group 'convenience)

;;;; User options

(defcustom pif-fallback-light-color "#ff0000" ; "#efe9dd"
  "Fallback color used in light mode if no color was saved."
  :type 'color
  :group 'pif-group)

(defcustom pif-fallback-dark-color "#00ff00" ; "#1d2235"
  "Fallback color used in dark mode if no color was saved."
  :type 'color
  :group 'pif-group)

(defcustom pif-hide-ui-elements t
  ""
  :type 'boolean
  :group 'pif-group)

;;;; Constants

(defconst pif-file-name ".pif.el"
  "Filename for storing PIF-related data within `user-emacs-directory`.")

;;;; Functions

(defun pif-update (appearance)
  "Save or update the colors, and the size, and position of the initial
frame.

APPEARANCE specifies whether to save the colors for 'light or 'dark
mode."
  (let* ((initial-frame (car (visible-frame-list))))
    (pif--update-state 'colors appearance (frame-parameter initial-frame 'background-color))
    (pif--update-state 'frame 'width      (frame-parameter initial-frame 'width))
    (pif--update-state 'frame 'height     (frame-parameter initial-frame 'height))
    (pif--update-state 'frame 'left       (frame-parameter initial-frame 'left))
    (pif--update-state 'frame 'top        (frame-parameter initial-frame 'top))))

(defun pif-early ()
  ""
  (pif--hide-ui-elements)
  (pif--prepare-frame))

(defun pif--hide-ui-elements ()
  (setq cursor-type nil)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq mode-line-format nil))

(defun pif--prepare-frame ()
  (let ((saved-state (alist-get 'frame (pif--read-state))))
    (when saved-state
      (setq default-frame-alist
            (append `(
                      (width                   . ,(+ 2 (alist-get 'width  saved-state)))
                      (height                  . ,(+ 1 (alist-get 'height saved-state)))
                      (userposition            . t)
                      (left                    . ,(alist-get 'left saved-state))
                      (top                     . ,(alist-get 'top  saved-state))
                      (ns-transparent-titlebar . t))
                    ())))))

(defun pif (appearance)
  "Configure the colors, and the size, and position of the initial frame to
prevent the 'Initial Flash of Light'.

APPEARANCE specifies whether to load the colors for 'light or 'dark
mode."
  (pif--set-colors appearance))

(defun pif-reset ()
  "Reset everything to the values that where active before `pif` was called."
  (pif--reset-colors))

(defun pif--set-colors (appearance)
  ;; FIXME: This should really be one step later. Maybe we have 'dark
  ;; saved but need 'light right now. We should still be able to
  ;; fall-back, then
  (let* ((saved-state (alist-get 'colors (pif--read-state)))
         (bg (alist-get appearance saved-state
                        (pcase appearance
                          ('light pif-fallback-light-color)
                          ('dark  pif-fallback-dark-color)))))
    (when bg
      ;; default
      (pif--update-state 'colors 'default-background (intern (face-attribute 'default :background)))
      (pif--update-state 'colors 'default-foreground (intern (face-attribute 'default :foreground)))
      (set-face-attribute 'default nil :background bg :foreground bg)
      ;; fringe
      (pif--update-state 'colors 'fringe-background (face-attribute 'fringe :background))
      (pif--update-state 'colors 'fringe-foreground (face-attribute 'fringe :foreground))
      (set-face-attribute 'fringe nil :background bg :foreground bg))))

(defun pif--set-face-attribute (face attribute value)
  (when value
    (set-face-attribute face nil attribute
                        (if (member value '(unspecified-bg unspecified-fg))
                            'unspecified
                          value))))

(defun pif--reset-colors ()
  (let ((saved-state (alist-get 'colors (pif--read-state))))
    (when saved-state
      (let ((orig-default-background (alist-get 'default-background saved-state))
            (orig-default-foreground (alist-get 'default-foreground saved-state))
            (orig-fringe-background  (alist-get 'fringe-background  saved-state))
            (orig-fringe-foreground  (alist-get 'fringe-foreground  saved-state)))
        (pif--set-face-attribute 'default :background orig-default-background)
        (pif--set-face-attribute 'default :foreground orig-default-foreground)
        (pif--set-face-attribute 'fringe  :background orig-fringe-background)
        (pif--set-face-attribute 'fringe  :foreground orig-fringe-foreground)))))

(defun pif--read-state ()
  (let ((pif-file (expand-file-name pif-file-name user-emacs-directory)))
    (if (file-exists-p pif-file)
        (with-temp-buffer
          (insert-file-contents pif-file)
          (condition-case nil
              (read (current-buffer))
            (error nil)))
      nil)))

(defun pif--write-state (state)
  (with-temp-file (expand-file-name pif-file-name user-emacs-directory)
    (insert (prin1-to-string state))))

(defun pif--update-state (key subkey value)
  (let* ((state (pif--read-state))
         (existing-sublist (alist-get key state))
         (updated-sublist (assoc-delete-all subkey existing-sublist)))
    (setq updated-sublist (cons (cons subkey value) updated-sublist))
    (setq state (assoc-delete-all key state))
    (add-to-list 'state (cons key updated-sublist))
    (pif--write-state state)))

(provide 'pif)
