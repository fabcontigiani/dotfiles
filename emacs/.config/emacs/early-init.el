;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Disable package.el
(setq package-enable-at-startup nil)

;;; Defer garbage collection at startup
(defconst fab/gc-cons-threshold (* 32 1024 1024)
  "Preferred garbage collection threshold value (32MB).")
(defconst fab/gc-cons-percentage 0.1
  "Preferred garbage collection percentage value (10%).")

(defun fab/defer-garbage-collection ()
  "Defer garbage collection by maximizing the collection threshold."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0))

(defun fab/restore-garbage-collection ()
  "Restore the garbage collection threshold parameters in a deferred fashion."
  (setq gc-cons-threshold fab/gc-cons-threshold
        gc-cons-percentage fab/gc-cons-percentage))

(fab/defer-garbage-collection)
(add-hook 'elpaca-after-init-hook #'fab/restore-garbage-collection)

;;; Maximize frame by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UI
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)

;; Bonus keys (GUI only)
;; Credit: https://emacsnotes.wordpress.com/2022/09/11/three-bonus-keys-c-i-c-m-and-c-for-your-gui-emacs-all-with-zero-headache/
(add-hook
 'after-make-frame-functions
 (defun fab/setup-bonus-keys (frame)
   "Reclaim keys for GUI Emacs.

- When you type `Ctrl-i', Emacs sees it as `Bonus-i', and NOT as 'Tab'
- When you type `Ctrl-m', Emacs sees it as `Bonus-m', and NOT as 'Return'
- When you type `Ctrl-[', Emacs sees it as `Bonus-lsb', and not as 'Esc'

That is,

- `Ctrl-i' and 'Tab' keys are different
- `Ctrl-m' and 'Return' keys are different
- `Ctrl-[' and 'Esc' keys are different"
   (with-selected-frame frame
     (when (display-graphic-p) ; don't remove this condition, if you want
                                        ; terminal Emacs to be usable
       (define-key input-decode-map (kbd "C-i") [Bonus-i])
       (define-key input-decode-map (kbd "C-[") [Bonus-lsb]) ; left square bracket
       (define-key input-decode-map (kbd "C-m") [Bonus-m])))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
