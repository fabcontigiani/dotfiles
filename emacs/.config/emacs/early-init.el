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

;;; Maximize initial frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; UI
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
