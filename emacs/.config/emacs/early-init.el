;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(defconst fab/gc-cons-threshold (* 32 1024 1024) "Preferred garbage collection threshold value (32MB).")
(defconst fab/gc-cons-percentage 0.1 "Preferred garbage collection percentage value (10%).")

(defun fab/defer-garbage-collection ()
      "Defer garbage collection by maximizing the collection threshold."
      (setq gc-cons-threshold most-positive-fixnum
            gc-cons-percentage 1.0))

(defun fab/restore-garbage-collection ()
      "Restore the garbage collection threshold parameters in a deferred fashion."
      (setq gc-cons-threshold fab/gc-cons-threshold
            gc-cons-percentage fab/gc-cons-percentage))

;; Defer garbage collection until after initialization
(fab/defer-garbage-collection)
(add-hook 'emacs-startup-hook #'fab/restore-garbage-collection)

;; package.el initialization is handled manually in init.el
(setq package-enable-at-startup nil)


(provide 'early-init)
;;; early-init.el ends here
