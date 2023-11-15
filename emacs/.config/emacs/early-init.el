;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(defconst user/gc-cons-threshold (* 32 1024 1024) "Preferred garbage collection threshold value (32MB).")
(defconst user/gc-cons-percentage 0.1 "Preferred garbage collection percentage value (10%).")

(defun user/defer-garbage-collection ()
      "Defer garbage collection by maximizing the collection threshold."
      (setq gc-cons-threshold most-positive-fixnum
            gc-cons-percentage 1.0))

(defun user/restore-garbage-collection ()
      "Restore the garbage collection threshold parameters in a deferred fashion."
      (setq gc-cons-threshold user/gc-cons-threshold
            gc-cons-percentage user/gc-cons-percentage))

;; Defer garbage collection until after initialization
(user/defer-garbage-collection)
(add-hook 'emacs-startup-hook #'user/restore-garbage-collection)

;; package.el initialization is handled manually in init.el
(setq package-enable-at-startup nil)


(provide 'early-init)
;;; early-init.el ends here
