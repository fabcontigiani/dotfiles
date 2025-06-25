;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
;;;; Bootstrap elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;;;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t
        use-package-compute-statistics t
        use-package-expand-minimally t))

;;;;; Block until current queue processed.
(elpaca-wait)

;;;; Better Defaults
(use-package emacs
  :ensure nil
  :init
  ;; Store automatic customization options elsewhere
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))
  
  ;; User variables
  (defvar user-email-address "fabcontigiani@gmail.com")
  (defvar fab/org-directory (expand-file-name "~/Nextcloud/org/"))
  (defvar fab/bibliography-dir (concat fab/org-directory "biblio/"))
  (defvar fab/bibliography-file (concat fab/bibliography-dir "references.bib"))
  
  :custom
  (user-full-name "Fabrizio Contigiani")

  ;; Elisp compilation warnings
  (native-comp-async-report-warnings-errors 'silent "Don't report errors from async native compilation")
  (byte-compile-warnings '(not lexical free-vars noruntime unresolved docstrings))

  ;; General configuration
  (use-short-answers t "Use y-or-n prompts by default")
  (truncate-lines t "Truncate lines instead of wrapping")
  (kill-whole-line t "Include newline character when killing a line.")
  (context-menu-mode t "Enable global context menu support")
  (message-truncate-lines nil "Don't truncate messages in the echo area")
  (cursor-in-non-selected-windows nil "Hide cursor in inactive windows")
  (ring-bell-function 'ignore "Disable terminal bell")
  (max-mini-window-height 10 "Limit minibuffer height to 10 lines")
  (enable-recursive-minibuffers t "Allow minibuffer commands to be called in the minibuffer")
  (use-dialog-box nil "Don't pop up UI dialogs when prompting")
  (load-prefer-newer t "Load from source files if they are newer than bytecode files")
  (read-extended-command-predicate #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode.")
  (large-file-warning-threshold (* 100 (expt 2 20)) "Warn about 100MB+ files")

  ;; Startup
  (initial-scratch-message "" "Leave scratch buffer empty on startup")
  (initial-major-mode 'fundamental-mode "Set initial mode to fundamental-mode on startup")
  (inhibit-startup-screen t "Do not create or show the initial splash screen")
  (inhibit-default-init t "Do not attempt to load any OS-provided init files")

  ;; Default style rules
  (sentence-end-double-space nil "Do not use double spacing between sentences in paragraphs.")
  (require-final-newline t "Require a terminating newline at the end of every file.")
  (indent-tabs-mode nil "Use spaces for indentation")
  (tab-width 4 "Use 4 spaces for indentation")
  (fill-column 80 "Set default line-wrap column to column 80")

  ;; Performance tweaks
  (inhibit-compacting-font-caches t)
  (jit-lock-defer-time 0 "Defer fontification while input is pending")
  (auto-mode-case-fold nil "Disable case-insensitive second pass over `auto-mode-alist'")
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t "Revert Dired and other buffers")
  (tab-always-indent 'complete "Enable indentation+completion using the TAB key")
  (tab-first-completion 'word-or-paren-or-punct)
  ;; (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

  ;; Fix S-SPC on pgtk
  ;; (pgtk-use-im-context-on-new-connection nil)

  :config
  ;; Convenience
  (delete-selection-mode 1) ;; Save a keystroke
  (electric-pair-mode t) ;; Tidy parenthesis
  (save-place-mode t) ;; Remember and restore the last cursor location of opened files
  (savehist-mode t) ;; Save what you enter into minibuffer prompts
  (recentf-mode t) ;; Keep track of recently opened files
  (global-auto-revert-mode 1) ;; Revert buffers when the underlying file has changed

  ;; Font configuration
  (set-face-attribute 'default nil :family "Iosevka" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 1.0)
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab" :height 1.0)

  ;; Make C-g a bit more helpful, credit to Prot:
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration
  (defun fab/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  (keymap-global-unset "C-z")
  (keymap-global-unset "C-x C-z")
  (keymap-global-set "<Bonus-lsb>" #'previous-buffer)
  (keymap-global-set "C-]" #'next-buffer)
  :bind
  (("C-x k" . #'kill-current-buffer)
   ("C-g" . #'fab/keyboard-quit-dwim)))

(use-package shell
  :ensure nil
  :defer t
  :custom
  (explicit-shell-file-name "/bin/bash")
  (comint-prompt-read-only t)
  (shell-kill-buffer-on-exit t))

(use-package eshell
  :ensure nil
  :hook
  (eshell-mode . fab/setup-eshell-outline-regexp)
  (eshell-mode . completion-preview-mode)
  :init
  (defun fab/setup-eshell-outline-regexp () ""
         (setq-local outline-regexp eshell-prompt-regexp))
  :config
  (ffap-bindings)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  :custom
  (eshell-kill-processes-on-exit t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-pushd-dunique t))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-prompt-function 'epe-theme-multiline-with-status)
  (epe-path-style 'full)
  (epe-show-git-status-extended t))

(use-package bash-completion
  :init
  (defun fab/setup-eshell-bash-completion () ""
         (add-hook 'completion-at-point-functions
                   #'bash-completion-capf-nonexclusive nil t))
  :hook
  (eshell-mode . fab/setup-eshell-bash-completion)
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

(use-package eat
  :after eshell
  :config
  (eat-eshell-mode t)
  (eat-eshell-visual-command-mode t)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-shell-prompt-annotation-success-margin-indicator ""))

(use-package isearch
  :ensure nil
  :defer t
  :custom
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  (search-whitespace-regexp ".?*"))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  :init
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package dired
  :ensure nil
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-alFh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-subtree
  :bind (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :bind
  (("C-c t" . dired-sidebar-toggle-sidebar))
  :custom
  (dired-sidebar-theme 'nerd-icons))

(use-package trashed
  :commands (trashed)
  :custom
  (delete-by-moving-to-trash t)
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package on ;; Additional hooks for faster startup
  :defer t)

;;;; Org-mode
(use-package org
  :defer
  :ensure `(org :repo "https://code.tecosaur.net/tec/org-mode.git/"
		:branch "dev")
  :hook
  (org-mode . (lambda ()
                (auto-fill-mode)
                (visual-line-mode)
                (variable-pitch-mode)))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (C . t)
     (python . t)
     (lua . t)
     (matlab . t)))
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-directory fab/org-directory)
  (org-agenda-files `(,(concat fab/org-directory "tasks.org")))
  (org-archive-location "::* Archived Tasks")
  (org-archive-reversed-order t)
  (org-capture-templates
   '(("t" "Tasks")
     ("tf" "Final Exam" entry
      (file+headline "tasks.org" "Finales")
      "** TODO Final %?\nSCHEDULED: %^{Scheduled: }t")
     ("te" "Exam" entry
      (file+headline "tasks.org" "Parciales")
      "** TODO Parcial %?\nSCHEDULED: %^{Scheduled: }t")
     ("tp" "Project/Assignment" entry
      (file+headline "tasks.org" "Trabajos Prácticos")
      "** TODO Trabajo Práctico %?\nDEADLINE: %^{Deadline: }t")
     ("tu" "Unscheduled" entry
      (file+headline "tasks.org" "Unscheduled")
      "** TODO %?")))
  (org-capture-bookmark nil "Don't bookmark last position when capturing")
  (org-id-method 'ts)
  (org-id-ts-format "%Y%m%dT%H%M%S")
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-startup-with-latex-preview t)
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-cycle-hide-drawers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native scripts entities))
  (org-latex-packages-alist '(("" "siunitx" t)
                              ("" "circuitikz" t)))
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-return-follows-link t)
  (org-use-speed-commands t)
  (org-attach-auto-tag nil)
  (org-attach-id-dir (concat fab/org-directory "attach"))
  (org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
                                         org-attach-id-uuid-folder-format
                                         org-attach-id-fallback-folder-format))
  :custom-face
  (org-block ((t (:background nil))))
  (org-document-title ((t (:family "Iosevka Etoile" :height 1.5))))
  )

(use-package org-latex-preview
  :ensure nil
  :after org
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options :page-width 0.8)

  ;; Increase preview scale
  (plist-put org-latex-preview-appearance-options :zoom 1.25)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n and C-p from opening up previews when using auto-mode
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Temp: Use temp dir instead of org-persist (until bug fix)
  (setq org-latex-preview-cache 'temp
        org-latex-preview-process-precompiled nil)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

(use-package corg
  :ensure (:host github :repo "isamert/corg.el")
  :hook (org-mode . corg-setup))

(use-package org-download
  :disabled ; Emacs >= 29 -> `yank-media'
  :config
  (setq org-download-annotate-function (lambda (_)  "Return empty string" ""))
  :custom
  (org-download-method 'attach)
  :bind (:map org-mode-map
              ("C-c s p" . org-download-clipboard)
              ("C-c s s" . org-download-screenshot)))

;;;; Better undo-redo
(use-package undo-fu
  :custom
  (undo-no-redo t)
  :bind
  ([remap undo] . #'undo-fu-only-undo)
  ([remap undo-redo] . #'undo-fu-only-redo))

;;;;; Undo history across sessions
(use-package undo-fu-session
  :config (undo-fu-session-global-mode))

;;;;; Undo tree
(use-package vundo
  :commands (vundo))

;;;; Line numbers
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode LaTeX-mode bibtex-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start 100))

;;;; Better folding
(use-package outline
  :ensure nil
  :hook (prog-mode . outline-minor-mode)
  :custom
  (outline-minor-mode-prefix (kbd "<Bonus-i>"))
  (outline-minor-mode-cycle t)
  (outline-minor-mode-cycle-filter 'bolp))

;;;; Better help
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key)
  ([remap display-local-help] . helpful-at-point)
  :commands
  (helpful-callable helpful-macro))

(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;; Better minibuffer
(use-package vertico
  :config
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :ensure nil
  :after vertico
  :config (vertico-mouse-mode))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config (vertico-multiform-mode))

(use-package marginalia
  :config
  (marginalia-mode))

;;;; Better completion-styles
(use-package orderless
  :custom
  (completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Better completing-read
(use-package consult
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c r" . consult-recent-file)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  ;; Avoid indenting when previewing org files
  (add-to-list 'consult-preview-variables '(org-startup-indented . nil))

  ;; Disable automatic latex preview when using consult live preview
  (add-to-list 'consult-preview-variables '(org-startup-with-latex-preview . nil))
  )

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :after consult)

(use-package avy
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark)
  :bind
  ((:map isearch-mode-map
         ("C-;" . avy-isearch))
   (:map global-map
         ("C-c C-j" . avy-resume)
         ("C-;" . avy-goto-char-timer))))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook ((prog-mode . combobulate-mode))
  :custom
  (combobulate-key-prefix "C-c o"))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-line-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-S-c C-S-c" . mc/edit-lines))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :custom
  (corfu-min-width 20))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :config
  (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(1.25 . 0.5)))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package tempel
  :disabled
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package jinx
  :hook (org-mode LaTeX-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  :custom
  (jinx-languages "es_AR en_US"))

;;;; Better Scrolling
(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 101)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))
  ;; :custom
  ;; (mouse-wheel-progressive-speed nil "Disable mouse wheel acceleration during scrolling")
  ;; (scroll-preserve-screen-position 1 "Prevent the cursor from moving during scrolling")
  ;; (fast-but-imprecise-scrolling t "Improve redisplay performance while scrolling"))
  ;; (pixel-scroll-precision-mode 1) ;; Native smooth scrolling
  ;; (redisplay-skip-fontification-on-input t "Improve redisplay performance while scrolling")

;;;; Better UI
(use-package casual
  :ensure (:host github :repo "kickingvegas/casual")
  :defer t)

(use-package casual-calc
  :ensure nil
  :after calc
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-calendar
  :ensure nil
  :after calendar
  :bind (:map calendar-mode-map ("C-o" . casual-calendar)))

(use-package casual-info
  :ensure nil
  :after info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package casual-dired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map ("M-o" . casual-dired-tmenu)))

(use-package casual-isearch
  :ensure nil
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package ibuffer
  :ensure nil
  :demand t
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer))

(use-package casual-ibuffer
  :ensure nil
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
  :after (ibuffer))

(use-package casual-image
  :ensure nil
  :after image-mode
  :bind (:map image-mode-map
              ("C-o" . #'casual-image-tmenu)))

(use-package re-builder
  :ensure nil
  :defer t)
(use-package casual-re-builder
  :ensure nil
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  :after (re-builder))

(use-package bookmark
  :ensure nil
  :defer t)
(use-package casual-bookmarks
  :ensure nil
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after (bookmark))

(use-package casual-agenda
  :ensure nil
  :bind (:map
         org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump))) ; optional

(use-package casual-editkit
  :ensure nil
  :bind (("M-o" . casual-editkit-main-tmenu)))

(use-package casual-make
  :ensure nil
  :after make-mode
  :config
  (define-key makefile-mode-map (kbd "<C-m>") #'casual-make-tmenu))

(use-package casual-avy
  :bind ("C-M-;" . casual-avy-tmenu))

(use-package symbol-overlay
  :disabled
  :hook
  (prog-mode . symbol-overlay-mode)
  :bind
  ("M-I" . #'symbol-overlay-put)
  ("M-N" . #'symbol-overlay-switch-forward)
  ("M-P" . #'symbol-overlay-switch-backward))

(use-package casual-symbol-overlay
  :disabled
  :after (symbol-overlay)
  :config
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu))

(use-package symbol-overlay-mc
  :disabled
  :after (symbol-overlay casual-symbol-overlay)
  :config
  (symbol-overlay-mc-insert-into-casual-tmenu))

(use-package tab-bar
  :ensure nil
  :init
  (defun fab/tab-bar-format-white-space ()
    " ")
  :config
  (tab-bar-mode)
  (tab-bar-history-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-format '(fab/tab-bar-format-white-space
                    tab-bar-format-menu-bar
                    tab-bar-format-history
                    tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-format-add-tab))
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-history-limit 100))

(use-package bufferlo
  :demand t
  :bind
  (
   ;; buffer / ibuffer
   ("C-z C-b" . bufferlo-ibuffer)
   ("C-z M-C-b" . bufferlo-ibuffer-orphans)
   ("C-z b -" . bufferlo-remove)
   ;; general bookmark (interactive)
   ("C-z b l" . bufferlo-bms-load)
   ("C-z b s" . bufferlo-bms-save)
   ("C-z b c" . bufferlo-bms-close)
   ("C-z b r" . bufferlo-bm-raise)
   ;; dwim frame or tab bookmarks
   ("C-z d s" . bufferlo-bm-save)
   ("C-z d l" . bufferlo-bm-load)
   ("C-z d 0" . bufferlo-bm-close)
   ;; tabs
   ("C-z t s" . bufferlo-bm-tab-save)               ; save
   ("C-z t u" . bufferlo-bm-tab-save-curr)          ; update
   ("C-z t l" . bufferlo-bm-tab-load)               ; load
   ("C-z t r" . bufferlo-bm-tab-load-curr)          ; reload
   ("C-z t 0" . bufferlo-bm-tab-close-curr)         ; kill
   ;; frames
   ("C-z f s" . bufferlo-bm-frame-save)             ; save
   ("C-z f u" . bufferlo-bm-frame-save-curr)        ; update
   ("C-z f l" . bufferlo-bm-frame-load)             ; load
   ("C-z f r" . bufferlo-bm-frame-load-curr)        ; reload
   ("C-z f m" . bufferlo-bm-frame-load-merge)       ; merge
   ("C-z f 0" . bufferlo-bm-frame-close-curr)       ; kill
   ;; sets
   ("C-z s s" . bufferlo-set-save)                  ; save
   ("C-z s u" . bufferlo-set-save-curr)             ; update
   ("C-z s l" . bufferlo-set-load)                  ; load
   ("C-z s 0" . bufferlo-set-close)                 ; kill
   ("C-z s c" . bufferlo-set-clear)                 ; clear
   ("C-z s L" . bufferlo-set-list)                  ; list contents of selected active sets
   )
  :init
  ;; these must be set before the bufferlo package is loaded
  (setq bufferlo-menu-bar-show t)
  (setq bufferlo-menu-bar-list-buffers 'ibuffer)
  (setq bufferlo-prefer-local-buffers 'tabs)
  (setq bufferlo-ibuffer-bind-local-buffer-filter t)
  (setq bufferlo-ibuffer-bind-keys t)
  :config
  (setq bufferlo-mode-line-prefix "🐃") ; "🐮"
  (setq bufferlo-mode-line-set-active-prefix "Ⓢ")
  (setq bufferlo-mode-line-frame-prefix "Ⓕ")
  (setq bufferlo-mode-line-tab-prefix "Ⓣ")
  (setq bufferlo-mode-line-left-prefix nil)
  (setq bufferlo-mode-line-right-suffix nil)
  (setq switch-to-prev-buffer-skip-regexp
        (concat "\\` *"
                "\\(\\*\\(" ; earmuffs
                (mapconcat #'identity
                           '("Messages"
                             "Buffer List"
                             "Ibuffer"
                             "Local Buffer List" ; bufferlo
                             "scratch"
                             "Occur"
                             "Completions"
                             "Help"
                             "Warnings"
                             "Apropos"
                             "Bookmark List"
                             "Async-native-compile-log"
                             "Flymake log"
                             "ruff-format errors"
                             "vc-diff")
                           "\\|")
                "\\)\\*\\)"
                "\\|" (rx "*" (1+ anything) " Ibuffer*")
                "\\|" (rx "*helpful " (1+ anything) "*")
                "\\|" (rx "*tramp" (1+ anything) "*")
                "\\|" (rx "magit" (* anything) ": " (1+ anything))
                "\\'"))
  (setq bufferlo-kill-buffers-prompt t)
  (setq bufferlo-kill-modified-buffers-policy 'retain-modified-kill-without-file-name) ; nil 'retain-modified 'retain-modified-kill-without-file-name 'kill-modified
  (setq bufferlo-bookmark-inhibit-bookmark-point t)
  (setq bufferlo-delete-frame-kill-buffers-prompt t)
  (setq bufferlo-bookmark-frame-save-on-delete 'when-bookmarked)
  (setq bufferlo-bookmark-tab-save-on-close 'when-bookmarked)
  (setq bufferlo-close-tab-kill-buffers-prompt t)
  (setq bufferlo-bookmark-frame-load-make-frame 'restore-geometry)
  (setq bufferlo-bookmark-frame-load-policy 'prompt)
  (setq bufferlo-bookmark-frame-duplicate-policy 'prompt)
  (setq bufferlo-bookmark-tab-replace-policy 'new)
  (setq bufferlo-bookmark-tab-duplicate-policy 'prompt)
  (setq bufferlo-bookmark-tab-in-bookmarked-frame-policy 'prompt)
  (setq bufferlo-bookmark-tab-failed-buffer-policy 'placeholder)
  (setq bufferlo-bookmarks-save-duplicates-policy 'prompt)
  (setq bufferlo-bookmarks-save-frame-policy 'all)
  (setq bufferlo-bookmarks-load-tabs-make-frame t)
  (setq bufferlo-bookmarks-save-at-emacs-exit 'all)
  (setq bufferlo-bookmarks-load-at-emacs-startup 'pred)
  (setq bufferlo-bookmarks-load-at-emacs-startup-tabs-make-frame nil)
  (setopt bufferlo-bookmarks-auto-save-interval (* 60 5)) ; 5 minutes
  (setq bufferlo-bookmarks-auto-save-messages 'saved)
  (setq bufferlo-set-restore-geometry-policy 'all)
  (setq bufferlo-set-restore-tabs-reuse-init-frame 'reuse) ; nil 'reuse 'reuse-reset-geometry
  (setq bufferlo-set-restore-ignore-already-active 'prompt) ; nil 'prompt 'ignore
  (setq bufferlo-frameset-restore-geometry 'bufferlo)
  (setq bufferlo-frame-geometry-function #'bufferlo-frame-geometry-default)
  (setq bufferlo-frame-sleep-for 0.3)

  (setq bookmark-bmenu-type-column-width 12) ; supported in Emacs 31 (innocuous on earlier versions)

  (setq bufferlo-bookmark-buffers-exclude-filters
        (list
         (rx bos " " (1+ anything)) ; ignores "invisible" buffers; e.g., " *Minibuf...", " markdown-code-fontification:..."
         (rx bos "*" (1+ anything) "*") ; ignores "special" buffers; e.g;, "*Messages*", "*scratch*", "*occur*"
         ))

  (setq bufferlo-bookmark-buffers-include-filters
        (list
         (rx bos "*shell*") ; comment out shells if you do not have bookmark support
         (rx bos "*" (1+ anything) "-shell*") ; project.el shell buffers
         (rx bos "*eshell*")
         (rx bos "*" (1+ anything) "-eshell*") ; project.el eshell buffers
         ))

  (defun my/bufferlo-bookmarks-save-p (bookmark-name)
    (string-match-p (rx "=as") bookmark-name))
  (setq bufferlo-bookmarks-save-predicate-functions nil) ; clear the save-all predicate
  (add-hook 'bufferlo-bookmarks-save-predicate-functions #'my/bufferlo-bookmarks-save-p)

  (defun my/bufferlo-bookmarks-load-p (bookmark-name)
    (string-match-p (rx "=al") bookmark-name))
  (add-hook 'bufferlo-bookmarks-load-predicate-functions #'my/bufferlo-bookmarks-load-p)

  (defvar my:bufferlo-consult--source-local-buffers
    (list :name "Bufferlo Local Buffers"
          :narrow   ?l
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-other-buffers
    (list :name "Bufferlo Other Buffers"
          :narrow   ?o
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-non-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-all-buffers
    (list :name "Bufferlo All Buffers"
          :narrow   ?a
          :hidden   t
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name)))
    "All Bufferlo buffer candidate source for `consult-buffer'.")

  ;; add in the reverse order of display preference
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-all-buffers)
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)

  (bufferlo-mode)
  (bufferlo-anywhere-mode))

;;;; Better themes
(use-package ef-themes
  :config
  (ef-themes-select 'ef-owl)
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-to-toggle '(ef-owl ef-light))
  (ef-themes-headings
   '((1 . (1.2))
     (2 . (1.15))
     (agenda-date . (variable-pitch 1.15))
     (agenda-structure . (variable-pitch 1.2))
     (t . (1.1))))
  :bind
  ("<f9>" . #'ef-themes-toggle))

(use-package auto-dark
  :after ef-themes
  :config (auto-dark-mode)
  :custom
  (auto-dark-dark-theme 'ef-owl)
  (auto-dark-light-theme 'ef-light))

(use-package minions
  :config
  (minions-mode))

(use-package spacious-padding
  :demand t
  :after ef-themes
  :config (spacious-padding-mode)
  :bind
  ("<f7>" . #'spacious-padding-mode))

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook (python-base-mode . indent-bars-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package pulsar
  :config
  (pulsar-global-mode 1))

(use-package lin
  :config
  (lin-global-mode)
  :custom
  (lin-face nil "Do not override hl-line-face"))

(use-package hl-line
  :ensure nil
  :defer t
  :custom
  (hl-line-sticky-flag nil))

(use-package olivetti
  :hook (org-mode markdown-mode))

(use-package logos
  :bind
  (([remap narrow-to-region] . #'logos-narrow-dwim)
   ([remap forward-page] . #'logos-forward-page-dwim)
   ([remap backward-page] . #'logos-backward-page-dwim)
   ("<f8>" . #'logos-focus-mode))
  :custom
  (logos-outlines-are-pages t)
  (logos-olivetti t))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :hook (minibuffer-setup . nerd-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))


(use-package bibtex
  :ensure nil
  :mode ("\\.bib\\'" . bibtex-mode)
  :config
  (dolist (format '(realign
                    whitespace
                    last-comma))
    (add-to-list 'bibtex-entry-format format))
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "")))
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-edit-before-use t)
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-year-title-separator "--")
  (bibtex-autokey-titleword-length 8)
  (bibtex-autokey-titlewords nil)
  (bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "and" "of"
                                     "el" "la" "los" "de" "y" "a"
                                     "con" "en" "al"
                                     ;"[^[:upper:]].*"
                                     ".*[^[:upper:][:lower:]0-5].*")))

(use-package persid
  :ensure (:host github :repo "fabcontigiani/persid" :branch "gbooks-api")
  :commands (persid-insert-bibtex))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           ;;for fast i-search in pdf buffers
                           (pdf-isearch-minor-mode)
                           (pdf-isearch-batch-mode)
                           (pdf-outline-minor-mode)
                           (pdf-outline-imenu-enable)
                           (pdf-annot-minor-mode)
                           (pdf-view-themed-minor-mode))))

(use-package org-noter
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-use-indirect-buffer nil)
  (org-noter-auto-save-last-location t)
  (org-noter-disable-narrowing t)
  (org-noter-highlight-selected-text t)
  :bind ("C-c n p" . org-noter))

(use-package denote
  :config
  (require 'consult-denote)
  :custom
  (denote-directory (concat fab/org-directory "denote/"))
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c n n" . denote-create-note)
   ("C-c n o" . denote-open-or-create)
   ("C-c n d" . denote-date)
   ("C-c n i" . denote-link-or-create)
   ("C-c n l" . denote-find-link)
   ("C-c n b" . denote-find-backlink)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-rename-file-keywords)))

(use-package denote-journal
  :hook
  (calendar-mode . denote-journal-calendar-mode)
  :config
  (denote-rename-buffer-mode t)
  :bind
  ("C-c n j" . #'denote-journal-new-or-existing-entry))

(use-package denote-org
  :after denote)

(use-package consult-denote
  :config
  (consult-denote-mode)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep)))

(use-package citar
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :init
  (setq citar--multiple-setup (cons "<tab>"  "RET")) ; <C-i> workaround
  :config
  (require 'bibtex)
  (require 'citar-denote)
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-notebook"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-orange
              :v-adjust -0.1)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))

  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-notes-icons
              citar-indicator-links-icons))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography fab/bibliography-file)
  (citar-library-paths `(,fab/bibliography-dir))
  :bind
  ("C-c n c o" . citar-open)
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode))

(use-package citar-denote
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes nil)
  (citar-denote-file-type 'org)
  (citar-denote-subdir nil)
  (citar-denote-signature nil)
  (citar-denote-template nil)
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "author-year-title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :bind
  (("C-c n c c" . citar-create-note)
   ("C-c n c n" . citar-denote-open-note)
   ("C-c n c d" . citar-denote-dwim)
   ("C-c n c e" . citar-denote-open-reference-entry)
   ("C-c n c a" . citar-denote-add-citekey)
   ("C-c n c k" . citar-denote-remove-citekey)
   ("C-c n c r" . citar-denote-find-reference)
   ("C-c n c l" . citar-denote-link-reference)
   ("C-c n c f" . citar-denote-find-citation)
   ("C-c n c x" . citar-denote-nocite)
   ("C-c n c y" . citar-denote-cite-nocite)))

(use-package auctex
  :ensure (auctex :pre-build (("./autogen.sh")
                              ("./configure"
                               "--without-texmf-dir"
                               "--with-packagelispdir=./"
                               "--with-packagedatadir=./")
                              ("make"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  (LaTeX-mode . prettify-symbols-mode)
  :custom
  ;; (font-latex-fontify-script nil)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-electric-sub-and-superscript t)
  (TeX-command-default "LaTeXMk")
  )

(use-package reftex
  :ensure nil
  :after org
  :config
  (require 'auctex)
  :custom
  (reftex-default-bibliography `(,fab/bibliography-file))
  (reftex-plug-into-AUCTeX t))

(use-package consult-reftex
  :ensure (:host github :repo "karthink/consult-reftex")
  :after reftex)

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex))

(use-package math-delimiters
  :ensure (:host github :repo "oantolin/math-delimiters")
  :config
  (autoload 'math-delimiters-insert "math-delimiters")
  (with-eval-after-load 'org
    (define-key org-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'tex              ; for AUCTeX
    (define-key TeX-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'cdlatex
    (define-key cdlatex-mode-map "$" nil)))

(use-package cmake-mode
  :defer t)

(use-package compile
  :ensure nil
  :custom
  (compilation-auto-jump-to-first-error t))

(use-package dape
  :disabled
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :ensure nil
  :config
  (repeat-mode))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :commands (treesit-fold-mode))

(use-package treesit-jump
  :ensure (:host github :repo "abougouffa/treesit-jump"
                 :branch "enhancements" :files ("*.el" "treesit-queries")))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-default-remote-shell "/bin/bash"))

(use-package eglot
  :ensure nil ;; use built-in
  :defer t
  :hook
  ((c-ts-mode c++-ts-mode python-ts-mode LaTeX-mode) . eglot-ensure)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :custom
  (eglot-autoshutdown t))

(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package consult-yasnippet
  :after yasnippet
  :bind ("C-c s" . #'consult-yasnippet))

(use-package consult-eglot
  :bind (:map eglot-mode-map ("M-g l" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode 1))

(use-package consult-xref-stack
  :ensure (:host github :repo "brett-lempereur/consult-xref-stack")
  :bind ("C-," . consult-xref-stack-backward))

(use-package lsp-snippet
  :disabled
  :ensure (:host github :repo "svaante/lsp-snippet")
  :after eglot
  :config
  (lsp-snippet-tempel-eglot-init))

(use-package breadcrumb
  :hook (eglot-connect . breadcrumb-local-mode))

(use-package flymake
  :ensure nil ;; use built-in
  ;; :custom
  ;; (flymake-show-diagnostics-at-end-of-line 'short)
  :bind
  (:map flymake-mode-map
        ("M-n" . #'flymake-goto-next-error)
        ("M-p" . #'flymake-goto-prev-error)))

(use-package flymake-popon ;; alternative flymake-cursor
  :disabled
  :hook flymake-mode)

(use-package eldoc
  :ensure nil ;; use built-in
  :defer t
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-print-after-edit t)
  (eldoc-echo-area-prefer-doc-buffer nil))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :bind (:map eglot-mode-map
              ([remap display-local-help] . #'eldoc-box-help-at-point))
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t))

(use-package ediff
  :ensure nil
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package transient)

(use-package magit-section)

(use-package magit
  :after nerd-icons
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package diff-hl
  :hook
  (find-file    . diff-hl-mode)
  (vc-dir-mode  . diff-hl-dir-mode)
  (dired-mode   . diff-hl-dired-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil))

(use-package hl-todo
  :ensure (:depth nil)
  :config
  ;; (add-hook 'flymake-diagnostic-functions 'hl-todo-flymake)
  (global-hl-todo-mode))

(use-package consult-todo
  :bind (("M-s t" . consult-todo)
         ("C-x p t" . consult-todo-project)))

(use-package magit-todos
  :disabled
  :after magit
  :config (magit-todos-mode 1))

(use-package apheleia
  :commands
  (aphelia-mode))

(use-package rainbow-mode
  :commands
  (rainbow-mode))

(use-package free-keys
  :ensure (:host github :repo "Fuco1/free-keys")
  :commands (free-keys))

(use-package atomic-chrome
  :commands (atomic-chrome-start-server))

(use-package gptel
  :commands (gptel)
  :config
  (delete (assoc "ChatGPT" gptel--known-backends) gptel--known-backends)
  (setq gptel-model 'gpt-4.1
        gptel-backend (gptel-make-gh-copilot "Copilot")))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :after embark
  :bind (:map embark-general-map
              ("?" . #'gptel-quick)))

(use-package copilot
  :commands (copilot-mode))

;;;; Languages
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook
  (markdown-mode . visual-line-mode))

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.yaml\\'")

(use-package yaml-pro
  :hook
  (yaml-ts-mode . yaml-pro-ts-mode))

(use-package outline-yaml
  :ensure (:host github :repo "jamescherti/outline-yaml.el")
  :hook
  (yaml-ts-mode . outline-yaml-minor-mode))

(use-package matlab-mode
  :ensure (:host github :repo "mathworks/Emacs-MATLAB-Mode")
  :mode "\\.m\\'"
  :custom
  (matlab-shell-command-switches '("-nodesktop" "-nosplash")))

;;;; FPGA Suite
(use-package verilog-ext
  :disabled
  :hook ((verilog-mode . verilog-ext-mode))
  :custom
  ;; Can also be set through `M-x RET customize-group RET verilog-ext':
  ;; Comment out/remove the ones you do not need
  (verilog-ext-feature-list
   '(font-lock
     xref
     capf
     hierarchy
     eglot
     beautify
     navigation
     template
     formatter
     compilation
     imenu
     which-func
     hideshow
     typedefs
     time-stamp
     block-end-comments
     ports))
  :config
  (verilog-ext-mode-setup))

(use-package verilog-mode
  :defer t)

(use-package verilog-ts-mode
  :mode "\\.s?vh?\\'")

(use-package vhdl-ext
  :disabled
  :hook ((vhdl-mode . vhdl-ext-mode))
  :custom
  (vhdl-ext-feature-list
   '(font-lock
     xref
     capf
     hierarchy
     eglot
     beautify
     navigation
     template
     compilation
     imenu
     which-func
     hideshow
     time-stamp
     ports))
  :config
  (vhdl-ext-mode-setup))

(use-package vhdl-ts-mode
  :mode "\\.vhdl?\\'"
  :custom
  (vhdl-modify-date-on-saving nil))

(use-package fpga
  :disabled
  :after (:any verilog-ts-mode vhdl-ts-mode)
  :custom
  (fpga-feature-list '(altera)))

(provide 'init)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; init.el ends here
