;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
;;;; Bootstrap elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;;;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t
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
  (defvar fab/org-directory (expand-file-name "~/MEGA/org/"))
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

  ;; Scrolling
  (mouse-wheel-progressive-speed nil "Disable mouse wheel acceleration during scrolling")
  (scroll-preserve-screen-position 1 "Prevent the cursor from moving during scrolling")
  (scroll-conservatively 101 "Scroll only one line at a time when cursor leaves view")
  (scroll-margin 5 "Maintain margin of 5 lines around cursor during scrolling")
  (fast-but-imprecise-scrolling t "Improve redisplay performance while scrolling")

  ;; Performance tweaks
  (redisplay-skip-fontification-on-input t "Improve redisplay performance while scrolling")
  (inhibit-compacting-font-caches t)
  (jit-lock-defer-time 0 "Defer fontification while input is pending")
  (auto-window-vscroll nil "Prevent calcuation of arbitrary line heights while scrolling")
  (auto-mode-case-fold nil "Disable case-insensitive second pass over `auto-mode-alist'")

  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (shell-kill-buffer-on-exit t)
  (global-auto-revert-non-file-buffers t "Revert Dired and other buffers")
  (tab-always-indent 'complete "Enable indentation+completion using the TAB key")
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")

  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

  :config

  ;; Convenience
  (delete-selection-mode 1) ;; Save a keystroke
  (electric-pair-mode t) ;; Tidy parenthesis
  (repeat-mode 1) ;; It bears repeating
  (save-place-mode t) ;; Remember and restore the last cursor location of opened files
  (savehist-mode t) ;; Save what you enter into minibuffer prompts
  (recentf-mode t) ;; Keep track of recently opened files
  (winner-mode 1) ;; Record changes to window configuration
  (global-auto-revert-mode 1) ;; Revert buffers when the underlying file has changed
  (pixel-scroll-precision-mode 1) ;; Native smooth scrolling

  ;; UI
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (column-number-mode 1)

  ;; Font configuration
  (set-face-attribute 'default nil :font "Iosevka" :height 140)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 140)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140)
  (set-face-attribute 'fixed-pitch-serif nil :font "Iosevka Slab" :height 140)
  )

(use-package isearch
  :ensure nil
  :defer t
  :custom
  (isearch-lazy-count t)
  (search-whitespace-regexp ".?*"))

(use-package dired
  :ensure nil
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (delete-by-moving-to-trash t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

;;;; Org-mode
(use-package org
  :defer
  :ensure `(org
            :remotes ("tecosaur"
                      :repo "https://git.tecosaur.net/tec/org-mode.git"
                      :branch "dev")
            :files (:defaults "etc")
            :build t
            :pre-build
            (with-temp-file "org-version.el"
              (require 'lisp-mnt)
              (let ((version
                     (with-temp-buffer
                       (insert-file-contents "lisp/org.el")
                       (lm-header "version")))
                    (git-version
                     (string-trim
                      (with-temp-buffer
			            (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
			            (buffer-string)))))
		        (insert
		         (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
		         (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
		         "(provide 'org-version)\n")))
            :pin nil)
  :hook
  (org-mode . (lambda ()
                (auto-fill-mode)
                (visual-line-mode)
                (variable-pitch-mode)))
  :custom
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
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-startup-with-latex-preview t)
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-cycle-hide-drawers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native scripts entities))
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-return-follows-link t)
  (org-use-speed-commands t)
  (org-babel-load-languages '((emacs-lisp . t)
                              (latex . t)
                              (C . t)
                              (python . t)
                              (lua . t)))
  (org-attach-auto-tag nil)
  (org-attach-id-dir (concat fab/org-directory "attach"))
  (org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
                                         org-attach-id-uuid-folder-format
                                         org-attach-id-fallback-folder-format))
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

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25))

(use-package org-download
  :config
  (setq org-download-annotate-function (lambda (_)  "Return empty string" ""))
  :custom
  (org-download-method 'attach)
  :bind (:map org-mode-map
              ("C-c s p" . org-download-clipboard)
              ("C-c s s" . org-download-screenshot)))

(use-package org-tree-slide
  :bind (:map org-mode-map
              ("<f8>" . org-tree-slide-mode))
  :custom
  (org-tree-slide-slide-in-effect nil))

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
  (outline-minor-mode-cycle t))

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
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;; Better completing-read
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
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
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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
  (global-corfu-mode))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :config
  (corfu-popupinfo-mode))

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


;;;; Better UI
(use-package casual-calc
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package casual-dired
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))

(use-package casual-avy
  :bind ("M-g" . casual-avy-tmenu))

(use-package casual-isearch
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :defer t)
(use-package casual-ibuffer
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

(use-package re-builder
  :ensure nil
  :defer t)
(use-package casual-re-builder
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  :after (re-builder))

(use-package bookmark
  :ensure nil
  :defer t)
(use-package casual-bookmarks
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after (bookmark))

(use-package casual-agenda
  :bind (:map
         org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump))) ; optional

(use-package casual-editkit
  :bind (("M-o" . casual-editkit-main-tmenu)))

(use-package symbol-overlay
  :config
  (symbol-overlay-mode 1)
  :bind
  ("M-I" . #'symbol-overlay-put)
  ("M-n" . #'symbol-overlay-switch-forward)
  ("M-p" . #'symbol-overlay-switch-backward))
(use-package casual-symbol-overlay
  :bind (:map
         symbol-overlay-map
         ("C-o" . casual-symbol-overlay-tmenu))
  :after (symbol-overlay))
;;;; Better themes
(use-package ef-themes
  :config
  (ef-themes-select 'ef-dream)
  :custom
  (ef-themes-bold-constructs t)
  (ef-themes-italic-constructs t)
  (ef-themes-mixed-fonts t)
  (ef-themes-to-toggle '(ef-dream
                         ef-reverie)))

(use-package auto-dark
  :config (auto-dark-mode t)
  :custom
  (auto-dark-dark-theme 'ef-dream)
  (auto-dark-light-theme 'ef-reverie))

(use-package spacious-padding
  :config
  (spacious-padding-mode))

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package indent-bars
  :ensure (:fetcher github :repo "jdtsmith/indent-bars")
  :commands
  (indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-prefer-character t))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package pulsar
  :config
  (pulsar-global-mode 1))

(use-package lin
  :config
  (lin-global-mode))

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
  (bibtex-comma-after-last-field t)
  (bibtex-autokey-edit-before-use t)
  (bibtex-autokey-titleword-length 7)
  (bibtex-autokey-titlewords nil)
  (bibtex-autokey-titleword-ignore '("A" "An" "On" "The"
                                     "[^[:upper:]].*"
                                     ".*[^[:upper:][:lower:]0-5].*")))

(use-package persid
  :ensure (:fetcher github :repo "fabcontigiani/persid" :branch "main")
  :commands (persid-insert-bibtex)
  :custom
  (persid-isbn-generate-citekey 'user))

(use-package pdf-tools
  :ensure (pdf-tools :pre-build ("./server/autobuild") :files (:defaults "server/epdfinfo"))
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
  (org-noter-use-indirect-buffer nil)
  (org-noter-auto-save-last-location t)
  (org-noter-disable-narrowing t)
  :bind ("C-c n p" . org-noter))

(use-package denote
  :config
  (require 'denote-journal-extras)
  (require 'consult-denote)
  (denote-rename-buffer-mode t)
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
   ("C-c n k" . denote-rename-file-keywords)
   ("C-c n j" . #'denote-journal-extras-new-or-existing-entry)))

(use-package consult-denote
  :ensure (:fetcher github :repo "protesilaos/consult-denote")
  :config
  (consult-denote-mode)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep)))

(use-package citar
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :config
  (require 'citar-denote)
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
  :ensure (:fetcher github :repo "karthink/consult-reftex")
  :after reftex)

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex))

(use-package math-delimiters
  :ensure (:fetcher github :repo "oantolin/math-delimiters")
  :config
  (autoload 'math-delimiters-insert "math-delimiters")
  (with-eval-after-load 'org
    (define-key org-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'tex              ; for AUCTeX
    (define-key TeX-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'cdlatex
    (define-key cdlatex-mode-map "$" nil)))

(use-package auctex-latexmk
  :disabled
  :after auctex
  :hook
  ;; Set LatexMk as the default.
  (LaTeX-mode . (lambda () (setq-local TeX-command-default "LatexMk")))
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package eglot
  :ensure nil ;; use built-in
  :defer t
  :hook
  ((c-ts-mode c++-ts-mode python-ts-mode LaTeX-mode) . eglot-ensure)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :custom
  (eglot-autoshutdown t))

(use-package eglot-booster
  :ensure (:fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package consult-eglot
  :bind (:map eglot-mode-map ("M-g l" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode 1))

(use-package lsp-snippet
  :ensure (:fetcher github :repo "svaante/lsp-snippet")
  :after eglot
  :config
  (lsp-snippet-tempel-eglot-init))

(use-package breadcrumb
  :config
  (breadcrumb-mode))

(use-package flymake
  :ensure nil ;; use built-in
  :defer t
  :custom
  (flymake-no-changes-timeout 1.5))

(use-package flymake-popon ;; alternative flymake-cursor
  :hook flymake-mode)

(use-package eldoc
  :ensure nil ;; use built-in
  :defer t
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  ;; (eldoc-print-after-edit t)
  (eldoc-echo-area-prefer-doc-buffer 'maybe))

(use-package eldoc-box
  :after eldoc
  :bind (:map eglot-mode-map
              ([remap eldoc-doc-buffer] . eldoc-box-help-at-point))
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t))

(use-package transient)

(use-package magit-section)

(use-package magit)

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
  (add-hook 'flymake-diagnostic-functions 'hl-todo-flymake)
  (global-hl-todo-mode))

(use-package consult-todo
  :bind (("M-s t" . consult-todo)
         ("C-x p t" . consult-todo-project)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package apheleia
  :commands
  (aphelia-mode))

(use-package rainbow-mode
  :commands
  (rainbow-mode))

(use-package free-keys
  :ensure (:fetcher github :repo "Fuco1/free-keys")
  :commands (free-keys))

(use-package atomic-chrome
  :commands (atomic-chrome-start-server))

(use-package gptel
  :init
  (load-file (concat user-emacs-directory "gemini-apikey.el"))
  :config
  (setq gptel-model 'gemini-pro
        gptel-backend (gptel-make-gemini "Gemini"
                        :key #'gemini-apikey
                        :stream t)))
(use-package gptel-quick
  :ensure (:fetcher github :repo "karthink/gptel-quick")
  :after embark
  :bind (:map embark-general-map
              ("?" . #'gptel-quick)))

;;;; Languages
(use-package markdown-mode
  :mode "\\.md\\'"
  :hook
  (markdown-mode . visual-line-mode))

(provide 'init)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; init.el ends here
