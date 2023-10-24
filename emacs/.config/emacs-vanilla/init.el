(use-package emacs
  :init
  (require 'package)
  (add-to-list 'package-archives '(("melpa" . "https://melpa.org/packages/")
                                   ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)

  ;; Store automatic customization options elsewhere
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  (setq use-package-compute-statistics t)

  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 120)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120)

  :config
  (global-display-line-numbers-mode t)
  (electric-pair-mode t)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (column-number-mode 1)
  (save-place-mode t)
  (savehist-mode t)
  (recentf-mode t)
  (winner-mode 1)
  (global-auto-revert-mode 1)
  (global-hl-line-mode t)
  (blink-cursor-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p) ;; change all prompts to y or n

  :custom
  (warning-minimum-level :error)
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (confirm-kill-emacs #'y-or-n-p)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (package-install-upgrade-built-in t)
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package windmove
  :init
  (windmove-default-keybindings 'meta)
  :bind
  (("M-h" . windmove-left)
   ("M-j" . windmove-down)
   ("M-k" . windmove-up)
   ("M-l" . windmove-right)))

(use-package undo-fu)
(use-package undo-fu-session
  :after undo-fu
  :config (undo-fu-session-global-mode))

(use-package evil
  :init
  (evil-mode)
  :bind (("<escape>" . keyboard-escape-quit))
  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
  :custom
  (evil-want-keybinding nil)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-fu)
  (evil-want-C-u-scroll t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  :custom
  (evil-snipe-scope 'visible)
  (evil-snipe-smart-case t))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package which-key
  :init (which-key-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)))

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

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
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
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

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
  )

(use-package embark
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode)
  :config
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
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
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
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

(use-package math-delimiters
  :vc (:fetcher github :repo oantolin/math-delimiters)
  :config
  (autoload 'math-delimiters-insert "math-delimiters")
  (with-eval-after-load 'org
    (define-key org-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'tex              ; for AUCTeX
    (define-key TeX-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'tex-mode         ; for the built-in TeX/LaTeX modes
    (define-key tex-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'cdlatex
    (define-key cdlatex-mode-map "$" nil)))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "en_US es_AR"))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package mood-line
  :config
  (mood-line-mode))

(use-package adwaita-dark-theme
  :config
  (load-theme 'adwaita-dark :no-confirm)
  (adwaita-dark-theme-arrow-fringe-bmp-enable)
  (eval-after-load 'diff-hl #'adwaita-dark-theme-diff-hl-fringe-bmp-enable)
  ;(eval-after-load 'flymake #'adwaita-dark-theme-flymake-fringe-bmp-enable)
  ;(eval-after-load 'neotree #'adwaita-dark-theme-neotree-configuration-enable)
  :custom
  (adwaita-dark-theme-bold-vertico-current t "Embolden the currently-selected candidate in vertico"))
  ;(adwaita-dark-theme-gray-rainbow-delimiters t "Use a gray color for rainbow-delimiters faces"))

(use-package solaire-mode
  :init (solaire-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vi-tilde-fringe
  :hook (prog-mode . vi-tilde-fringe-mode))

(use-package indent-bars
  :vc (:fetcher github :repo jdtsmith/indent-bars)
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-tabs-mode nil)
  (indent-bars-treesit-support t)
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)) ; blend=1: blend with BG only
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current  (indent-bars-highlight-current-depth nil)
  (indent-bars-display-on-blank-lines nil))

(use-package magit
  :custom
  (magit-diff-refine-hunk t))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :config (diff-hl-flydiff-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package org
  :defer t
  ;; :hook
  ;; (org-mode . (lambda ()
  ;;               (variable-pitch-mode 1)
  ;;               (visual-line-mode 1)
  ;;               (display-line-numbers-mode -1)))

  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  ;; Fix org-mode latex preview background color
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.125)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1))))
    ;(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  ;(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  (setq org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
                                              org-attach-id-uuid-folder-format
                                              org-attach-id-fallback-folder-format))

  :custom
  (org-directory "/home/fab/Documents/note-box/")
  (org-agenda-files `("/home/fab/Documents/note-box/inbox.org"))
  (org-hide-emphasis-markers t)
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
  (org-id-method 'ts)
  (org-attach-id-dir "/home/fab/Documents/note-box/assets/")
  (org-attach-auto-tag nil)
  (org-attach-store-link-p 'attached))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-download
  :after org
  :config
  (setq org-download-annotate-function (lambda (link) (previous-line 1) ""))
  :custom
  (org-download-method 'attach))

;(use-package visual-fill-column
  ;:hook (org-mode . visual-fill-column-mode)
  ;:custom
  ;(visual-fill-column-width 100)
  ;(visual-fill-column-center-text t))

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory "/home/fab/Documents/note-box/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n t" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  ;; Disable automatic latex preview when using consult live preview
  (add-to-list 'consult-preview-variables '(org-startup-with-latex-preview . nil))
  (add-to-list 'consult-preview-variables '(org-startup-indented . nil))
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n s" . consult-org-roam-search))

(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex))

(use-package citar
  :custom
  (org-cite-global-bibliography '("/home/fab/Documents/note-box/references.bib"))
  (org-cike-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)

  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(use-package tex
  :defer t
  :ensure auctex
  :custom
  (font-latex-fontify-script nil))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex)
  :custom
  (cdlatex-insert-auto-labels-in-env-templates nil))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)

  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1)
                           (pdf-view-midnight-minor-mode)
                           (set (make-local-variable 
                                 'evil-normal-state-cursor)
                                (list nil)))))

(use-package org-noter
  :after org
  :custom
  (org-noter-auto-save-last-location t))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  :custom
  (treesit-auto-install t))

(use-package eat
  :vc (:fetcher codeberg :repo akib/emacs-eat)
  :custom
  (eat-kill-buffer-on-exit t))
