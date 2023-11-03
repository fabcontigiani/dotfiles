;;; init.el --- Initialization file for Emacs

;;; Commentary:
;; 

;;; Code:

(use-package emacs
  :init
  (require 'package)
  (setq package-archives
        '(("elpa" . "https://elpa.gnu.org/packages/")
          ("elpa-devel" . "https://elpa.gnu.org/devel/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
  (setq package-archive-priorities
        '(("elpa" . 2)
          ("nongnu" . 1)))
  ;; (setq package-pinned-packages
  ;;       '((org . "elpa-devel")))

  ;; Store automatic customization options elsewhere
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
  ;; (setq use-package-compute-statistics t)

  ;; Not needed on Emacs 30
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))

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

  ;; Font configuration
  (set-face-attribute 'default nil :font "Iosevka" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 120)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 120)
  (set-face-attribute 'fixed-pitch-serif nil :font "Iosevka Slab" :height 120)

  :config
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
  ;; Elisp compilation warnings
  (native-comp-async-report-warnings-errors nil "Don't report errors from async native compilation")
  (byte-compile-warnings '(not lexical
                               free-vars
                               noruntime
                               unresolved
                               docstrings))

  ;; General configuration
  (truncate-lines t "Truncate lines instead of wrapping")
  (kill-whole-line t "Include newline character when killing a line.")
  (context-menu-mode t "Enable global context menu support")
  (message-truncate-lines t "Truncate messages in the echo area")
  (cursor-in-non-selected-windows nil "Hide cursor in inactive windows")
  (ring-bell-function 'ignore "Disable terminal bell")
  (fill-column 80 "Set default line-wrap column to column 80")
  (max-mini-window-height 10 "Limit minibuffer height to 10 lines")
  (enable-recursive-minibuffers t "Allow minibuffer commands to be called in the minibuffer")
  (use-dialog-box nil)
  (load-prefer-newer t "Load from source files if they are newer than bytecode files")
  (read-extended-command-predicate #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode.")


  ;; Startup
  ;; (initial-scratch-message "" "Leave scratch buffer empty on startup")
  ;; (initial-major-mode 'fundamental-mode "Set initial mode to fundamental-mode on startup")
  ;; (inhibit-startup-screen t "Do not create or show the initial splash screen")
  ;; (inhibit-default-init t "Do not attempt to load any OS-provided init files")

  ;; Default style rules
  (sentence-end-double-space nil "Do not use double spacing between sentences in paragraphs.")
  (require-final-newline t "Require a terminating newline at the end of every file.")
  (indent-tabs-mode nil "Use spaces for indentation")
  (tab-width 4 "Use 4 spaces for indentation")

  ;; Scrolling
  (mouse-wheel-progressive-speed nil "Disable mouse wheel acceleration during scrolling")
  (scroll-preserve-screen-position 1 "Prevent the cursor from moving during scrolling")
  (scroll-conservatively 101 "Scroll only one line at a time when cursor leaves view")
  (scroll-margin 5 "Maintain margin of 5 lines around cursor during scrolling")
  (fast-but-imprecise-scrolling t "Improve redisplay performance while scrolling")

  ;; Performance tweaks
  (redisplay-skip-fontification-on-input t "Improve redisplay performance while scrolling")
  (fast-but-imprecise-scrolling t "Improve redisplay performance while scrolling")
  (jit-lock-defer-time 0 "Defer fontification while input is pending")
  (auto-window-vscroll nil "Prevent calcuation of arbitrary line heights while scrolling")
  (auto-mode-case-fold nil "Disable case-insensitive second pass over `auto-mode-alist'")

  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  (confirm-kill-emacs #'y-or-n-p)
  (shell-kill-buffer-on-exit t)
  (global-auto-revert-non-file-buffers t)
  (package-install-upgrade-built-in t)
  (tab-always-indent 'complete "Enable indentation+completion using the TAB key")
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")

  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (delete-by-moving-to-trash t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t))

(use-package dired-sidebar
  :bind
  ("C-x C-l" . dired-sidebar-toggle-sidebar))

(use-package nerd-icons-dired
  :hook dired-mode)

(use-package display-line-numbers
  :hook (prog-mode LaTeX-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start 100))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

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
  (evil-undo-system 'undo-fu)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook))

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

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys nil t))

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode))

(use-package evil-numbers
  :after evil
  :config
  :bind (:map evil-normal-state-map
              ("g C-a" . evil-numbers/inc-at-pt)
              ("g C-x" . evil-numbers/dec-at-pt)))

(use-package avy
  :bind (:map evil-normal-state-map
              ("z f" . evil-avy-goto-char-timer)
              ("z j" . evil-avy-goto-line)))

(use-package which-key
  :init (which-key-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key)
  ([remap display-local-help] . helpful-at-point))

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

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
         ("M-s f" . consult-fd)
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
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-separator ?\s)          ;; Orderless field separator
  :hook
  ;; Displaying popups aggressively (i.e. without summoning them with a key press) can
  ;; cause the cursor to jump around in `eshell-mode'
  (eshell-mode . (lambda () (setq-local corfu-auto nil))))


(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :after doom-themes
  :config
  (solaire-global-mode))

(use-package doom-modeline
  :hook emacs-startup)

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package indent-bars
  :vc (:fetcher github :repo jdtsmith/indent-bars)
  :hook prog-mode
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)) ; blend=1: blend with BG only
  (indent-bars-highlight-current-depth '(:blend 0.5))) ; pump up the BG blend on current  (indent-bars-highlight-current-depth nil)

(use-package diff-hl
  :init (global-diff-hl-mode)
  :config (diff-hl-flydiff-mode))

(use-package hl-todo
  :hook prog-mode)

(use-package visual-fill-column
  :hook org-mode
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100))

(use-package org
  :defer t
  :vc (org-mode :url "https://git.tecosaur.net/tec/org-mode.git" :branch "dev")
  :hook
  (org-mode . (lambda ()
                (auto-fill-mode)
                (visual-line-mode)
                (variable-pitch-mode)))

  :config

  (require 'org-latex-preview)
 
  (setq org-format-latex-options (plist-put org-format-latex-options :background "Transparent"))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  ;; Fix org-mode latex preview background color
  ;; (require 'org-src)
  ;; (add-to-list 'org-src-block-faces '("latex" (:background "Transparent")))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  :custom
  (org-directory "/home/fab/Documents/note-box/")
  (org-agenda-files `("/home/fab/Documents/note-box/inbox.org"))
  (org-log-done 'time)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-startup-with-latex-preview t)
  (org-preview-latex-default-process 'dvipng)
  (org-preview-latex-image-directory (concat user-emacs-directory ".cache/ltximg/" (buffer-file-name)))
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-cycle-hide-drawers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-highlight-latex-and-related '(native scripts entities))
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-return-follows-link t)

  :custom-face
  (org-level-1 ((t (:font "Iosevka Etoile" :height 1.5))))
  (org-level-2 ((t (:font "Iosevka Etoile" :height 1.3))))
  (org-level-3 ((t (:font "Iosevka Etoile" :height 1.25))))
  (org-level-4 ((t (:font "Iosevka Etoile" :height 1.2))))
  (org-level-5 ((t (:font "Iosevka Etoile" :height 1.15))))
  (org-level-6 ((t (:font "Iosevka Etoile" :height 1.1))))
  (org-level-7 ((t (:font "Iosevka Etoile" :height 1.1))))
  (org-level-8 ((t (:font "Iosevka Etoile" :height 1.1)))))


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
  (org-download-image-dir "/home/fab/Documents/note-box/assets/"))

(use-package org-fragtog
  :hook org-mode)

(use-package org-appear
  :hook org-mode)

(use-package org-modern
  :hook org-mode
  :custom
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-table nil)
  (org-modern-priority nil)
  (org-modern-timestamp nil)
  (org-modern-todo nil))

(use-package org-modern-indent
  :vc (:fetcher github :repo jdtsmith/org-modern-indent)
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory "/home/fab/Documents/note-box/")
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
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
  (consult-customize consult-org-roam-forward-links :preview-key "M-.")
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
  :defer t
  :custom
  (bibtex-dialect 'biblatex))

(use-package citar
  :custom
  (org-cite-global-bibliography '("/home/fab/Documents/note-box/references.bib"))
  (citar-notes-paths '("/home/fab/Documents/note-box/pages/"))
  (org-cike-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)

  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode)
  :custom
  (citar-at-point-function 'embark-act))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(use-package org-ref
  :commands (isbn-to-bibtex isbn-to-bibtex-lead isbn-to-bibtex-open-library))

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :custom
  (lua-indent-level 2))

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

(use-package auctex-latexmk
  :after tex
  :hook
  ;; Set LatexMk as the default.
  (LaTeX-mode . (lambda () (setq-local TeX-command-default "LatexMk")))
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package evil-tex
  :hook LaTeX-mode)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (pdf-view-midnight-minor-mode)
                           (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))
  :config
  (pdf-tools-install)

(use-package org-noter
  :bind ("C-c n p" . org-noter)
  :custom
  (org-noter-auto-save-last-location t))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  :custom
  (treesit-auto-install t))

(use-package flymake
  :hook prog-mode)

(use-package flymake-popon
  :hook flymake-mode)

(use-package eglot
  :hook
  ((c-ts-mode c++-ts-mode python-ts-mode) . eglot-ensure))

(use-package rainbow-mode
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package wgrep
  :defer t)

(provide 'init)

;;; init.el ends here
