;; NOTE: init.el is now generated from init.org
;;       Please edit that file in Emacs and init.el will be generated automatically!

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-file.el"))
(load custom-file 'noerror 'nomessage)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(menu-bar-mode -1)    ; Disable the menu bar
(horizontal-scroll-bar-mode -1)

;; Disable cursor blink
(blink-cursor-mode -1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Highlight current line.
(global-hl-line-mode t)

(set-face-attribute 'default nil :font "Iosevka" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 120)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 120)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq display-line-numbers-type 'relative)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :init (solaire-global-mode))

(use-package doom-modeline)
  :init (doom-modeline-mode 1)

(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vi-tilde-fringe
  :hook (prog-mode . vi-tilde-fringe-mode))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package minimap
  :defer t)

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package undo-fu) ;; needed for undo functionality

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer fab/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(fab/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(load-theme :which-key "choose theme")
  ":"  '(execute-extended-command :which-key "execute command")
  "."  '(find-file :which-key "find file"))

(use-package which-key
  :init (which-key-mode)
  :diminish wich-key-mode
  :config
  (setq which-key-delay 0.3))

(use-package vertico
  :straight (:files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 16)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-mouse
  :straight nil
  :after vertico
  :config (vertico-mouse-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
(recentf-mode 1)
(save-place-mode 1)

;; Corfu
(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c c p" . completion-at-point) ;; capf
         ("C-c c t" . complete-tag)        ;; etags
         ("C-c c d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c c h" . cape-history)
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword)
         ("C-c c s" . cape-symbol)
         ("C-c c a" . cape-abbrev)
         ("C-c c i" . cape-ispell)
         ("C-c c l" . cape-line)
         ("C-c c w" . cape-dict)
         ("C-c c \\" . cape-tex)
         ("C-c c _" . cape-tex)
         ("C-c c ^" . cape-tex)
         ("C-c c &" . cape-sgml)
         ("C-c c r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-'" . embark-dwim)        ;; good alternative: M-.
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

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
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
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
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

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

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
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :straight (:type built-in)
  :init
  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  ;; Revert Dired and other buffers
  (setq global-auto-revert-non-file-buffers t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

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

(defun fab/org-mode-setup ()
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun fab/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Etoile" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :hook (org-mode . fab/org-mode-setup)
  :init (setq org-directory "/home/fab/org/")
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-startup-with-latex-preview t
        org-highlight-latex-and-related '(native scripts entities)
        org-src-preserve-indentation nil 
        org-edit-src-content-indentation 0
        org-return-follows-link t
        org-preview-latex-image-directory "/tmp/ltximg/"
        org-attach-id-dir "/home/fab/note-box/assets/"
        org-attach-dir "/home/fab/note-box/assets/"
        org-attach-store-link-p 'attached)
  
  ;; Fix org-mode latex preview background color
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  (fab/org-font-setup)
  
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(defun fab/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . fab/org-mode-visual-fill))

(use-package org-modern
  :after org
  :custom
  (org-modern-table nil)
  (org-modern-block-fringe nil)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package org-fragtog
  :after org
  :init
  (add-hook 'org-mode-hook #'org-fragtog-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-auto-tangle
  :after org
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-sticky-header
  :after (org org-modern)
  :defer t
  :hook (org-mode . org-sticky-header-mode))

(use-package org-download
   :after org
   :custom
   (org-download-method 'attach)
   (org-download-screenshot-method '"scrot -s -l mode=edge %s"))

(use-package org-roam
  :custom
  (org-roam-directory "/home/fab/note-box/pages/")
  :bind (("C-c n t" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package citar-org-roam
  :after org-roam
  :config (citar-org-roam-mode))

(use-package consult-org-roam
   :after org-roam
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
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(use-package citar
  :custom
  (org-cite-global-bibliography '("/home/fab/note-box/references/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("/home/fab/note-box/references/references.bib"))
  (citar-notes-paths '("/home/fab/note-box/pages/"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :no-require
  :config (citar-embark-mode))

(use-package org-noter
    :custom
        (org-noter-notes-search-path '("/home/fab/note-box/pages"))
    :general
        (general-nmap
          :keymaps '(org-noter-mode-map pdf-view-mode-map)
            "I" 'org-noter-insert-note-toggle-no-questions
            "i" 'org-noter-insert-note))

(use-package jinx
  :init
     (global-jinx-mode)
     (setq jinx-languages '("en" "es"))
(keymap-global-set "<remap> <ispell-word>" #'jinx-correct))

(use-package langtool
  :straight (:host github :repo "mhayashi1120/Emacs-langtool")
  :custom
  (langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(use-package tex
    :straight auctex
    :init
    (require 'texmathp)
    (require 'preview))

(use-package cdlatex
   :hook
   (LaTeX-mode . turn-on-cdlatex)
   (org-mode . turn-on-org-cdlatex))

(use-package math-delimiters
  :straight (:host github :repo "oantolin/math-delimiters")
  :config
  (autoload 'math-delimiters-insert "math-delimiters")
  (with-eval-after-load 'org
    (define-key org-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'tex              ; for AUCTeX
    (define-key TeX-mode-map "$" #'math-delimiters-insert))
  (with-eval-after-load 'cdlatex
    (define-key cdlatex-mode-map "$" nil)))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package magit)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects"))))

(use-package consult-projectile
  :after projectile)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package consult-flycheck
  :after flycheck)

(use-package rg
  :defer t)

(use-package tree-sitter
  :init (global-tree-sitter-mode)
  :hook
  (python-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install :no-query))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (python-mode . lsp-mode) ; pip install python-language-server[all]
	 (LaTeX-mode . lsp-mode) ; pacman -S texlab
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package consult-lsp
  :after lsp-mode)

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)
