;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Fabrizio Contigiani"
      user-mail-address "fabcontigiani@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 14))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
;; (after! diff-hl (adwaita-dark-theme-diff-hl-fringe-bmp-enable))
;; (after! flycheck (adwaita-dark-theme-flycheck-fringe-bmp-enable))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! math-delimiters
  :after (:any auctex org)
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

(after! org
  (setq org-directory "/home/fab/Documents/note-box/")
  (setq org-agenda-files `("/home/fab/Documents/note-box/tasks.org"))
  (setq org-attach-id-dir "/home/fab/Documents/note-box/attachments/")
  (setq org-attach-dir "/home/fab/Documents/note-box/attachments/")
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq org-startup-with-latex-preview t)
  ;; (setq org-startup-indented t)
  ;; (setq org-startup-folded nil)
  ;; (setq org-cycle-hide-drawers t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-highlight-latex-and-related '(native scripts entities))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  ;; Fix org-mode latex preview background color
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

(add-hook! org-mode
  (org-fragtog-mode)
  (org-appear-mode))

(setq org-roam-directory "/home/fab/Documents/note-box/")

(use-package! consult-org-roam
  :after org
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
  ;; Disable indentation when using consult live preview
  (add-to-list 'consult-preview-variables '(org-startup-indented . nil)))

(map! :map doom-leader-notes-map
      ;; Define some convenient keybindings as an addition
      "r e" #'consult-org-roam-file-find
      "r b" #'consult-org-roam-backlinks
      "r l" #'consult-org-roam-forward-links
      "r S" #'consult-org-roam-search)

(after! citar
  (setq citar-bibliography '("/home/fab/Documents/note-box/references.bib"))
  (setq citar-notes-paths '("/home/fab/Documents/note-box/pages/")))

(after! org-roam
  (setq org-roam-directory "/home/fab/Documents/note-box/")
  (setq org-roam-dailies-directory "journals/")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?" :target
           (file+head "pages/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))))

