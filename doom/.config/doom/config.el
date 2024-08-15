;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

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
(setq doom-font (font-spec :family "JetBrains Mono" :size 15))
;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


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

(after! org
  (setq org-agenda-files `(,(concat org-directory "tasks.org"))
        org-id-method 'ts
        org-id-ts-format "%Y%m%dT%H%M%S"
        org-attach-auto-tag nil
        org-attach-id-dir (concat org-directory "attach")
        org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
                                              org-attach-id-uuid-folder-format
                                              org-attach-id-fallback-folder-format)))

(use-package! denote
  :config
  (require 'denote-journal-extras)
  (require 'consult-denote)
  (denote-rename-buffer-mode t)
  :custom
  (denote-directory (concat org-directory "denote/"))
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (:map doom-leader-notes-map
        ("r n" . denote-create-note)
        ("r o" . denote-open-or-create)
        ("r d" . denote-date)
        ("r i" . denote-link-or-create)
        ("r l" . denote-find-link)
        ("r b" . denote-find-backlink)
        ("r r" . denote-rename-file)
        ("r R" . denote-rename-file-using-front-matter)
        ("r k" . denote-keywords-add)
        ("r K" . denote-keywords-remove)))

(use-package! consult-denote
  :config
  (consult-denote-mode)
  (consult-customize consult-denote-find :state (consult--file-preview))
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  (consult-denote-find-command #'consult-fd)
  :bind
  (:map doom-leader-notes-map
        ("r f" . consult-denote-find)
        ("r g" . consult-denote-grep)))

(use-package! citar
  ;; :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :config
  (require 'citar-denote)
  :custom
  ;; (org-cite-insert-processor 'citar)
  ;; (org-cite-follow-processor 'citar)
  ;; (org-cite-activate-processor 'citar)
  ;;(citar-library-paths (concat org-directory "biblio"))
  (citar-bibliography (concat org-directory "biblio/" "references.bib")))
;; :bind
;; ("C-c n c o" . citar-open)
;; (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package! citar-denote
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
  (:map doom-leader-notes-map
        ("r c c" . citar-create-note)
        ("r c n" . citar-denote-open-note)
        ("r c d" . citar-denote-dwim)
        ("r c e" . citar-denote-open-reference-entry)
        ("r c a" . citar-denote-add-citekey)
        ("r c k" . citar-denote-remove-citekey)
        ("r c r" . citar-denote-find-reference)
        ("r c l" . citar-denote-link-reference)
        ("r c f" . citar-denote-find-citation)
        ("r c x" . citar-denote-nocite)
        ("r c y" . citar-denote-cite-nocite)))
