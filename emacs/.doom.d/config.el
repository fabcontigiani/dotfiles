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
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 16))
      ;;doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      ;;doom-serif-font (font-spec :family "Iosevka Etoile" :size 16)
      ;;doom-unicode-font (font-spec :family "Iosevka Nerd Font" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

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

;; Set up LaTeX snippet preview and input aid
(setq org-startup-with-latex-preview t)
(setq org-preview-latex-default-process 'dvisvgm)
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; Multiple language setup
(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure English and Spanish.
  (setq ispell-dictionary "en_US,es_AR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es_AR")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale.
  (setq ispell-personal-dictionary "~/note-box/.hunspell_personal"))

;; org-sticky-header
(setq org-sticky-header-full-path 'full)
(add-hook 'org-mode-hook 'org-sticky-header-mode)

;; Logseq compatibility
;; taken from https://coredumped.dev/2021/05/26/taking-org-roam-everywhere-with-logseq/
(setq org-directory "/home/fab/note-box/"
      org-roam-directory "/home/fab/note-box/pages/"
      org-roam-dailies-directory "/home/fab/note-box/journals/"
      org-attach-id-dir "/home/fab/note-box/assets/"
      org-roam-capture-templates
      '(("d" "default" plain
         "%?" :target
         (file+head "${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

;; Org-noter directory
(setq org-noter-notes-search-path '("/home/fab/note-box/notes/"))

(setq citar-bibliography '("/home/fab/note-box/biblio/references.bib")
       citar-notes-paths '("/home/fab/note-box/biblio/"))

;; Don't fontify subscripts and superscripts
(setq org-pretty-entities-include-sub-superscripts 'nil)
(setq font-latex-fontify-script 'nil)
