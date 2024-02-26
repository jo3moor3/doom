;May help some performance issues
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq confirm-kill-emacs nil) ;Disable quit confirmation
(setq company-idle-delay nil) ;Completion now triggers via CTRL-SPC

;Open an emacs buffer with super+alt+SPC to edit text from any program.
;Then confirm edit with the following keybinding:
(map!
 :map emacs-everywhere-mode-map
 "C-c C-c" #'emacs-everywhere--finish-or-ctrl-c-ctrl-c)

;;Undo and redo
(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))

(after! ispell
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "en_US,fr_FR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))
(map! :n "SPC I" #'ispell)

(defun reload_all ()
(interactive)
(doom/reload)
(sit-for 3) ; delay for doom/reload to finish
(org-reload))

(map! :n "SPC r" #'reload_all)

(setq user-full-name "Theodore Moore"
      user-mail-address "jo3moore@gmail.com")
(setq projectile-project-search-path '("~/Shaders" "~/Documents/GitHub/" "~/code/"))

(setq doom-font (font-spec :family "JetBrainsMonoNerdFont" :size 18))
(setq doom-variable-pitch-font (font-spec :family "Alegreya" :size 18))

;Relative line numbers is nice for vim(evil) movement!
(setq display-line-numbers-type 'relative)

(use-package! visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins nil))

;disabling solaire mode for now because of conflicts
(after! solaire-mode (solaire-global-mode -1))
;Window opacity for seeing my beautiful desktop
(add-to-list 'default-frame-alist '(alpha . 90))
;Theme config begins
(use-package doom-themes
  :ensure t
  :config
  ;Themes to chooose from
  ;(load-theme 'doom-tokyo-night  t)
  (load-theme 'doom-dracula  t)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq fancy-splash-image (concat doom-private-dir "/home/moore/Pictures/bengal.png"))

(defun NONO-EMACS ()
          (let* ((banner '(
"      ___           ___           ___           ___     "
"     /\\__\\         /\\  \\         /\\__\\         /\\  \\    "
"    /::|  |       /::\\  \\       /::|  |       /::\\  \\   "
"   /:|:|  |      /:/\\:\\  \\     /:|:|  |      /:/\\:\\  \\  "
"  /:/|:|  |__   /:/  \\:\\  \\   /:/|:|  |__   /:/  \\:\\  \\ "
" /:/ |:| /\\__\\ /:/__/ \\:\\__\\ /:/ |:| /\\__\\ /:/__/ \\:\\__\\"
" \\/__|:|/:/  / \\:\\  \\ /:/  / \\/__|:|/:/  / \\:\\  \\ /:/  /"
"     |:/:/  /   \\:\\  /:/  /      |:/:/  /   \\:\\  /:/  / "
"     |::/  /     \\:\\/:/  /       |::/  /     \\:\\/:/  /  "
"     /:/  /       \\::/  /        /:/  /       \\::/  /   "
"     \\/__/         \\/__/         \\/__/         \\/__/    "
"                                                        "
"                        E M A C S                       "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 102)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'NONO-EMACS)

(after! org
(setq org-element-use-cache nil)
(setq org-directory "~/org/")
(setq org-roam-index-file "~/org/roam/index.org")
(add-hook 'org-mode-hook 'org-eldoc-load))
;org download for pasting images
(setq-default org-download-image-dir: "~/Pictures/org-download")
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
;Org auto tangle
(use-package! org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :config
    (setq org-auto-tangle-default t))

(after! org
(setq org-modern-star nil)
(setq org-startup-folded t)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook '+org-pretty-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook #'mixed-pitch-mode)
;Make latex fragments easy to edit/preview
(add-hook 'org-mode-hook 'org-fragtog-mode))

(setq mixed-pitch-variable-pitch-cursor nil)

(defun zz/adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))
;(add-hook! org-mode (zz/adjust-org-company-backends))

;;Setup for previews. dvipng is the fastest, but may not support all
(setq org-preview-latex-default-process 'dvipng)

(load "auctex.el" nil t t)
(require 'tex-mik)

(map! :n "SPC d" #'org-download-clipboard)
(map! :n "SPC n r t" #'org-roam-tag-add)
(map! :n "SPC l" #'org-latex-preview)

;; ;;CAPE
;; (use-package corfu
;;   :init
;;   (global-corfu-mode))
;; (use-package cape
;;   :bind )
;; (setq-local completion-at-point-functions
;;             (mapcar #'cape-company-to-capf
;;                     (list #'company-file #'company-ispell #'company-dabbrev)))
;; ;;CODEIUM
;; ;;COMPANY
;;     (use-package company
;;       :defer 0.1
;;       :config
;;       (global-company-mode t)
;;       (setq-default
;;        company-idle-delay 0.05
;;        company-require-match nil
;;        company-minimum-prefix-length 0
;;        company-frontends '(company-preview-frontend)  ;; get only preview
;;        ))

;DEBUGGER
(after! dap-mode
  (setq dap-python-debuger 'debugpy))
;Style
(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;Virtual enviroment
(setq conda-env-autoactivate-mode t)
(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "~/.conda/envs/"))

(use-package! conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.conda"))
  (setq conda-env-home-directory (expand-file-name "~/.conda")))

;keybindings
(map! :n "SPC P" #'run-python
      :n "SPC e a" #'conda-env-activate
      :n "SPC e d" #'conda-env-deactivate
      :n "SPC g p" #'magit-push)

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;; See 'C-h v doom-font' for documentation and more examples of what they
;;
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
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
