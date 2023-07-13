(setq confirm-kill-emacs nil)
(setq company-idle-delay nil)
(map! :n "SPC I" #'ispell
      :n "SPC r" #'doom/reload)

;;UNDO
(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))

(setq user-full-name "Joe Moore"
      user-mail-address "jo3moore@gmail.com")
(setq projectile-project-search-path '("~/.config/qtile" "~/Shaders" "~/Documents/GitHub/website/website"))

(setq doom-font (font-spec :family "JetBrainsMonoNerdFont" :size 18))
(setq doom-variable-pitch-font (font-spec :family "Alegreya" :size 18))





(use-package! visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins nil))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)
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
(setq org-directory "~/org/")
(setq org-roam-index-file "~/org/roam/index.org")
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook '+org-pretty-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook #'mixed-pitch-mode)
(add-hook 'org-mode-hook #'solaire-mode)
)
(setq mixed-pitch-variable-pitch-cursor nil)
(map! :n "SPC n r t" #'org-roam-tag-add
      :n "SPC d" #'org-download-clipboard)

(setq-default org-download-image-dir: "~/Pictures/org-download")
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(custom-theme-set-faces
 'user
 '(org-level-8 ((t ( :height 1.0))))
 '(org-level-7 ((t ( :height 1.0))))
 '(org-level-6 ((t ( :height 1.1))))
 '(org-level-5 ((t ( :height 1.15))))
 '(org-level-4 ((t ( :foreground "#A1E5AB" :height 1.25))))
 '(org-level-3 ((t ( :foreground "#F9DB6D" :height 1.5))))
 '(org-level-2 ((t ( :foreground "#EEB4B3" :height 1.75))))
 '(org-level-1 ((t ( :weight bold :foreground "#86BBD8" :height 2.0))))
 '(org-document-title ((t ( :weight bold :foreground "#FFFFFF" :height 2.5 :underline t)))))

(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; (after! org
;;   (setq org-agenda-files "~/org/agenda.org"))

(use-package! org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :config
    (setq org-auto-tangle-default t))

(map! :n "SPC l" #'org-latex-preview)
(defun zz/adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))
(add-hook! org-mode (zz/adjust-org-company-backends))

;;Very important setup for previews. dvipng should be the fastest, but may not support all
(setq org-preview-latex-default-process 'dvipng)


(load "auctex.el" nil t t)
(require 'tex-mik)
;;Fix for latex that possibly does nothing
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space]\n]*$" ""
                (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
          (setenv "PATH" path-from-shell)
          (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

(setq cdlatex-env-alist
     '(("bmatrix" "\\begin{bmatrix}\n\?\&\ \\\\ \&\n\\end{bmatrix}\n" nil)
       ("Fraction" "\$\\frac\{\?\}\{\}\$\n" nil)
       ("Tabular" "\\begin{tabular*}}\?\n\\end{tabular*}\n" nil)))

(setq cdlatex-command-alist
 '(("mat" "Insert bmatrix env"   "" cdlatex-environment ("bmatrix") t nil)
   ("fr" "Insert Fraction env" "" cdlatex-environment ("Fraction") t nil)
   ("tab" "Insert Tabular env" "" cdlatex-environment ("Tabular") t nil)))

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

;; (use-package codeium
;;   :straight '(:type git :host github :repo "Exafunction/codeium.el")'
;;   :init
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;                  (setq-local completion-at-point-functions
;;                         (list (cape-super-capf # codeium-completion-at-point #'lsp-completion-at-point)))))
;;   :config
;;   (setq use-dialog-box nil)    ;; Disabling popup boxes
;;   ;; Codeium status in modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest HeartBeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions HeartBeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion'))))
;;   ;;Limitng the string sent to codeium for better performance
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text')
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))
(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "/.virtualenvs/"))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
; (add-load-path! "lisp")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;               (setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
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
