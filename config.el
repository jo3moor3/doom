;May help some performance issues
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq confirm-kill-emacs nil) ;Disable quit confirmation
(setq company-idle-delay nil) ;Completion now triggers via CTRL-SPC

(when (version< "29.0.50" emacs-version)
  (pixel-scroll-precision-mode))

(after! ispell
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "en_US,fr_FR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))
(map! :n "SPC I" #'ispell)

(setq user-full-name "Theodore Moore"
      user-mail-address "jo3moore@gmail.com")
(setq projectile-project-search-path '("~/Shaders" "~/Documents/GitHub/" "~/code/"))

(defun reload_all ()
(interactive)
(doom/reload)
(sit-for 3) ; delay for doom/reload to finish
(org-reload))

;For even more stability after reloading.
(add-hook! 'reload_all (doom-load-envvars-file (expand-file-name "env" doom-local-dir) t))

(map! :n "SPC r" #'reload_all)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and (eq major-mode 'comint-mode)
             (string-match "finished" string)
             (not
              (with-current-buffer buffer
                (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (let ((window (get-buffer-window buf)))
                        (when (and (window-live-p window)
                                   (eq buf (window-buffer window)))
                          (delete-window window))))
                    buffer)))

(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)

(map! :g "C-s" #'save-buffer)

(map! :desc "iedit" :nv "C-=" #'iedit-mode)

(map! :after evil :gnvi "C-f" #'consult-line)

(map! :map emacs-everywhere-mode-map
      "C-c C-c" #'emacs-everywhere--finish-or-ctrl-c-ctrl-c)

(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))

(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)

(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file" :ng "f" #'find-file
        :desc "Recent files" :ng "r" #'consult-recent-file
        :desc "Config dir" :ng "C" #'doom/open-private-config
        :desc "Open config.org" :ng "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
        :desc "Open dotfile" :ng "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Open qtile" :ng "q" (cmd! (doom-project-find-file "~/.config/qtile/"))
        :desc "Notes" :ng "n" #'org-roam-node-find
        :desc "Switch buffers (all)" :ng "B" #'consult-buffer
        :desc "IBuffer" :ng "i" #'ibuffer
        :desc "Previous buffer" :ng "p" #'previous-buffer
        :desc "Set theme" :ng "t" #'consult-theme
        :desc "Quit" :ng "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings" :ng "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))

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

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

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

;;(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 2)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(after! org
(setq org-element-use-cache nil)
(setq org-directory "~/org/")
(setq org-roam-index-file "~/org/roam/index.org")
(add-hook 'org-mode-hook 'org-eldoc-load))
;org download for pasting images
(setq-default org-download-image-dir: "~/Pictures/org-download")
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

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

;Used this function when company was acting up with latex. keeping it around just in case.
(defun zz/adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))
;;(add-hook! org-mode (zz/adjust-org-company-backends))

;Make latex fragments easy to edit/preview
(after! org (add-hook 'org-mode-hook 'org-fragtog-mode))

;;Setup for previews. dvipng is the fastest, but may not support all
(setq org-preview-latex-default-process 'dvipng)

(load "auctex.el" nil t t)
(require 'tex-mik)

(map! :n "SPC n r t" #'org-roam-tag-add
      (:prefix ("SPC l" . "link")
      :desc "store org link" :nv "s" #'org-store-link
      :desc "insert org link" :nv "i" #'org-insert-link
      :desc "insert url" :nv "c" #'org-cliplink
      :desc "paste image" :nv "d" #'org-download-clipboard
      ))

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

(setq conda-env-autoactivate-mode t)
(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "~/.conda/envs/"))

(use-package! conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.conda"))
  (setq conda-env-home-directory (expand-file-name "~/.conda")))

(map! :n "SPC g p" #'magit-push
      (:prefix ("SPC c p" . "Python")
      :desc "run python" :nv "p" #'run-python
      :desc "activate conda" :nv "a" #'conda-env-activate
      :desc "deactivate conda" :nv "d" #'conda-env-deactivate
      ))

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
