;May help sreen flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq confirm-kill-emacs nil) ;Disable quit confirmation
(setq company-idle-delay nil) ;Disable company auto-complete

;; (defvar no-flyspell-list '("config.org"))

;; (defun turn-off-flyspell-if-match ()
;;   (if (member (file-name-nondirectory (buffer-file-name)) no-flyspell-list)
;;       (flyspell-mode -1)))

;; (add-hook 'find-file-hook #'turn-off-flyspell-if-match)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(when (version< "29.0.50" emacs-version)
  (pixel-scroll-precision-mode))

(setq which-key-idle-delay 0.5)

(setq yas-triggers-in-field t)
;;(setq gc-cons-threshold 20000000) ;increase garbage collection threshold

(after! ispell
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "en_US,fr_FR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))

(setq ispell-personal-dictionary
      (expand-file-name ".hunspell_en_US" doom-private-dir))

(map! :n "SPC I" #'ispell)
(map! :n "C-S-i" #'ispell-word)

(set-flyspell-predicate! '(org-mode)
  #'+org-flyspell-word-p)

(defun +org-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on this list."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'org-level-1 faces))
	(not (cl-loop with unsafe-faces = '(org-code
                   org-roam-olp
                   org-property-value
                   org-block-begin-line
                   org-column
					    org-document-info
					    org-document-info-keyword
					    org-link
					    org-block
					    org-tag
					    org-modern-tag)
		      for face in faces
		      if (memq face unsafe-faces)
		      return t)))))

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

;; Add extensions
(use-package cape
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

;; Use Company backends as Capfs.
(setq-local completion-at-point-functions
  (mapcar #'cape-company-to-capf
    (list #'company-files #'company-keywords #'company-dabbrev #'company-ispell)))

(require 'company)
;; Use the company-dabbrev and company-elisp backends together.
(setq completion-at-point-functions
      (list
       (cape-company-to-capf
        (apply-partially #'company--multi-backend-adapter
                         '(company-dabbrev company-elisp)))))

(setq-local completion-at-point-functions
            (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))

;; (setq-local completion-at-point-functions
;;             (list (cape-capf-buster #'dabbrev-capf)))

(use-package corfu
  ;; Optional customizations
   :custom
   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
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
;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;(setq nerd-icons-corfu-mapping
      ;; '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
      ;;   (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
      ;;   (t :style "cod" :icon "code" :face font-lock-warning-face)))
        ;; Remember to add an entry for `t', the library uses that as default.

;; The Custom interface is also supported for tuning the variable above.

(use-package corfu-candidate-overlay
  :after corfu
  :config
  ;; enable corfu-candidate-overlay mode globally
  ;; this relies on having corfu-auto set to nil
  (corfu-candidate-overlay-mode +1)

  ;Whenever I delete these keybindings it throws an error?
  ;Even if I make a more Doom-like keybinding as a replacement...
  (global-set-key (kbd "M-<tab>") 'completion-at-point)
  (global-set-key (kbd "C-<iso-lefttab>") 'corfu-candidate-overlay-complete-at-point))

(corfu-prescient-mode 1)

(use-package corfu
  ;:custom
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map ("SPC" . corfu-insert-separator)))
;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless

   ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(map! :g "C-s" #'save-buffer)

(map! :desc "iedit" :nv "C-;" #'iedit-mode)

(map! :after evil :gnvi "C-f" #'consult-line)

(map! :map emacs-everywhere-mode-map
      "C-c C-c" #'emacs-everywhere--finish-or-ctrl-c-ctrl-c)

(after! undo-fu
  (map! :map undo-fu-mode-map
        "C-S-z" #'undo-fu-only-redo
         :nvi "C-z" #'undo-fu-only-undo))

(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

;disabling solaire mode for now because of conflicts
(after! solaire-mode (solaire-global-mode -1))
;       Window opacity for seeing my beautiful desktop
(add-to-list 'default-frame-alist '(alpha . 90))
;Theme config begins
(use-package doom-themes
  :ensure t
  :config
  ;Default theme
  (load-theme 'doom-dracula  t)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors") ;; or for treemacs users
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq doom-font (font-spec :family "JetBrainsMonoNerdFont" :size 18))
(setq doom-variable-pitch-font (font-spec :family "Alegreya" :size 18))

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

;;Only setup required besides downloading the package
(require 'pcmpl-args)

;;Corfu setup
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
    (comint-send-input))))

(advice-add #'corfu-insert :after #'corfu-send-shell)

(require 'chatgpt-shell)
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-key")))))

(setq chatgpt-shell-openai-key
      (lambda ()
        (nth 0 (process-lines "pass" "show" "openai-key"))))

(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))

(after! org
(setq org-element-use-cache nil)
(setq org-directory "~/org/")
(setq org-roam-index-file "~/org/roam/index.org")
(add-hook 'org-mode-hook 'org-eldoc-load))
(setq org-use-property-inheritance t)
;org download for pasting images
(setq-default org-download-image-dir: "~/Pictures/org-download")
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-ellipsis " ▾")
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
(after! org
(setq org-startup-folded t)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook '+org-pretty-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook #'mixed-pitch-mode))

(setq mixed-pitch-variable-pitch-cursor nil)

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("bibliography" . "")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

;Make latex fragments easy to edit/preview
(after! org
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;;Setup for previews. dvipng is the fastest, but may not support all
(setq org-preview-latex-default-process 'dvipng)

(load "auctex.el" nil t t)
(require 'tex-mik)

(map! :n "SPC n r t" #'org-roam-tag-add
      (:prefix ("SPC l" . "link")
      :desc "store org link" :nv "s" #'org-store-link
      :desc "insert org link" :nv "i" #'org-insert-link
      :desc "link url" :nv "u" #'org-cliplink
      :desc "link image" :nv "p" #'org-download-clipboard
      ))

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
      (:prefix ("M-p" . "Python")
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
