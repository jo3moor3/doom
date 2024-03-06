(after! evil
  (setq evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

(setq confirm-kill-emacs nil) ;Disable quit confirmation

(after! org
(setq org-roam-completion-everywhere nil)) ;Disable org roam completions

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(after! org
(set-flyspell-predicate! '(org-mode)
  #'+org-flyspell-word-p))

(defun +org-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on this list."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'org-level-1 faces))
	(not (cl-loop with unsafe-faces ='(org-code
        org-roam-olp
        org-verbatim
        org-property-value
        org-block-begin-line
        font-lock-comment-face
        font-lock-comment-delimiter-face
        font-lock-constant-face
        font-lock-keyword-face
        font-lock-function-name-face
        font-lock-string-face
        org-block-end-line
        org-block-begin-line
        org-table
        org-column
		   org-document-info-keyword
		   org-document-info-keyword
		   org-link
		   org-block
		   org-tag
		   org-modern-tag)
		      for face in faces
		      if (memq face unsafe-faces)
		      return t)))))

(when (version< "29.0.50" emacs-version)
  (pixel-scroll-precision-mode))

;;(setq gc-cons-threshold 20000000)

(setq yas-triggers-in-field t)

(setq which-key-idle-delay 0.5)

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

(setq user-full-name "Theodore Moore"
      user-mail-address "jo3moore@gmail.com")
(setq projectile-project-search-path '("~/Shaders" "~/code/"))

(after! ispell
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "en_US,fr_FR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))

(setq ispell-personal-dictionary
      (expand-file-name ".hunspell_en_US" doom-private-dir))

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

(custom-set-faces!
`(corfu-default :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg))
`(org-block :inherit (fixed-pitch))
`(org-code :inherit (shadow fixed-pitch))
'(org-document-info :foreground "wheat")
'(org-document-info-keyword :inherit (shadow fixed-pitch))
'(org-document-title :weight bold :foreground "#FFFFFF" :height 2.5 :underline nil)
'(org-indent :inherit (org-hide fixed-pitch))
'(org-level-1 :weight bold :foreground "#86BBD8" :height 2.0)
'(org-level-2 :foreground "#EEB4B3" :height 1.75)
'(org-level-3 :foreground "#F9DB6D" :height 1.5)
'(org-level-4 :foreground "#A1E5AB" :height 1.25)
'(org-level-5 :height 1.15)
'(org-level-6 :height 1.1)
'(org-level-7 :height 1.0)
'(org-level-8 :height 1.0)
'(org-link :foreground "lavender" :underline t)
'(org-meta-line :inherit font-lock-comment-face fixed-pitch)
'(org-property-value :inherit fixed-pitch)
'(org-special-keyword :inherit font-lock-comment-face fixed-pitch)
'(org-table :inherit fixed-pitch :foreground "#83a598")
'(org-tag :inherit shadow fixed-pitch :weight bold :height 0.8)
'(org-verbatim :inherit shadow fixed-pitch))

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

(map! :g "C-s" #'save-buffer)

(map! :n "SPC I" #'ispell)
(map! :n "C-S-i" #'ispell-word)

(map! :desc "iedit" :nv "C-;" #'iedit-mode)

(map! :n "M-f" #'consult-ripgrep)
(map! :after evil :gnvi "C-f" #'isearch-toggle-word)
(define-key isearch-mode-map "\C-j" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-k" 'isearch-repeat-backward)

(map! :map emacs-everywhere-mode-map
      "C-c C-c" #'emacs-everywhere--finish-or-ctrl-c-ctrl-c)

(after! undo-fu
  (map! :map undo-fu-mode-map
        "C-S-z" #'undo-fu-only-redo
         :nvi "C-z" #'undo-fu-only-undo))

(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

;; Add extensions
(use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Context
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point) ;; Ai
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; elisp code block
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Files and directories
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(add-hook 'python-mode-hook
        (lambda ()
          (setq-local completion-at-point-functions
                (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point #'cape-file)))))

;; (setq-local completion-at-point-functions
;;             (list (cape-capf-buster #'codeium-completion-at-point)))

(use-package corfu
  ;; Optional customizations
   :custom
  ;;(corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
   (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (corfu-echo-mode t)
  (global-corfu-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 0)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(setq nerd-icons-corfu-mapping
      '(;; lsp
        (array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        (function :style "md" :icon "function_variant" :face font-lock-function-name-face)
        (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
        (method :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
        (param :style "fa" :icon "gear" :face default)
        (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
        ; Keyword
        ; Variable
        ;; File and Directory
        (file :style "fa" :icon "file" :face font-lock-string-face)
        (folder :style "fa" :icon "folder" :face font-lock-doc-face)
        ;; Codeium
        (magic :style "fa" :icon "magic" :face font-lock-string-face)
        ;; Dabbrev
        (text :style "cod" :icon "library" :face font-lock-string-face)
        ;; Default
        (t :style "cod" :icon "code" :face font-lock-warning-face)))

(use-package corfu-candidate-overlay
  :after corfu
  :config
  ;; enable corfu-candidate-overlay mode globally
  ;; this relies on having corfu-auto set to nil
  (corfu-candidate-overlay-mode +1)

  ;Whenever I delete these keybindings it throws an error?
  ;Even if I make a more Doom-like keybinding as a replacement...
  (global-set-key (kbd "M-<tab>") 'completion-at-point)

  (map! :map 'override "C-<iso-lefttab>" #'corfu-candidate-overlay-complete-at-point))

(corfu-prescient-mode t)
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

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

(defvar splash-phrase-source-folder
  (expand-file-name "splash-phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splash-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defun splash-phrase-select-set ()
  "Select a specific splash phrase set."
  (interactive)
  (setq splash-phrase-set (completing-read "Phrase set: " (mapcar #'car splash-phrase-sources)))
  (+doom-dashboard-reload t))

(defvar splash-phrase--cached-lines nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splash-phrase--cached-lines))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splash-phrase--cached-lines)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun splash-phrase-dashboard-formatted ()
  "Get a splash phrase, flow it over multiple lines as needed, and fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defun splash-phrase-dashboard-insert ()
  "Insert the splash phrase surrounded by newlines."
  (insert "\n" (splash-phrase-dashboard-formatted) "\n"))

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

(defun +doom-dashboard-benchmark-line ()
  "Insert the load time line."
  (when doom-init-time
    (insert
     "\n\n"
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       (doom-display-benchmark-h 'return))
      'face 'doom-dashboard-loaded))))

(setq +doom-dashboard-functions
      (list #'doom-dashboard-widget-banner
            #'+doom-dashboard-benchmark-line
            #'splash-phrase-dashboard-insert
            #'doom-dashboard-widget-footer))

(defun +doom-dashboard-tweak (&optional _)
  (with-current-buffer (get-buffer +doom-dashboard-name)
    (add-hook! '+doom-dashboard-functions (hide-mode-line-mode 1))
    (setq-local mode-line-format nil
                evil-normal-state-cursor (list nil))))

(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)

(setq +doom-dashboard-name "► Doom"
      doom-fallback-buffer-name +doom-dashboard-name)

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
(setq chatgpt-shell-welcome-function nil)

(setq chatgpt-shell-openai-key
      (lambda ()
        (nth 0 (process-lines "pass" "show" "openai-key"))))

(map! :leader
      :prefix ("o g" . "chatGPT")
      :desc "open GPTshell" :nv "g" #'chatgpt-shell
      :desc "explain code" :nv "c" #'chatgpt-explain-code
      :desc "make unit test" :nv "u" #'chatgpt-generate-unit-test
      :desc "proofread" :nv "p" #'chatgpt-shell-proofread-region)

(after! org
(setq org-element-use-cache nil)
(setq org-directory "~/org/")
(setq org-roam-index-file "~/org/roam/index.org")
(add-hook 'org-mode-hook 'org-eldoc-load))
(setq org-use-property-inheritance t)

(setq org-roam-capture-templates `(("d" "default" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}"):unnarrowed t)))

(setq-default org-download-image-dir: "~/Pictures/org-download")
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-ellipsis " ▾")
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

(after! org
(setq org-startup-folded t)
(add-hook 'org-mode-hook '+org-pretty-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook #'mixed-pitch-mode))

(setq mixed-pitch-variable-pitch-cursor nil)

(use-package! org-modern
  :hook (org-mode . org-modern-mode))

(use-package! org-modern
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
        org-modern-horizontal-rule (make-string 36 ?─))
(custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(use-package! org-modern
  :config
  (setq org-modern-keyword
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
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶"))))

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

(map! :n "SPC g p" #'magit-push)

(setq lsp-enable-file-watchers 1)

(use-package codeium
    :init
    ;; use globally
    ;;(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
     (add-hook 'emacs-startup-hook
      (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
)

;; we recommend using use-package to organize your init.el
(use-package codeium
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;DEBUGGER
(after! dap-mode
  (setq dap-python-debuger 'debugpy))
;Style
(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package! virtualenvwrapper)
(after! virtualenvwrapper
  (setq venv-location "~/.virtualenvs"))

;; (use-package! conda
;;   :ensure t
;;   :init
;;   (setq conda-anaconda-home (expand-file-name "~/.conda"))
;;   (setq conda-env-home-directory (expand-file-name "~/.conda")))

(use-package python-pytest
 :custom
 (python-pytest-confirm t)
 :config
 ;; just an extra `-y' after the `-x' suffix
 (transient-append-suffix
   'python-pytest-dispatch
   "-x"
   '("-y" "The Y" "-y"))
 ;; group with `-z' after second from the last group,
 ;; that is before `Run tests'
 (transient-append-suffix
   'python-pytest-dispatch
   '(-2)
   ["My Z"
    ("-z" "The Z" "-z")]))

(map! (:prefix ("M-p" . "Python")
      :desc "run python" :nv "p" #'run-python
      :desc "add dependency" :nv "a" #'poetry-add
      :desc "remove dependency" :nv "r" #'poetry-remove
      :desc "update dependencies" :nv "u" #'poetry-update
      :desc "show dependencies" :nv "s" #'poetry-show
      :desc "lock dependencies" :nv "l" #'poetry-lock
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
