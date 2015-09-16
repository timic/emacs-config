(package-initialize)
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar local-packages
  (quote
   (projectile
    auto-complete
    jedi
    flx
    flx-ido
    ido-vertical-mode
    neotree
    web-mode
    color-theme-sanityinc-tomorrow
    ergoemacs-mode)))
(dolist (p local-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'ido)
(require 'flx)
(require 'flx-ido)
(require 'ido-vertical-mode)
(setq ido-enable-flex-matching t)
(setq ido-vertical-show-count t)
(setq ido-create-new-buffer 'always)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(ido-vertical-mode t)

;; builtin
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-set-key (kbd "<f7>") 'bs-show)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)

(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)

;; enable web modes
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; enable markdown mode

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-css-dir "~/.emacs.d/plugins/md-css/")
(setq markdown-css-theme "clearness")

;; enable groovy mode
(require 'groovy-mode)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
 
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; configure python auto complete
(require 'jedi)
(setq jedi:setup-keys t)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(defvar jedi-config:python-module-sentinel "__init__.py")

; configure look appearence and keymap
(load-theme 'sanityinc-tomorrow-night t)

(setq show-paren-style 'expression)
(show-paren-mode 2)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "c7e8605c82b636fc489340e8276a3983745891e18e77440bbae305d1b5af9201" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(ergoemacs-ctl-c-or-ctl-x-delay 0.2)
 '(ergoemacs-handle-ctl-c-or-ctl-x (quote both))
 '(ergoemacs-ini-mode t)
 '(ergoemacs-keyboard-layout "us")
 '(ergoemacs-mode nil)
 '(ergoemacs-smart-paste nil)
 '(ergoemacs-theme "standard")
 '(ergoemacs-theme-options
   (quote
    ((apps-swap off)
     (f2-edit off)
     (apps off)
     (fn-keys off))))
 '(ergoemacs-use-menus t)
 '(fringe-mode 0 nil (fringe))
 '(global-hl-line-mode t)
 '(ido-vertical-define-keys (quote C-n-and-C-p-only))
 '(initial-scratch-message
   ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to open a file, visit that file with C-O,
;; then enter the text in that file's own buffer.

")
 '(line-number-mode nil)
 '(minimap-mode nil)
 '(minimap-width-fraction 0.05)
 '(minimap-window-location (quote right))
 '(org-CUA-compatible nil)
 '(org-special-ctrl-a/e nil)
 '(org-support-shift-select nil)
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom nil)
 '(set-mark-command-repeat-pop nil)
 '(shift-select-mode t)
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
