(require 'package)
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/plugins/restclient.el")
(package-initialize)

(defvar local-packages
  '(projectile
    auto-complete
    bash-completion
    jedi
    pungi
    swiper
    cider
    ac-cider
    flx
    flx-ido
    ido-vertical-mode
    ido-ubiquitous
    smex
    recentf
    plsense
    bongo
    bash-completion
    powerline
    web-mode
    nlinum
    smooth-scrolling
    rainbow-delimiters
    color-theme-sanityinc-tomorrow
    ace-jump-mode
    ergoemacs-mode))
(dolist (p local-packages)
  (unless (package-installed-p p)
    (package-install p)))

(recentf-mode t)
(defun my-recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(powerline-default-theme)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(ac-config-default)
(setq ac-auto-show-menu 0.7)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-delay 0)
(setq ac-quick-help-delay 1)
(setq eldoc-idle-delay 0)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(ido-vertical-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)
(smex-initialize)

(projectile-global-mode)

;; enable web modes
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
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
 
;; configure python auto complete
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'pungi:setup-jedi)

;; configure clojure
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages nil)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "<f7>") 'my-recentf-ido-find-file)
(global-set-key (kbd "<f8>") 'switch-to-buffer)

(bash-completion-setup)

(setq show-paren-style 'mixed)
(scroll-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(menu-bar-mode -1)
(global-linum-mode)
(setq inhibit-startup-screen t)

;; replace default search with swiper
(defalias 'isearch-forward 'swiper)
(defalias 'isearch-backward 'swiper)

;; configure look appearence and keymap
(load-theme 'sanityinc-tomorrow-night t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff9e6deb9cfc908381c1267f407b8830bcad6028231a5f736246b9fc65e92b44" default)))
 '(ergoemacs-ctl-c-or-ctl-x-delay 0.2)
 '(ergoemacs-handle-ctl-c-or-ctl-x (quote both))
 '(ergoemacs-ini-mode t)
 '(ergoemacs-keyboard-layout "us")
 '(ergoemacs-mode nil)
 '(ergoemacs-smart-paste nil)
 '(ergoemacs-theme "standard")
 '(ergoemacs-theme-options
   (quote
    ((apps-toggle off)
     (apps-apps off)
     (apps-swap off)
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
 '(org-CUA-compatible nil)
 '(org-replace-disputed-keys nil)
 '(org-special-ctrl-a/e nil)
 '(org-support-shift-select nil)
 '(pyvenv-mode t)
 '(pyvenv-tracking-mode t)
 '(recentf-menu-before "Open File...")
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom nil)
 '(set-mark-command-repeat-pop nil)
 '(shift-select-mode t)
 '(size-indication-mode t)
 '(smex-prompt-string "M-x "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
