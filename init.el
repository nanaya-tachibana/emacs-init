;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

;; Add ELPA repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-initialize)

(defvar my-packages
  '(use-package
    yasnippet
    lsp-mode
    lsp-ui
    company
    company-lsp
    better-defaults
    flycheck
    magit
    markdown-mode
    rainbow-delimiters
    fill-column-indicator
    direnv
    smart-mode-line
    yaml-mode
    json-mode
    vue-mode
    auctex
    material-theme
    neotree
    ace-window))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; BASIC CUSTOMIZATION
;; --------------------------------------
(set-language-environment "UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;; hide the startup message
(setq inhibit-startup-message t)
;; hide toolbar
(tool-bar-mode -1)
;; load material theme
(load-theme 'material t)
(global-linum-mode t)
;; display column number
(column-number-mode t)
;; move all backup files to one directory
(setq backup-directory-alist '(("." . "~/.emacs_backup")))

;; set mark
(global-set-key (kbd "C-.") 'set-mark-command)

(require 'rainbow-delimiters)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(if (eq system-type 'darwin)
    (setq mac-option-modifier 'super))
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))
(if (eq system-type 'darwin)
    (set-frame-font "Menlo 15" nil t))

(use-package direnv
  :config
  (direnv-mode))

;; neotree
(use-package neotree
  :bind ([f8] . neotree-toggle))

;; acewindow
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; MARKDOWN
;; --------------------------------------
(use-package gfm-mode
  :mode "\\.md\\'"
  :mode "\\.markdown\\'")

;; FLYSPELL
;; --------------------------------------
(global-unset-key (kbd "C-SPC"))
(define-key key-translation-map (kbd "C-.") (kbd "C-@"))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))


;; LSP
;; --------------------------------------
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook (python-mode . lsp-deferred)
  :hook (c++-mode . lsp-deferred)
  :hook (yaml-mode . lsp-deferred)
  :hook (dockerfile-mode . lsp-deferred)
  :hook (json-mode . lsp-deferred)
  :hook (html-mode . lsp-deferred)
  :hook (css-mode . lsp-deferred)
  :hook (vue-mode . lsp-deferred)
  :commands (lsp lsp-deferred))


(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ;;
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (setq lsp-ui-doc-use-webkit t)
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))


;; COMPANY
;; --------------------------------------
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))

(use-package company-lsp
  :ensure t
  :config
  (setq compnay-lsp-enable-snippet t)
  (push 'company-lsp company-backends))


(setq js-indent-level 2)
;; C++
;; --------------------------------------
;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-irony))

;; (use-package irony
;;   :ensure t
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   )

;; (use-package irony-eldoc
;;   :ensure t
;;   :config
;;   (add-hook 'irony-mode-hook #'irony-eldoc))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cmake-mode vue-mode json-mode yaml-mode lsp-mode dockerfile-mode use-package smart-mode-line rainbow-delimiters neotree material-theme magit lsp-ui flycheck fill-column-indicator direnv company-lsp better-defaults auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
