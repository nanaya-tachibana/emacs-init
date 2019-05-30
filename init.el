;;Swap command key and option key

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
;;Add ELPA repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-initialize)

(defvar my-packages
  '(better-defaults
    elpy
    flycheck
    py-autopep8
    js2-mode
    json-mode
    magit
    markdown-mode
    rainbow-delimiters
    fill-column-indicator
    direnv
    smart-mode-line
    web-mode
    yaml-mode
    auctex
    material-theme
    neotree
    ace-window))

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (not package-archive-contents)
  (package-refresh-contents))

;; BASIC CUSTOMIZATION
;; --------------------------------------
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

(set-language-environment "UTF-8")
;; set mark
(global-set-key (kbd "C-.") 'set-mark-command)

(require 'rainbow-delimiters)

(require 'direnv)
(direnv-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(set-frame-font "Menlo 15" nil t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; PYTHON
;; --------------------------------------
;; Enable elpy
(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
;; (require 'py-autopep8)
;; (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; MARKDOWN
;; --------------------------------------
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

;; JSON
;; --------------------------------------
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook 'flycheck-mode)

;; JS
;; --------------------------------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

;; HTML
;; --------------------------------------
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

;; FLYSPELL
;; --------------------------------------
(global-unset-key (kbd "C-SPC"))
(define-key key-translation-map (kbd "C-.") (kbd "C-@"))

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; acewindow
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
