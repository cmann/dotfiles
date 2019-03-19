;;; init --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Hide GUI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Highlight current line
(global-hl-line-mode)

;; Show column number
(column-number-mode)

;; Load customization
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

(defun reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Setup MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Themes
(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (set-face-background 'vertical-border "#c5c8c6")
  (set-face-foreground 'vertical-border "#c5c8c6")
  (set-face-background 'hl-line "#282a2e")
  (set-face-background 'mode-line "#282a2e")
  (set-cursor-color "#c5c8c6"))

;; Status line
(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-percent-position "")
  :hook (after-init . doom-modeline-mode))

;; Keybinding
(use-package general
  :functions leader
  :config
  (general-create-definer leader
    :states '(normal insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :prefix-command 'leader-command
    :prefix-map 'leader-map)
  (leader
    "f" 'find-file
    "b" 'switch-to-buffer
    "r" 'reload-config))

;; Show available keys
(use-package which-key
  :config
  (which-key-mode))

;; Vim emulation
(use-package evil
  :config
  (evil-mode))
(use-package evil-surround
  :config
  (global-evil-surround-mode))
(use-package evil-commentary
  :config
  (evil-commentary-mode))
(use-package undo-tree)

;; Automatic parens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode))
(use-package evil-cleverparens
  :config
  (evil-cleverparens-mode))

;; Incremental completion
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))
(use-package counsel
  :general
  (leader
    "SPC" 'counsel-M-x))
(use-package swiper)

;; Project interaction
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/repos"))
  (projectile-discover-projects-in-search-path)
  (projectile-mode)
  :general
  (leader
    "p" projectile-command-map))

;; Git wrapper
(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :general
  (leader
    "m" 'magit-status))

;; Linting
(use-package flycheck
  :init (global-flycheck-mode)
  :hook
  (python-mode-hook . (lambda ()
                        (setq flycheck-python-pylint-executable "~/.local/bin/pylint"))))

;; (use-package lsp-mode
;;   :commands lsp
;;   :hook (prog-mode . lsp))

(use-package virtualenvwrapper)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package ein)

(provide 'init.el)
;;; init.el ends here
