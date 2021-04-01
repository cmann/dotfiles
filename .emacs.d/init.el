;;; init --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode)
(global-auto-revert-mode)
(electric-pair-mode)
(show-paren-mode)
(recentf-mode)

(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
(setq hl-line-sticky-flag nil)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq org-directory "~/org")
(setq mode-require-final-newline t)
(setq create-lockfiles nil)
(setq vc-make-backup-files t
      version-control t
      backup-by-copying t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-directory-alist '(("." . "~/.emacs.d/backups/")))

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-default js-indent-level 2)

(add-to-list 'completion-styles 'flex)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'grep)
(grep-apply-setting 'grep-find-command
                    '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

(setq compilation-read-command nil
      compilation-always-kill t)

(setq vc-handled-backends '(Git))
(setq tramp-default-method "ssh")
(customize-set-variable 'tramp-ssh-controlmaster-options
                        (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                "-o ControlMaster=auto "
                                "-o ControlPersist=yes "))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package nord-theme
  :init (setq nord0 "#2E3440"
              nord1 "#3B4252"
              nord2 "#434C5E"
              nord3 "#4C566A"
              nord4 "#D8DEE9"
              nord5 "#E5E9F0"
              nord6 "#ECEFF4"
              nord7 "#8FBCBB"
              nord8 "#88C0D0"
              nord9 "#81A1C1"
              nord10 "#5E81AC"
              nord11 "#BF616A"
              nord12 "#D08770"
              nord13 "#EBCB8B"
              nord14 "#A3BE8C"
              nord15 "#B48EAD")
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'nord t)))
    (load-theme 'nord t))
  (custom-set-faces `(compilation-mode-line-exit ((t (:foreground ,nord14)))))
  (custom-set-faces `(compilation-mode-line-fail ((t (:foreground ,nord11))))))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode)
  :config (custom-set-faces `(highlight-numbers-number ((t (:foreground ,nord15))))))

(use-package delight)

(use-package which-key
  :delight
  :config (which-key-mode))

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer leader
    :states '(normal visual)
    :prefix "SPC"))

(use-package evil
  :init (setq evil-want-Y-yank-to-eol t)
  :hook ((evil-visual-state-entry . (lambda() (hl-line-mode -1)))
         (evil-visual-state-exit  . (lambda() (hl-line-mode +1))))
  :config (evil-mode)
  :general
  (leader
    "SPC" 'execute-extended-command
    "f" 'find-file
    "b" 'switch-to-buffer
    "k" 'kill-buffer-and-window
    "e" 'eval-buffer
    "g" 'grep-find
    "i" 'counsel-imenu
    "o" 'other-window
    "1" 'delete-other-windows
    "2" (lambda () (interactive) (split-window-below) (other-window 1))
    "3" (lambda () (interactive) (split-window-right) (other-window 1))
    "0" 'delete-window)
  (general-nmap
    "]q" 'next-error
    "[q" 'previous-error))

(use-package evil-surround
  :config (global-evil-surround-mode))
(use-package evil-commentary
  :delight
  :config (evil-commentary-mode))
(use-package evil-lion
  :config (evil-lion-mode))
(use-package undo-tree
  :delight)

(use-package flyspell
  :delight
  :hook ((text-mode . flyspell-mode)
         (org-mode  . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (custom-set-faces `(flyspell-incorrect ((t (:weight bold :underline (:color ,nord11 :style wave))))))
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-prog-text-faces (delq 'font-lock-string-face flyspell-prog-text-faces)))

(use-package counsel
  :delight ivy-mode
  :init (setq ivy-use-virtual-buffers t)
  :config
  (require 'ivy)
  (ivy-mode))
(use-package amx
  :config
  (setq amx-show-key-bindings nil)
  (amx-mode))

(use-package vterm
  :hook ('vterm-mode . (lambda () (hl-line-mode -1)))
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (setq vterm-shell "bash"
        vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t)
  (custom-set-faces `(vterm-color-default ((t (:foreground ,nord4  :background ,nord0))))
                    `(vterm-color-black   ((t (:foreground ,nord1  :background ,nord3))))
                    `(vterm-color-red     ((t (:foreground ,nord11 :background ,nord11))))
                    `(vterm-color-green   ((t (:foreground ,nord14 :background ,nord14))))
                    `(vterm-color-yellow  ((t (:foreground ,nord13 :background ,nord13))))
                    `(vterm-color-blue    ((t (:foreground ,nord9  :background ,nord9))))
                    `(vterm-color-magenta ((t (:foreground ,nord15 :background ,nord15))))
                    `(vterm-color-cyan    ((t (:foreground ,nord8  :background ,nord7))))
                    `(vterm-color-white   ((t (:foreground ,nord5  :background ,nord6)))))
  (defun visit-vterm ()
    (interactive)
    (let ((term-buffer (get-buffer "vterm")))
      (if (eq major-mode 'vterm-mode)
          (if (term-check-proc (buffer-name))
              (if (string= "vterm" (buffer-name))
                  (previous-buffer)
                (if term-buffer
                    (switch-to-buffer "vterm")
                  (vterm)))
            (kill-buffer (buffer-name))
            (vterm))
        (if term-buffer
            (if (term-check-proc "vterm")
                (switch-to-buffer "vterm")
              (kill-buffer "vterm")
              (vterm))
          (vterm)))))
  :general ("C-`" 'visit-vterm))

(use-package ace-jump-mode
  :config
  (custom-set-faces `(ace-jump-face-background ((t (:foreground ,nord3))))
                    `(ace-jump-face-foreground ((t (:foreground ,nord8)))))
  (setq ace-jump-mode-scope 'global)
  :general (leader "j" 'ace-jump-word-mode))

(use-package company
  :delight
  :config
  (setq company-tooltip-maximum-width 80)
  (global-company-mode))

(use-package projectile
  :delight
  :config
  (setq projectile-completion-system 'ivy
        projectile-project-search-path '("~/devel"))
  (projectile-mode)
  :general
  (leader "p" '(:keymap projectile-command-map))
  (:keymaps 'prog-mode-map
            "C-c c" 'projectile-compile-project
            "C-c t" 'projectile-test-project
            "C-c r" 'projectile-run-project))

(use-package flycheck)
(use-package lsp-mode
  :hook ((prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-completion-provider :capf))
(use-package lsp-ivy)

(use-package magit
  :general (leader "m" 'magit-file-dispatch))

(use-package yasnippet
  :delight yas-minor-mode
  :config (yas-global-mode))
(use-package yasnippet-snippets)

(use-package sh-script
  :mode ("\\.bashrc\\'" . sh-mode)
  :config (custom-set-faces `(sh-heredoc ((t (:foreground ,nord14))))))

(use-package rust-mode
  :general (:keymaps 'rust-mode-map
                     "C-c C-f" nil
                     "C-c c" 'rust-compile
                     "C-c f" 'rust-format
                     "C-c r" 'rust-run
                     "C-c t" 'rust-test))

(use-package go-mode
  :hook (go-mode . (lambda () (add-hook 'before-save-hook 'gofmt-before-save)))
  :general (:keymaps 'go-mode-map
                     "C-c f" 'gofmt))

(use-package terraform-mode
  :general (:keymaps 'terraform-mode-map
                     "C-c f" 'terraform-format-buffer))

(use-package zig-mode
  :config
  (require 'lsp)
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "zls")
    :major-modes '(zig-mode)
    :server-id 'zls))
  :general (:keymaps 'zig-mode-map
                     "C-c f" 'zig-format-buffer))

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package yaml-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package protobuf-mode)
(use-package typescript-mode)
(use-package php-mode)

(defun buffer-local-file-name ()
  (if (file-remote-p buffer-file-name)
      (tramp-file-name-localname (tramp-dissect-file-name buffer-file-name))
    (buffer-file-name)))

(defun black-format-buffer ()
  (interactive)
  (shell-command (concat "black " (buffer-local-file-name))))

(general-def python-mode-map
  "C-c f" 'black-format-buffer)

(provide 'init.el)
;;; init.el ends here
