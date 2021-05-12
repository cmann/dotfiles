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
(setq require-final-newline t)
(setq create-lockfiles nil)
(setq vc-make-backup-files t
      version-control t
      backup-by-copying t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-default js-indent-level 2)

(add-to-list 'completion-styles 'flex)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'grep)
(grep-apply-setting 'grep-find-command
                    '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

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

(use-package delight)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer leader
    :states '(normal visual)
    :prefix "SPC"))

(use-package nord-theme
  :init
  (defvar nord0 "#2E3440")
  (defvar nord1 "#3B4252")
  (defvar nord2 "#434C5E")
  (defvar nord3 "#4C566A")
  (defvar nord4 "#D8DEE9")
  (defvar nord5 "#E5E9F0")
  (defvar nord6 "#ECEFF4")
  (defvar nord7 "#8FBCBB")
  (defvar nord8 "#88C0D0")
  (defvar nord9 "#81A1C1")
  (defvar nord10 "#5E81AC")
  (defvar nord11 "#BF616A")
  (defvar nord12 "#D08770")
  (defvar nord13 "#EBCB8B")
  (defvar nord14 "#A3BE8C")
  (defvar nord15 "#B48EAD")
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'nord t)))
    (load-theme 'nord t)))

(use-package compile
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'first-error
        compilation-read-command nil
        compilation-always-kill t)
  (custom-set-faces `(compilation-mode-line-exit ((t (:foreground ,nord14)))))
  (custom-set-faces `(compilation-mode-line-fail ((t (:foreground ,nord11))))))

(use-package project
  :pin gnu
  :general (leader "p" '(:keymap project-prefix-map)))

(use-package flymake
  :config
  ;; (set-face-attribute 'flymake-error nil :underline '(:color ,nord11 :style wave))
  ;; (set-face-attribute 'flymake-warning nil :underline '(:color ,nord13 :style wave))
  ;; (set-face-attribute 'flymake-note nil :underline '(:color ,nord14 :style wave))
  :general (general-nmap "[e" 'flymake-goto-prev-error
                         "]e" 'flymake-goto-next-error))

(use-package org
  :config
  (setq org-directory "~/org"
        org-agenda-files '("~/org/inbox.org"
                           "~/org/projects.org"
                           "~/org/reminders.org")
        org-refile-targets '(("~/org/projects.org" :maxlevel . 3)
                             ("~/org/someday.org" :level . 1)
                             ("~/org/reminders.org" :maxlevel . 2))
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-capture-templates '(("t" "Todo" entry
                                 (file+headline "~/org/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("r" "Reminder" entry
                                 (file+headline "~/org/reminders.org" "Reminders")
                                 "* %i%? \n %U")))
  :general
  (leader
    "a" 'org-agenda
    "c" 'org-capture))

(use-package org-roam
  :delight
  :hook (after-init . org-roam-mode)
  :custom (org-roam-directory (file-truename "~/org/roam/"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode)
  :config (custom-set-faces `(highlight-numbers-number ((t (:foreground ,nord15))))))

(use-package which-key
  :delight
  :config (which-key-mode))

(use-package evil
  :init (setq evil-want-Y-yank-to-eol t)
  :hook ((evil-visual-state-entry . (lambda() (hl-line-mode -1)))
         (evil-visual-state-exit  . (lambda() (hl-line-mode +1))))
  :config
  (evil-mode)
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
  :after evil
  :delight
  :config
  (setq undo-tree-history-directory-alist `((".*" . "~/emacs.d/undo"))
        undo-tree-auto-save-history t)
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode))

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
              (if (string= "*vterm*" (buffer-name))
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

;; (use-package flycheck)
(use-package lsp-mode
  :hook ((javascript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-completion-provider :capf)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'"))
(use-package lsp-ivy)
(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package magit
  :general (leader "m" 'magit-file-dispatch))

(use-package yasnippet
  :delight yas-minor-mode
  :config (yas-global-mode))
(use-package yasnippet-snippets)

(use-package ws-butler
  :delight
  :hook (prog-mode . ws-butler-mode))

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

(use-package yaml-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
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
