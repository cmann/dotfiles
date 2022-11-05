;;; init --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package tramp
  :straight (:build t :pre-build (("make" "autoloads")))
  :custom
  (tramp-default-method "ssh")
  (tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                           "-o ControlMaster=auto "
                                           "-o ControlPersist=yes ")))

(use-package emacs
  :straight nil

  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'nord t)))
    (load-theme 'nord t))
  (load-file (expand-file-name
              (cond ((eq system-type 'darwin) "darwin.el")
                    ((eq system-type 'gnu/linux) "linux.el"))
              user-emacs-directory))

  :hook
  (dired-mode-hook . auto-revert-mode)
  (comint-mode-hook . (lambda () (setq-local global-hl-line-mode nil)))

  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode)
  (electric-pair-mode)
  (show-paren-mode)
  (recentf-mode)
  (savehist-mode)
  (global-auto-revert-mode)
  (global-hl-line-mode)

  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (custom-theme-directory (expand-file-name "themes" user-emacs-directory))
  (use-short-answers t)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  (ring-bell-function 'ignore)
  (require-final-newline t)
  (create-lockfiles nil)
  (native-comp-async-report-warnings-errors 'silent)

  (scroll-margin 3)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position t)

  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)

  (dired-listing-switches "-alh")

  (vc-handled-backends '(Git))
  (vc-make-backup-files t)
  (vc-follow-symlinks t)
  (version-control t)
  (backup-by-copying t)
  (kept-new-versions 10)
  (kept-old-versions 0)
  (delete-old-versions t)
  (backup-directory-alist '(("." . "~/.emacs.d/backups/")))

  (indent-tabs-mode nil)
  (tab-width 4)
  (js-indent-level 2))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns pgtk x))
            (exec-path-from-shell-initialize)))

(use-package delight)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer leader
    :states '(normal visual)
    :prefix "SPC"))

(use-package compile
  :straight nil
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compilation-read-command nil)
  (compilation-always-kill t))

(use-package project
  :general (leader "p" '(:keymap project-prefix-map)))

(use-package org
  :custom
  (org-directory "~/org")
  (org-agenda-files '("~/org/inbox.org"
                      "~/org/projects.org"
                      "~/org/reminders.org"))
  (org-refile-targets '(("~/org/projects.org" :maxlevel . 3)
                        ("~/org/someday.org" :level . 1)
                        ("~/org/reminders.org" :maxlevel . 2)))
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-capture-templates '(("t" "Todo" entry
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
  :config (org-roam-db-autosync-mode)
  :custom (org-roam-directory (file-truename "~/org/roam/")))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package which-key
  :delight
  :config (which-key-mode))

(use-package evil
  :init (customize-set-variable 'evil-want-Y-yank-to-eol t)
  :hook ((evil-visual-state-entry . (lambda() (global-hl-line-mode -1)))
         (evil-visual-state-exit  . (lambda() (global-hl-line-mode +1))))
  :config
  (dolist (mode '(help-mode
                  compilation-mode
                  dired-mode
                  xref--xref-buffer-mode))
    (evil-set-initial-state mode 'emacs))
  (evil-mode)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-motion-state-modes nil)
  :general
  (leader
    "SPC" 'execute-extended-command
    "f" 'find-file
    "k" 'kill-buffer-and-window
    "e" 'eval-buffer
    "o" 'other-window
    "1" 'delete-other-windows
    "2" (lambda () (interactive) (split-window-below) (other-window 1))
    "3" (lambda () (interactive) (split-window-right) (other-window 1))
    "4" '(:keymap ctl-x-4-map)
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

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :config
  (consult-customize
   consult-buffer :group nil)
  :custom
  (consult-preview-key nil)
  :general
  (search-map
   "o" 'consult-outline
   "i" 'consult-imenu
   "p" 'consult-project-imenu
   "l" 'consult-line
   "g" 'consult-grep
   "r" 'consult-ripgrep
   "G" 'consult-git-grep
   "f" 'consult-find
   "L" 'consult-locate)
  (leader
    "b" 'consult-buffer
    "i" 'consult-imenu
    "g" 'consult-ripgrep
    "s" '(:keymap search-map)))

(use-package embark
  :general
  (general-override-mode-map
   :states '(normal emacs)
   "C-." 'embark-act
   "M-." 'embark-dwim)
  (minibuffer-local-map
   "C-." 'embark-act
   "M-." 'embark-dwim))

(use-package embark-consult
  :after (embark consult))

(use-package vterm
  :hook ('vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (defun project-vterm-toggle ()
    (interactive)
    (require 'comint)
    (if (eq major-mode 'vterm-mode)
        (previous-buffer)
      (let* ((default-directory (project-root (project-current t)))
             (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
             (vterm-buffer (get-buffer default-project-vterm-name)))
        (if (and vterm-buffer (not current-prefix-arg))
            (if (comint-check-proc vterm-buffer)
                (switch-to-buffer vterm-buffer)
              (let ((current-prefix-arg 'vterm-buffer))
                (call-interactively 'vterm)))
          (let ((current-prefix-arg (generate-new-buffer-name default-project-vterm-name)))
            (call-interactively 'vterm))))))
  :custom
  (vterm-shell "bash")
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  :general
  ("C-`" 'project-vterm-toggle))

(use-package avy
  :custom
  (avy-background t)
  (avy-highlight-first nil)
  :general
  (leader "j" 'avy-goto-char-timer))

(use-package eglot)

(use-package flymake)

(use-package corfu
  :init (global-corfu-mode)
  :general (:keymaps 'corfu-map
                     "SPC" 'corfu-insert-separator))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package magit
  :general (leader "m" 'magit-file-dispatch))
(use-package forge)

(use-package yasnippet
  :delight yas-minor-mode
  :config (yas-global-mode))
(use-package yasnippet-snippets)

(use-package ws-butler
  :delight
  :hook (prog-mode . ws-butler-mode))

(use-package sh-script
  :mode ("\\.bashrc\\'" . sh-mode))

(use-package python-mode
  :straight nil
  :general (:keymaps 'python-mode-map
                     "C-c f" 'black-format-buffer))

(use-package rust-mode
  :general (:keymaps 'rust-mode-map
                     "C-c C-f" nil
                     "C-c c" 'rust-compile
                     "C-c f" 'rust-format-buffer
                     "C-c r" 'rust-run
                     "C-c t" 'rust-test))

(use-package go-mode
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'eglot-format-buffer t t)
                     (add-hook 'before-save-hook #'eglot-code-action-organize-imports t t)))
  :custom
  (gofmt-command "goimports")
  :general (:keymaps 'go-mode-map
                     "C-c f" 'gofmt))

(use-package terraform-mode
  :general (:keymaps 'terraform-mode-map
                     "C-c f" 'terraform-format-buffer))

(use-package zig-mode
  :general (:keymaps 'zig-mode-map
                     "C-c f" 'zig-format-buffer))

(use-package sql
  :config (sql-set-product 'postgres))

(use-package yaml-mode)
(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package typescript-mode)
(use-package php-mode)
(use-package pyvenv)

(defun black-format-buffer ()
  "Formats the buffer using `black'"
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "black -"
   (current-buffer)
   t
   "*Black Error Buffer*"
   t))

(provide 'init.el)
;;; init.el ends here
