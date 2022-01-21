;;; init --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(load-file (expand-file-name
            (cond ((eq system-type 'darwin) "darwin.el")
                  ((eq system-type 'gnu/linux) "linux.el"))
            user-emacs-directory))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
(add-hook 'dired-mode-hook 'auto-revert-mode)
(global-hl-line-mode)
(add-hook 'comint-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

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

(setq vc-handled-backends '(Git))
(setq tramp-default-method "ssh")
(customize-set-variable 'tramp-ssh-controlmaster-options
                        (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                "-o ControlMaster=auto "
                                "-o ControlPersist=yes "))

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

(use-package delight)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer leader
    :states '(normal visual)
    :prefix "SPC"))

(use-package compile
  :straight nil
  :config
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'first-error
        compilation-read-command nil
        compilation-always-kill t)
  (custom-set-faces
   `(compilation-mode-line-exit ((t (:foreground ,nord14))))
   `(compilation-mode-line-fail ((t (:foreground ,nord11))))))

(use-package project
  :general (leader "p" '(:keymap project-prefix-map)))

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
  :hook (after-init . org-roam-setup)
  :init (setq org-roam-v2-ack t)
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
  :hook ((evil-visual-state-entry . (lambda() (global-hl-line-mode -1)))
         (evil-visual-state-exit  . (lambda() (global-hl-line-mode +1))))
  :config
  (setq evil-motion-state-modes nil)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-mode)
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
  (setq undo-tree-history-directory-alist `((".*" . "~/.emacs.d/undo"))
        undo-tree-auto-save-history t)
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode))

(use-package selectrum
  :config (selectrum-mode +1))
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :config
  (consult-customize
   consult-buffer :group nil)
  (setq consult-preview-key nil)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project))))
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
  :config
  (defun cm--embark-export-other-window (orig-fun &rest args)
    (when (= (count-windows) 1)
      (split-window-right))
    (let ((res (apply orig-fun args)))
      (other-window 1)
      res))
  (advice-add 'embark-consult-export-grep :around #'cm--embark-export-other-window)
  :general (minibuffer-local-map "C-e" 'embark-export))
(use-package embark-consult
  :after (embark consult)
  :demand t
  :config
  (add-to-list 'embark-exporters-alist
               '(consult-git-grep . embark-consult-export-grep)
               '(consult-ripgrep . embark-consult-export-grep)))

(use-package vterm
  :hook ('vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
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

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-highlight-first nil)
  (custom-set-faces `(avy-background-face ((t (:foreground ,nord3))))
                    `(avy-lead-face ((t (:foreground ,nord8 :background ,nord0))))
                    `(avy-lead-face-0 ((t (:foreground ,nord8 :background ,nord0)))))
  :general (leader "j" 'avy-goto-word-1))

(use-package company
  :delight
  :config
  (setq company-tooltip-maximum-width 80)
  (global-company-mode))

(use-package flycheck
  :hook (sh-mode ruby-mode)
  :config
  (setq flycheck-display-errors-delay 0.5
        flycheck-shellcheck-follow-sources nil)
  (customize-set-variable
   'flycheck-shellcheck-excluded-warnings '("SC1090")))

(use-package lsp-mode
  :hook ((js-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-completion-provider :capf)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'"))
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config (setq lsp-pyright-typechecking-mode "off"))

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
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'lsp-format-buffer t t)
                     (add-hook 'before-save-hook #'lsp-organize-imports t t)))
  :config
  (setq gofmt-command "goimports")
  :general (:keymaps 'go-mode-map
                     "C-c f" 'gofmt))

(use-package terraform-mode
  :general (:keymaps 'terraform-mode-map
                     "C-c f" 'terraform-format-buffer))

(use-package zig-mode
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
