;;; init --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))
(elpaca-wait)
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer leader
    :states '(normal visual)
    :prefix "SPC"))
(elpaca-wait)

(use-package emacs
  :ensure nil

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
  (special-mode . visual-line-mode)

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
  (inhibit-startup-screen t)
  (use-short-answers t)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  (ring-bell-function 'ignore)
  (require-final-newline t)
  (create-lockfiles nil)
  (native-comp-async-report-warnings-errors 'silent)
  (use-package-enable-imenu-support t)
  (eldoc-echo-area-prefer-doc-buffer t)

  (scroll-margin 3)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position t)
  (line-spacing 0.1)

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

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                           "-o ControlMaster=auto "
                                           "-o ControlPersist=yes ")))

(use-package compile
  :ensure nil
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns pgtk x))
            (exec-path-from-shell-initialize)))

(use-package delight)

(use-package project
  :ensure nil
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

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package evil
  :hook ((evil-visual-state-entry . (lambda() (global-hl-line-mode -1)))
         (evil-visual-state-exit  . (lambda() (global-hl-line-mode +1))))
  :config
  (customize-set-variable 'evil-normal-state-modes '(fundamental-mode text-mode prog-mode conf-mode))
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (evil-mode)
  :custom
  (evil-default-state 'emacs)
  (evil-motion-state-modes nil)
  (evil-insert-state-modes nil)
  (evil-undo-system 'undo-redo)
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

(use-package eat
  :ensure
  (eat :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el")))
  :hook
  ('eat-mode . (lambda ()
                 (setq-local global-hl-line-mode nil)
                 (setq-local left-margin-width 0)
                 (setq-local scroll-margin 0)))
  :config
  (defun project-eat-toggle (arg)
    (interactive "P")
    (if (eq major-mode 'eat-mode)
        (previous-buffer)
      (eat-project arg)))
  :custom
  (eat-kill-buffer-on-exit t)
  :general
  ("C-`" 'project-eat-toggle))

(use-package dape
  :config
  (add-to-list 'dape-configs
               `(skaffold-go
                 modes (go-mode go-ts-mode)
                 host "127.0.0.1"
                 port 56268
                 :type "go"
                 :request "attach"
                 :mode "remote"
                 :substitutePath [(:from dape-cwd :to "")]
                 :cwd dape-cwd))
  :custom
  (dape-debug t)
  (dape-buffer-window-arrangement 'right))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '(typescript-mode . ("npx" "typescript-language-server" "--stdio")))
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package flymake-eslint
  :hook
  (eglot-managed-mode . (lambda ()
                          (when (derived-mode-p 'typescript-mode 'js-mode)
                            (flymake-eslint-enable)))))

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

(use-package editorconfig
  :delight
  :config (editorconfig-mode 1))

(use-package sh-script
  :ensure nil
  :mode ("\\.bashrc\\'" . sh-mode))

(use-package python
  :ensure nil
  :config
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
  :general
  (:keymaps 'python-mode-map
            "C-c f" 'black-format-buffer))

(use-package sql
  :ensure nil
  :config (sql-set-product 'postgres))

(use-package rust-mode
  :general (:keymaps 'rust-mode-map
                     "C-c C-f" nil
                     "C-c c" 'rust-compile
                     "C-c f" 'rust-format-buffer
                     "C-c r" 'rust-run
                     "C-c t" 'rust-test))

(use-package go-mode
  :hook
  (go-mode . (lambda ()
               (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  :custom
  (gofmt-command "goimports"))

(use-package gotest
  :config
  (defun go-test-dwim ()
    (interactive)
    (condition-case nil
        (go-test-current-test)
      (error (go-test-current-file))))
  :general
  (:keymaps 'go-mode-map
            "C-c t" 'go-test-dwim))

(use-package terraform-mode
  :general (:keymaps 'terraform-mode-map
                     "C-c f" 'terraform-format-buffer))

(use-package zig-mode
  :general (:keymaps 'zig-mode-map
                     "C-c f" 'zig-format-buffer))

(use-package graphql-mode
  :after editorconfig
  :init
  (add-to-list 'editorconfig-indentation-alist '(graphql-mode . graphql-indent-level)))

(use-package typescript-mode)
(use-package markdown-mode)
(use-package markdown-preview-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package protobuf-mode)
(use-package bazel)
(use-package pyvenv)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(provide 'init.el)
;;; init.el ends here
