;;; init --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(load-file (expand-file-name
            (cond ((eq system-type 'darwin) "darwin.el")
                  ((eq system-type 'gnu/linux) "linux.el"))
            user-emacs-directory))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package tramp
  :straight (:build t :pre-build (("make" "autoloads")))
  :config
  (setq tramp-default-method "ssh")
  (customize-set-variable 'tramp-ssh-controlmaster-options
                          (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                  "-o ControlMaster=auto "
                                  "-o ControlPersist=yes ")))

(use-package emacs
  :straight nil

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
  (use-short-answers t)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  (ring-bell-function 'ignore)
  (require-final-newline t)
  (create-lockfiles nil)
  (native-comp-async-report-warnings-errors 'silent)

  (scroll-margin 0)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)

  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)

  (dired-listing-switches "-alh")

  (vc-handled-backends '(Git))
  (vc-make-backup-files t)
  (version-control t)
  (backup-by-copying t)
  (kept-new-versions 10)
  (kept-old-versions 0) (delete-old-versions t)
  (backup-directory-alist '(("." . "~/.emacs.d/backups/")))

  (indent-tabs-mode nil)
  (tab-width 4)
  (js-indent-level 2))

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

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
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
  (setq consult-preview-key nil)
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
  (defun vterm-toggle ()
    (interactive)
    (if (eq major-mode 'vterm-mode)
        (previous-buffer)
      (vterm)))
  :general ("C-`" 'vterm-toggle))

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-highlight-first nil)
  (custom-set-faces `(avy-background-face ((t (:foreground ,nord3))))
                    `(avy-lead-face ((t (:foreground ,nord8 :background ,nord0))))
                    `(avy-lead-face-0 ((t (:foreground ,nord8 :background ,nord0)))))
  :general (leader "j" 'avy-goto-char-timer))

(use-package eglot)

(use-package flymake
  :config
  (custom-set-faces `(flymake-note ((t (:underline (:style wave, :color ,nord14)))))
                    `(flymake-warning ((t (:underline (:style wave, :color ,nord13)))))
                    `(flymake-error ((t (:underline (:style wave, :color ,nord11)))))))

(use-package corfu
  :custom-face
  (corfu-default ((t (:background ,nord1))))
  (corfu-current ((t (:background ,nord3))))
  :config
  (global-corfu-mode))

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

(use-package python-mode
  :straight nil
  :general (:keymaps 'python-mode-map
                     "C-c f" 'black-format-buffer))

(use-package rust-mode
  :general (:keymaps 'rust-mode-map
                     "C-c C-f" nil
                     "C-c c" 'rust-compile
                     "C-c f" 'rust-format
                     "C-c r" 'rust-run
                     "C-c t" 'rust-test))

(use-package go-mode
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'eglot-format-buffer t t)
                     (add-hook 'before-save-hook #'eglot-code-action-organize-imports t t)))
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
