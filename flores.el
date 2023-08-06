;; Configure package repositories.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package and configure to "always ensure"
;; (always install if not present).
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; In all modes, delete trailing whitespaces before saving.
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Defaults.
(setq-default fill-column 80)
(setq-default inhibit-startup-message t)
(setq-default visible-bell nil)

;; GUI.
(load-theme 'wombat)

;; Disable some things.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; An integer is the pixel width.
(set-fringe-mode 10)

;; Put all backup files here (those that end on a ~).
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Arrows movement between different windows with shift.
(windmove-default-keybindings)

;; Enable line numbers on the right.
(global-display-line-numbers-mode t)

;; Show column number at the bottom next to the line number.
(column-number-mode t)

;; Package git-gutter.el
;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :hook
  (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2))

;; Here are some ideas for a different configuration using
;; package git-gutter-fringe.el
;; https://github.com/emacsorphanage/git-gutter-fringe
;(use-package git-gutter-fringe
;  :config
;  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
; (global-git-gutter-mode +1)

(global-display-fill-column-indicator-mode t)

;; Enable auto-fill in org-mode.
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Evalute code without asking for confirmation.
(set 'org-confirm-babel-evaluate nil)

;; Enable babel languages in orgmode.
(org-babel-do-load-languages
 'org-babel-load-languages '((ocaml . t)))

;; Make ESC quit prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Add :diminish to keep minor modes out of the mode line.
(use-package diminish)

;; Auto-completion.
(use-package counsel
  :bind (
	;; ("C-x b" . counsel-ibuffer))
	))
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Shortcut to switch between two buffers.
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

;; Prescient to sort auto-completion by recency.
(use-package ivy-prescient
  :diminish
  :config
  (ivy-prescient-mode 1))

;; Projects.
(use-package projectile
  :diminish
  :config
  (projectile-mode 1)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/devel")
    (setq projectile-project-search-path '("~/devel")))
  ; Open project in dired
  (setq projectile-switch-project-action 'projectile-dired))

;; Search: grep
(use-package ripgrep)

;; Git
(use-package magit
  :custom
  ; By default, Magit opens status in a new window.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode)
  (display-line-numbers-mode 0))

(defun org-mode-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

;; Orgmode
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (org-mode-font-setup)
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files
	'("~/devel/tasks.org")))

;; Replace stars with utf-8 chars.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Center text.
(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; StandardML
(use-package sml-mode)

;; Enable the language server protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (tuareg-mode . lsp-deferred))

;; Show messages on the right-side margin
(use-package lsp-ui
  :hook (tuareg-mode . lsp-ui-sideline-mode))

;; OCaml
(use-package ocamlformat
  :hook (before-save . ocamlformat-before-save))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Refresh buffers automatically
(global-auto-revert-mode 1)

(savehist-mode 1)
(save-place-mode 1)

(use-package pdf-tools
      :config
     (pdf-tools-install))

(use-package geiser)
(use-package geiser-guile)
