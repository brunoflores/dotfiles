(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Add :diminish to keep minor modes out of the mode line.
(use-package diminish)

;; In all modes, delete trailing whitespaces before saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; In all prog modes, display line numbers on the left margin.
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

;; Keep point at the same screen position when scrolling up and down pages.
(setq-default scroll-preserve-screen-position t)

;; Regardless of whether auto-fill is enabled, we always have 80 columns.
(setq-default fill-column 80)
(setq-default inhibit-startup-message t)
(setq-default visible-bell nil) ; Disable visible bell

;; Put all backup files here (those that end on a ~).
(setq-default backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Use the language's major-mode binding in code blocks.
(setq-default org-src-tab-acts-natively t)

;; Smoother scrolling from
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq-default scroll-step 1
	      scroll-conservatively 10000)

;(setq find-file-visit-truename t)

(winner-mode +1)
;(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
;(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (c "https://github.com/tree-sitter/tree-sitter-c")))

(setq-default indent-tabs-mode nil)

(setq-default next-error-message-highlight t)
(setq-default compilation-window-height 20)

(load-theme 'wombat t)
(set-fringe-style 10) ; Set a fringe of 10 pixels on both sides.

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Disable line numbers on the left by default.
;; We enable them for all prog modes below.
(global-display-line-numbers-mode -1)

;; Show column number at the bottom next to the line number.
(column-number-mode t)

;; Refresh buffers automatically.
(global-auto-revert-mode t)

;; Save minibuffer history.
(savehist-mode t)

;; Save place in each file.
(save-place-mode t)

;; Break lines at right margin.
(auto-fill-mode)

(use-package which-key
  :diminish
  :init
  (which-key-mode))

(use-package rainbow-mode)

;; (setq load-path (cons (expand-file-name "~/devel/ott/emacs") load-path))
;; (require 'ott-mode)

;; (load "~/devel/HOL/tools/hol-mode")
;; (load "~/devel/HOL/tools/hol-unicode")

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Make ESC quit prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Shortcut to switch between two buffers.
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

;; Reload our ~/.emacs.
(global-set-key (kbd "C-c <f12>") 'reload-dotemacs)

(windmove-default-keybindings)

;; Automatically tangle our emacs.org config file when we save it.
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/devel/dotfiles/emacs/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'after-save-hook
                                     'org-babel-tangle-config)))

(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))

(use-package company
  ;; Enable company-mode in all buffers.
  :hook (after-init-hook . global-company-mode))

(use-package ivy
  :disabled
  :diminish
  :bind (("C-s" . swiper)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer))
  :config
  ;; Always enabled.
  (ivy-mode 1)
  :init
  ;; Add files and bookmarks to switch-buffer prompt.
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Prescient to sort auto-completion by recency.
(use-package ivy-prescient
  :disabled
  :diminish
  :config
  (ivy-prescient-mode 1))

(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :init
  (recentf-mode) ; Enables tracking of recent files
  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-s" . consult-line) ; Search in current buffer
         ("s-s" . consult-line-multi) ; Seach across project buffers
         ("s-f" . consult-find) ; Find file in project
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-y" . consult-yank-from-kill-ring)))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package projectile
  :diminish projectile-mode
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1)
  (when (file-directory-p "~/devel")
    (setq projectile-project-search-path '("~/devel")))
  :config
  ;; Open project in dired
  (setq projectile-switch-project-action 'projectile-dired))

(use-package counsel-projectile
  :disabled
  :after projectile
  :config
  (counsel-projectile-mode))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package ripgrep)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :hook (;; Enable languages here:
	 (c-mode . lsp-deferred)
	 (tuareg-mode . lsp-deferred)
	 (shell-script-mode . lsp-deferred)))

(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; This uses Merlin internally
(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

;; Show messages on the right-side margin.
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook ((tuareg-mode . lsp-ui-sideline-mode)
	 (lsp-mode . lsp-ui-mode))
  :init
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-update-mode 'point)
  (setq lsp-ui-sideline-enable t))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :diminish
  :hook
  ;; Enable in all modes that inherit from prog-mode.
  (prog-mode . git-gutter-mode)
  :config
  ;; Interval in seconds.
  (setq git-gutter:update-interval 1))

;; https://github.com/emacsorphanage/git-gutter-fringe
(use-package git-gutter-fringe
  :config
  ;; Green
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  ;; Purple
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  ;; Red
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package magit
  :custom
  ; By default, Magit opens status in a new window.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package paredit
  :hook (lisp-mode . paredit-mode))

(use-package tuareg)

(use-package ocamlformat
  :hook (before-save . ocamlformat-before-save))

(use-package utop
  :config
  ;; Use the opam installed utop
  (setq utop-command "opam exec -- utop -emacs"))

;; OCaml + Make
(projectile-register-project-type 'ocaml-make '("dune-project" "Makefile")
                                  :project-file "dune-project"
                                  :compile "make -k")

(use-package fstar-mode)

(defun org-mode-setup ()
  (org-indent-mode)
  (diminish 'org-indent-mode))

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
    (set-face-attribute (car face) nil
                        :font "Cantarell" :weight 'regular
                        :height (cdr face))))

(use-package org
  :after
  (git-gutter)
  :hook
  (org-mode . org-mode-setup)
  (org-mode . git-gutter-mode)
  :custom
  (org-ellipsis " …")
  (org-hide-emphasis-markers t)

  ; Agenda:
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time) ; Use current time with completing a task
  (org-log-into-drawer t) ; Put properties in closed drawer
  ;(org-directory "~/devel/org-mode-my-files")
  (org-deadline-warning-days 2)
  ; Files to be used for agenda display:
  (org-agenda-files '("tasks.org"))

  :config
  (org-mode-font-setup)
  ;; Remove the default underline style from elipsis.
  (set-face-underline 'org-ellipsis nil))

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
  :hook
  (org-mode . org-mode-visual-fill)
  (markdown-mode . org-mode-visual-fill))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(setq-default org-image-actual-width nil)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/devel/org-roam-my-files/"))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "\n* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "~/devel/dotfiles/org-roam/template-book.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph)
         ("C-c n a a" . org-roam-alias-add)
         ("C-c n a d" . org-roam-alias-remove)
         ("C-c n t a" . org-roam-tag-add)
         ("C-c n t d" . org-roam-tag-remove)
         ;; Dailies
         ("C-c n d j" . org-roam-dailies-capture-today)
         ("C-c n d y" . org-roam-dailies-capture-yesterday)
         ("C-c n d t" . org-roam-dailies-capture-tomorrow)
         :map org-mode-map
         ("s-i" . completion-at-point))
  :config
  ;; Configure what sections are displayed in the Org-roam buffer:
  (setq org-roam-mode-sections
    (list #'org-roam-backlinks-section
#'org-roam-reflinks-section
#'org-roam-unlinked-references-section))
  ;; If you're using a vertical completion framework, you might want
  ;; a more informative completion interface.
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:30}" 'face 'org-tag)))
  ;; Setup Org-roam to run functions on file changes to maintain
  ;; cache consistency:
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package deft
  :after org-roam
  :bind
  ;("C-c d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase)))
  (deft-text-mode 'org-mode)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  (deft-auto-save-interval 300))

(use-package flyspell)

(use-package tex
  :ensure auctex)

;; Evalute Babel code without asking for confirmation.
(set 'org-confirm-babel-evaluate nil)

;; Package org-tempo allows me to create Babel blocks with
;; templates starting with "<".
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ocaml" . "src ocaml"))
(add-to-list 'org-structure-template-alist '("bash" . "src bash"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; Enable Babel languages.
(org-babel-do-load-languages
 'org-babel-load-languages '((ocaml . t)
                             (emacs-lisp . t)
                             (latex . t)
                             (js . t)))

(use-package sml-mode)

;; (use-package pdf-tools
;;   ;; :pin manual ;; manually update
;;   :config
;;   ;; initialise
;;   (pdf-tools-install)
;;   ;; open pdfs scaled to fit page
;;   (setq-default pdf-view-display-size 'fit-page)
;;   ;; automatically annotate highlights
;;   ;; (setq pdf-annot-activate-created-annotations t)
;;   ;; use normal isearch
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;   ;; turn off cua so copy works
;;   (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
;;   ;; more fine-grained zooming
;;   (setq pdf-view-resize-factor 1.1)
;;   ;; keyboard shortcuts
;;   (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
;;   (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
;;   (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

(use-package pdf-tools
  :mode
  (("\\.pdf$" . pdf-view-mode))

  :custom
  ;; pdf-annot-activate-created-annotations t ;; automatically annotate highlights
  pdf-view-resize-factor 1.1 ;; 10%

  :bind
  (:map pdf-view-mode-map
	;; normal isearch
	("C-s" . isearch-forward)
	;; custom keys
	("h" . pdf-annot-add-highlight-markup-annotation)
	("t" . pdf-annot-add-text-annotation)
	("D" . pdf-annot-delete))

  :hook
  ((pdf-view-mode) . (lambda () (cua-mode 0)))

  :config
  (pdf-tools-install)

  (setq-default pdf-view-display-size 'fit-page)
  (setq-default pdf-annot-color-history '("plum" "dark khaki" "coral" "cyan" "gold"
					  (highlight (color . "gold")) (underline (color . "blue"))
					  (squiggly (color . "orange")) (strike-out (color . "red"))))

  (customize-set-variable
   'display-buffer-alist
   '(("^\\*outline"
      display-buffer-in-side-window
      (side . right)
      (window-width . 0.35)
      (inhibit-switch-frame . t)))))

(use-package geiser)
(use-package geiser-guile)

(use-package rust-mode
  :config
  ; Force indentation to use spaces.
  (indent-tabs-mode nil)
  (rust-enable-format-on-save)
  (prettify-symbols-mode))

(use-package json-mode)

(use-package go-mode
  :hook (before-save . gofmt-before-save))

;; (use-package vterm)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-ltgo")))

(use-package idris-mode
  :ensure t
  :custom
  (idris-interpreter-path "idris2"))

;; (use-package idris-mode
;;   :mode ("\\.l?idr\\'" . idris-mode)
;;   :config
;;   (after lsp-mode
;;     (add-to-list 'lsp-language-id-configuration '(idris-mode . "idris2"))

;;     (lsp-register-client
;;      (make-lsp-client
;;       :new-connection (lsp-stdio-connection "idris2-lsp")
;;       :major-modes '(idris-mode)
;;       :server-id 'idris2-lsp))
;;     )
;;     ;; (setq lsp-semantic-tokens-enable t) ;; Optionally enable semantic tokens

;;   (add-hook 'idris-mode-hook #'lsp!)
;;   )

(setq load-path (cons (expand-file-name "~/devel/llvm-project/llvm/utils/emacs/") load-path))
(require 'llvm-mode)

(use-package clang-format
  :init
  (setq-default clang-format-style "file")
  (setq-default clang-format-fallback-style "llvm")
  :config
  (add-hook 'c-mode-hook (lambda ()
        		   (add-hook 'before-save-hook 'clang-format-buffer nil 'local))))

(defun db/treemacs-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  ((if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1)))
  (treemacs)))

;; (defadvice compile (before ad-compile-smart activate)
;;   "Advises `compile' so it sets the argument COMINT to t."
;;   (ad-set-arg 1 t))

;; Frama C
(setq load-path (cons (expand-file-name "~/.opam/5.3.0/share/frama-c/share/emacs/") load-path))
(autoload 'acsl-mode "acsl" "Major mode for editing ACSL code" t)
(add-to-list 'lsp-language-id-configuration '(acsl-mode . "c"))
