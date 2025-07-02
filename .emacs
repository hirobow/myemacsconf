;;; -*- lexical-binding: t; -*-
;;; .emacs --- Personal Emacs configuration

;;; Commentary:
;; Modern Emacs configuration for web development with AI/ML integrations

;;; Code:

;;; ========================================
;;; Package Management Setup
;;; ========================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap leaf
(eval-and-compile
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    :config
    (leaf-keywords-init)))

;;; ========================================
;;; Core Settings
;;; ========================================

(leaf *core-settings
  :config
  ;; Encoding
  (prefer-coding-system 'utf-8)
  (set-language-environment "Japanese")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)

  ;; UI
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode 0)
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t)

  ;; Font
  (set-face-attribute 'default nil
                      :font "HackGen Console NF"
                      :family "Mono"
                      :height 120)
  (setq-default line-spacing 0.15)

  ;; Frame
  (setq default-frame-alist
        '((top . 0)
          (left . 0)
          (width . 210)
          (height . 64)
          (background-color . "#224433")
          (foreground-color . "Gray92")
          (cursor-color . "Yellow")))

  ;; Image support
  (setq image-types (cons 'svg image-types)))

;;; ========================================
;;; Backup and Auto-save
;;; ========================================

(leaf *backup-settings
  :custom
  ((auto-save-timeout . 15)
   (auto-save-interval . 60)
   (auto-save-file-name-transforms . `((".*" ,(locate-user-emacs-file "backup/") t)))
   (backup-directory-alist . `((".*" . ,(locate-user-emacs-file "backup"))
                               (,tramp-file-name-regexp . nil)))
   (version-control . t)
   (delete-old-versions . t)))

;;; ========================================
;;; Indentation and Formatting
;;; ========================================

(leaf *indentation
  :config
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 2 120 2)))

;;; ========================================
;;; File Associations
;;; ========================================

(leaf *file-associations
  :config
  (setq auto-mode-alist
        (append '(("\\.py$" . python-mode)
                  ("\\.json$" . json-mode)
                  ("\\.yaml$" . yaml-mode)
                  ("\\.yml$" . yaml-mode)
                  ("\\.md$" . markdown-mode)
                  ("\\.css$" . css-mode)
                  ("\\.less$" . css-less-mode)
                  ("\\.scss$" . scss-mode)
                  ("\\.php$\\|\\.phplib$\\|\\.phpclass$\\|\\.inc$" . php-mode)
                  ("\\.html$\\|\\.volt$\\|\\.tpl$\\|\\.ctp$" . web-mode)
                  ("\\.js$" . js2-mode)
                  ("\\.mjs$" . js2-mode)
                  ("\\.jsx$" . rjsx-mode)
                  ("\\.ts$" . typescript-mode)
                  ("\\.tsx$" . web-mode)
                  ("\\.astro$" . web-mode)
                  ("\\.svg$" . xml-mode))
                auto-mode-alist)))

;;; ========================================
;;; Theme Configuration
;;; ========================================

(leaf doom-themes
  :ensure t
  :custom
  ((doom-themes-enable-italic . nil)
   (doom-themes-enable-bold . t))
  :custom-face
  ((doom-modeline-bar . '((t (:background "#6272a4")))))
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;; ========================================
;;; Whitespace Management
;;; ========================================

(leaf whitespace
  :blackout ((global-whitespace-mode . "")
             (whitespace-mode . ""))
  :hook (after-init-hook . global-whitespace-mode)
  :custom
  ((whitespace-line-column . 72)
   (whitespace-style . '(face trailing tabs spaces empty space-mark tab-mark))
   (whitespace-display-mappings . '((space-mark ?\u3000 [?\uff3f])
                                    (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (whitespace-space-regexp . "\\(\u3000+\\)")
   (whitespace-trailing-regexp . "\\([ \u00A0]+\\)$")
   (whitespace-global-modes . '(not markdown-mode)))
  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "#303340"
                      :foreground nil)

  ;; Auto-cleanup on save
  (defun my/delete-trailing-whitespace-only ()
    "Delete only trailing whitespace on save."
    (when (derived-mode-p 'prog-mode 'text-mode)
      (let ((whitespace-style '(trailing)))
        (whitespace-cleanup))))
  (add-hook 'before-save-hook #'my/delete-trailing-whitespace-only))

;;; ========================================
;;; Core Packages
;;; ========================================

;; TRAMP
(leaf tramp
  :doc "Transparent Remote Access, Multiple Protocol"
  :tag "builtin"
  :config
  (setq tramp-default-method "ssh"))

;; Company - Auto-completion
(leaf company
  :ensure t
  :blackout t
  :bind
  ((company-active-map
    ("M-n" . nil)
    ("M-p" . nil)
    ("C-s" . company-filter-candidates)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("<tab>" . company-complete-selection))
   (company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :custom
  ((company-idle-delay . 0)
   (company-minimum-prefix-length . 1)
   (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

;; EditorConfig
(leaf editorconfig
  :ensure t
  :global-minor-mode editorconfig-mode)

;;; ========================================
;;; Navigation Tools
;;; ========================================

(leaf dumb-jump
  :ensure t
  :if (executable-find "rg")
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g i" . dumb-jump-go-prompt)
   ("M-g x" . dumb-jump-go-prefer-external)
   ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :custom
  ((dumb-jump-selector . 'ido)
   (dumb-jump-force-searcher . 'rg)))

(leaf smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers))

;;; ========================================
;;; Web Development
;;; ========================================

(leaf web-mode
  :ensure t
  :custom
  ((web-mode-markup-indent-offset . 2)
   (web-mode-enable-comment-interpolation . t)))

(leaf js2-mode
  :ensure t
  :hook
  ((js2-mode-hook . (lambda ()
                      (setq js2-basic-offset 2)
                      (setq tab-width 2)
                      (setq c-basic-offset 2)))))

(leaf rjsx-mode
  :ensure t
  :hook
  ((rjsx-mode-hook . (lambda ()
                       (setq tab-width 2)
                       (setq c-basic-offset 2)))))

(leaf scss-mode
  :ensure t
  :hook
  ((scss-mode-hook . (lambda ()
                       (setq indent-tabs-mode nil)
                       (setq css-indent-offset 2)
                       (setq tab-width 2)
                       (setq c-basic-offset 2)))))

(leaf prettier-js
  :ensure t
  :hook (js2-mode-hook . prettier-js-mode))

(leaf js-doc
  :ensure t)

;;; ========================================
;;; AI/ML Integrations
;;; ========================================

;; TabNine
(leaf company-tabnine
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))

;; GitHub Copilot
(leaf copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind
  (:copilot-mode-map
   ("M-TAB" . copilot-accept-completion)
   ("C-g" . copilot-reject-completion))
  :custom
  ((copilot-idle-delay . 0.5)
   (copilot-max-suggestions . 10)))

;; Ellama - Local LLM Integration
(leaf ellama
  :ensure t
  :if (executable-find "ollama")
  :defer-config
  (require 'llm-ollama)
  :custom
  ((ellama-keymap-prefix . "C-c e")
   (ellama-language . "日本語")
   (ellama-provider . #s(llm-ollama nil nil nil "http" "localhost" 11434 "gemma2" "gemma2"))
   (ellama-providers . '(("codellama" . #s(llm-ollama nil nil nil "http" "localhost" 11434 "codellama" "codellama"))
                         ("gemma2" . #s(llm-ollama nil nil nil "http" "localhost" 11434 "gemma2" "gemma2"))
                         ("llama3.2" . #s(llm-ollama nil nil nil "http" "localhost" 11434 "llama3.2" "llama3.2"))))
   (ellama-code-review-prompt-template . "以下のコードのレビューと改善案をだして:\n```\n%s\n```")
   (ellama-define-word-prompt-template . "%s の定義を教えて"))
  :config
  (setq ellama-enable-code-completion t)
  (setq ellama-enable-refactoring t)
  (setq ellama-enable-syntax-checking t)
  (setq ellama-enable-code-snippets t))

;; Claude Code
(leaf claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :bind
  (("C-c x" . claude-code-command-map))
  :config
  (claude-code-mode))

;;; ========================================
;;; Custom Variables (Auto-generated)
;;; ========================================

(custom-set-variables
 '(auth-source-save-behavior nil)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
 '(auto-save-interval 60)
 '(auto-save-timeout 15)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backup") ("\\`/[^/:]+:[^/:]*:")))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-transformers '(company-sort-by-occurrence))
 '(delete-old-versions t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic nil)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"
                  :rev :newest)))
 '(phpcbf-executable "/usr/local/bin/phpcbf")
 '(phpcbf-standard "PSR12")
 '(safe-local-variable-values '((php-project-root . auto)))
 '(version-control t))

(custom-set-faces
 '(doom-modeline-bar ((t (:background "#6272a4"))) nil "Customized with leaf in doom-themes block"))

;;; .emacs ends here