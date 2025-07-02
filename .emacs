(setq image-types (cons 'svg image-types))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4"))) nil "Customized with leaf in doom-themes block"))

;; scroll bar
(scroll-bar-mode -1)
(menu-bar-mode nil)
(tool-bar-mode 0)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; utf8
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)

;; facあいうえお
(set-face-attribute 'default nil
                    :font "HackGen Console NF"
                    :family "Mono"
                    :height 120
                    )
(setq-default line-spacing 0.15)

;(set-face-attribute 'default nil
;                    :font "PlemolJP Console NF"
;                    :family "Monno"
;                    )
;(setq face-font-rescale-alist
;      '((".*Monaco-bold.*" . 1.0)
;        (".*Monaco-medium.*" . 1.0)
;        (".*Osaka-bold.*" . 1.0)
;        (".*Osaka-medium.*" . 1.0)
;        ("-cdac$" . 1.4)))

;; tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(add-hook 'js2-mode-hook (lambda ()
            (setq tab-width 2)
            (setq c-basic-offset 2)))
(add-hook 'rjsx-mode-hook (lambda ()
            (setq tab-width 2)
            (setq c-basic-offset 2)))
(add-hook 'scss-mode-hook (lambda ()
            (setq indent-tabs-mode nil)
            (setq css-indent-offset 2)
            (setq tab-width 2)
            (setq c-basic-offset 2)))

(setq web-mode-markup-indent-offset 2)
(setq js2-mode-markup-indent-offset 2)
(setq tab-stop-list (number-sequence 2 120 2))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;;
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(setq-default indicate-empty-lines t)
;; window
(setq default-frame-alist
      (append (list '(top . 0) ; 起動時の表示位置（右から）
                    '(left . 0) ; 起動時の表示位置（左から）
                    '(width . 210) ; 起動時のサイズ（幅）
                    '(height . 64) ; 起動時のサイズ（縦）
                    '(background-color . "#224433")
                    '(foreground-color . "Gray92")
                    '(cursor-color . "Yellow")
                    )
              default-frame-alist))

;; leaf
(leaf leaf-keywords
  :ensure t
  :init
  ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
  (leaf hydra :ensure t)
  (leaf el-get :ensure t)
  (leaf blackout :ensure t)
  :config
  ;; initialize leaf-keywords.el
  (leaf-keywords-init))


;; TRAMP
(leaf tramp
  :doc "Transparent Remote Access, Multiple Protocol"
  :tag "builtin"
  :added "2020-09-08"
  :config
  (setq tramp-default-method "ssh")
  )


;; files
(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))


;; company
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)


; doom-themes
(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
  :url "https://github.com/hlissner/emacs-doom-theme"
  :emacs>= 25.1
  :ensure nil
  :custom ((doom-themes-enable-italic . nil)
           (doom-themes-enable-bold . t))
  :custom-face ((doom-modeline-bar quote ((t (:background "#6272a4")))))
  :require t
  :config
;  (load-theme 'doom-molokai t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
;(setq doom-theme 'doom-dracula)


;; whitespace
(leaf whitespace
  :blackout ((global-whitespace-mode . "")
             (whitespace-mode        . ""))
  :hook (after-init-hook . global-whitespace-mode)
  :custom
  ((whitespace-line-column      . 72)
   (whitespace-style            . '(face        ; faceを使う
                                    trailing    ; 行末の空白を対象.
                                    tabs        ; tab
                                    spaces      ; space
                                    empty       ; 先頭/末尾の空行
                                    space-mark  ; 表示のマッピング
                                    tab-mark
                                    ))
   (whitespace-display-mappings . '((space-mark ?\u3000 [?\uff3f])
                                    (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (whitespace-space-regexp     . "\\(\u3000+\\)")
   (whitespace-trailing-regexp  . "\\([ \u00A0]+\\)$")
   (whitespace-global-modes     . '(not markdown-mode)))
  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "#303340"
                      :foreground nil
                      )
  )

(defun my/delete-trailing-whitespace-only ()
  "Delete only trailing whitespace on save."
  (when (derived-mode-p 'prog-mode 'text-mode)
    (let ((whitespace-style '(trailing)))
      (whitespace-cleanup))))

(add-hook 'before-save-hook #'my/delete-trailing-whitespace-only)

;; dumb-jump
(leaf dumb-jump
  :if (executable-find "rg")
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :custom '((dumb-jump-selector       . 'ido)
            (dumb-jump-force-searcher . 'rg))
  )
(leaf smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers)
  )

;; phpcbf
;(leaf phpcbf
;  :doc "Format PHP code in Emacs using PHP_CodeSniffer's phpcbf"
;  :req "s-1.9.0"
;  :tag "php" "tools"
;  :added "2020-09-05"
;  :url "https://github.com/nishimaki10/emacs-phpcbf"
;  :ensure t
;  :config
; (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
;  (custom-set-variables
;   '(phpcbf-executable "/usr/local/bin/phpcbf")
;   '(phpcbf-standard "PSR12"))
;  )

;(defun phpstan-mode-hook ()
;  "PHPStan-mode hook."
;  (require 'flycheck-phpstan)
;  (flycheck-mode t)
;  (flycheck-select-checker 'phpstan))

;(add-hook 'php-mode-hook 'phpstan-mode-hook)

; prettier
(leaf prettier-js
  :doc "Minor mode to format JS code on file save"
  :tag "js" "edit" "wp" "convenience"
  :added "2020-09-08"
  :url "https://github.com/prettier/prettier-emacs"
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  )


;; auto-mode
(setq auto-mode-alist
      (append '(
                ("\\.py$" . python-mode)
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
                ("\\.svg$" . xml-mode)
                ) auto-mode-alist))

;-----------
; tabnine
;-----------
; https://github.com/TommyX12/company-tabnine
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)
; Trigger completion immediately.
(setq company-idle-delay 0)
; Number the candidates (use M-1, M-2 etc to select completions).
;(setq company-show-numbers t)

;-----------
; codeium
;-----------
; https://github.com/Exafunction/codeium.el
(add-to-list 'load-path "~/.emacs.d/codeium.el")

(require 'js-doc)

(editorconfig-mode 1)

(leaf web-mode
  :ensure t
  :config
  (setq web-mode-enable-comment-interpolation t)
  )


;(leaf tree-sitter
;  :ensure (t tree-sitter-langs)
;  :require tree-sitter-langs
;  :config
;  (global-tree-sitter-mode)
;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;  ;; TSXの対応
;  (tree-sitter-require 'tsx)
;  (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . tsx))
;  ;; ハイライトの追加
;  (tree-sitter-hl-add-patterns 'tsx
;    [
;     ;; styled.div``
;     (call_expression
;      function: (member_expression
;                 object: (identifier) @function.call
;                 (.eq? @function.call "styled"))
;      arguments: ((template_string) @property.definition
;                  (.offset! @property.definition 0 1 0 -1)))
;     ;; styled(Component)``
;     (call_expression
;      function: (call_expression
;                 function: (identifier) @function.call
;                 (.eq? @function.call "styled"))
;      arguments: ((template_string) @property.definition
;                  (.offset! @property.definition 0 1 0 -1)))
;     ])
;  )

;;
;(add-to-list 'load-path "~/.emacs.d/conf")
;(load "typescript-mode")


;(require 'multi-web-mode)
;(setq mweb-default-major-mode 'web-mode)
;(setq mweb-tags '((scss-mode "`\n" "\n`")
;                  (rjsx-mode "return (" ");")
;                  ))
;(setq mweb-filename-extensions '("tsx"))
;(multi-web-global-mode 1)


;; use css in js with polymode
;(leaf polymode
;  :ensure t
;  :config
;  (define-hostmode poly-web-hostmode :mode 'js-mode)
;  (define-innermode poly-css-innermode :mode 'scss-mode
;    :head-matcher "`\n"
;    :tail-matcher "\n`"
;    :head-mode 'host
;    :tail-mode 'host)
;  (define-innermode poly-jsx-innermode :mode 'rjsx-mode
;    :head-matcher "return ("
;    :tail-matcher ");"
;    :head-mode 'host
;    :tail-mode 'host)
;  (define-polymode poly-web-mode
;    :hostmode 'poly-web-hostmode
;    :innermodes '(poly-css-innermode
;                  poly-jsx-innermode))
;  (setq auto-mode-alist (append (list '("\\.tsx" . poly-web-mode))))
;  )

;; Install and configure GitHub Copilot using leaf
(leaf copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  ;; Keybindings for Copilot
  (define-key copilot-mode-map (kbd "M-TAB") 'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "C-g") 'copilot-reject-completion))

;; Additional configuration for GitHub Copilot
(leaf copilot
  :config
  (setq copilot-idle-delay 0.5)
  (setq copilot-max-suggestions 10))

(leaf ellama
  :if (executable-find "ollama")
  :ensure t
  :defer-config
  (require 'llm-ollama)
  :custom
  (ellama-keymap-prefix . "C-c e")  ;; キーバインディングの設定
  (ellama-language . "日本語")    ;; 言語設定
  (ellama-provider . #s(llm-ollama nil nil nil "http" "localhost" 11434 "gemma2" "gemma2"))
  (ellama-providers
   . '(("codellama"  . #s(llm-ollama nil nil nil "http""localhost" 11434 "codellama" "codellama"))
         ("gemma2"   . #s(llm-ollama nil nil nil "http""localhost" 11434 "gemma2"    "gemma2"))
         ("llama3.2" . #s(llm-ollama nil nil nil "http""localhost" 11434 "llama3.2"  "llama3.2"))
         ))

  (ellama-code-review-prompt-template . "以下のコードのレビューと改善案をだして:\n```\n%s\n```")
  (ellama-define-word-prompt-template . "%s の定義を教えて")
)

(setq ellama-enable-code-completion t)
(setq ellama-enable-refactoring t)
(setq ellama-enable-syntax-checking t)
(setq ellama-enable-code-snippets t)


(leaf claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (claude-code-mode)
  (define-key global-map (kbd "C-c x") claude-code-command-map))
