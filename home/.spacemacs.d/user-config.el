(add-to-list 'custom-theme-load-path "~/.spacemacs.d/")
(load-file "~/.spacemacs.d/faces-init.el")

(setq dotspacemacs-startup-banner "~/.spacemacs.d/vaporwave-sun.png")

(defun my-open-chat-gpt ()
  (interactive)
  (gptel "*ChatGPT*")
  (let ((buffer (get-buffer "*ChatGPT*")))
    (switch-to-buffer buffer)))

(spacemacs/set-leader-keys
  "og" 'my-open-chat-gpt)

(with-eval-after-load 'bug-reference
  (remove-hook 'prog-mode-hook #'bug-reference-prog-mode))

(spacemacs/set-leader-keys "off" 'hs-toggle-hiding)
(spacemacs/set-leader-keys "ofl" 'hs-hide-level)
(spacemacs/set-leader-keys "ofa" 'hs-show-all)

(setq lsp-rubocop-use-bundler t)
(setq lsp-ruby-lsp-use-bundler t)
(setq lsp-solargraph-use-bundler t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local flycheck-command-wrapper-function
                        (lambda (command) (append '("bundle" "exec") command)))))

(add-hook 'inf-ruby-mode-hook
          (lambda()
            (let ((p "\\|\\(^\\[cleo\\]\\[development\\] main:[0-9]+> *\\)"))
              (setq inf-ruby-first-prompt-pattern
                    (concat inf-ruby-first-prompt-pattern p))
              (setq inf-ruby-prompt-pattern
                    (concat inf-ruby-prompt-pattern p)))))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
     `(ruby-mode
        ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
        ,(rx (or "}" "]" "end"))                       ; Block end
        ,(rx (or "#" "=begin"))                        ; Comment start
        ruby-forward-sexp nil)))

(assq-delete-all 'ruby-Test::Unit compilation-error-regexp-alist-alist)
(add-to-list 'compilation-error-regexp-alist-alist '(ruby-Test::Unit "^ +\\([^ (].*\\):\\([1-9][0-9]*\\):in " 1 2))
(assoc 'ruby-Test::Unit compilation-error-regexp-alist-alist)

(add-hook 'dired-mode-hook 'diredfl-mode)

(setq insert-directory-program "gls")

(setq evil-escape-key-sequence [106 107])

(defalias 'forward-evil-word 'forward-evil-symbol)

(setq mac-right-option-modifier 'none)

(spacemacs/set-leader-keys "oi" 'ibuffer)

(setq vc-follow-symlinks t)

(use-package transient-posframe
  :ensure t
  :init (transient-posframe-mode))

(setq doom-modeline-buffer-encoding 'nondefault)
(setq doom-modeline-env-enable-ruby nil)
(setq doom-modeline-env-version nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-mu4e t)
(setq doom-modeline-time nil)

(defun my-org-mode-hook ()
  (auto-fill-mode 0)
  (face-remap-add-relative 'hl-line `(:background nil))
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (setq evil-auto-indent nil
        visual-fill-column-width 120
        visual-fill-column-center-text t))

(add-hook 'org-mode-hook 'my-org-mode-hook)
