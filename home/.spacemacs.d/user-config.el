(setq safe-local-variable-directories
      '("/Users/jake/work/cleo/meetcleo/"
        "/Users/jake/work/rusty-bucket/rusty-bucket/"))

(add-to-list 'custom-theme-load-path "~/.spacemacs.d/")
(load-file "~/.spacemacs.d/faces-init.el")

(setq dotspacemacs-startup-banner "~/.spacemacs.d/vaporwave-sun.png")
(setq lsp-ui-doc-border "#200933")

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

(defun my-add-flycheck-next-checker ()
  (when (and (derived-mode-p 'ruby-mode)
             ;; Ensure LSP checker exists
             (flycheck-registered-checker-p 'lsp))
    (flycheck-add-next-checker 'lsp 'ruby-rubocop)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'my-add-flycheck-next-checker))

(setq flycheck-disabled-checkers '(ruby-reek))

(setq lsp-rubocop-use-bundler t)

  (add-hook
   'ruby-mode-hook
   (lambda ()
     (setq-local flycheck-command-wrapper-function
                 (lambda (command)
(append (list (concat (project-root (project-current)) "bin/bundle") "exec") command)))))

(setq lsp-sorbet-as-add-on t)
(setq lsp-sorbet-use-bundler t)

(eval-after-load "lsp-mode"
  '(defun lsp--symbols-informations->document-symbols-hierarchy (symbols-informations current-position)
     "Convert SYMBOLS-INFORMATIONS to symbols hierarchy on CURRENT-POSITION."
     (--> symbols-informations
          (-keep (-lambda (symbol)
                   (when (and (gethash "location" symbol)
                              (lsp-point-in-range? current-position (gethash "range" (gethash "location" symbol))))
                     (lsp--symbol-information->document-symbol symbol)))
                 it)
          (sort it (-lambda ((&DocumentSymbol :range (&Range :start a-start-position :end a-end-position))
                             (&DocumentSymbol :range (&Range :start b-start-position :end b-end-position)))
                     (and (lsp--position-compare b-start-position a-start-position)
                          (lsp--position-compare a-end-position b-end-position)))))))

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

(setq lsp-modeline-code-action-fallback-icon "")
(setq lsp-progress-prefix " ")

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
