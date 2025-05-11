(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-by-copying t) ; Avoid symlinks getting messed up
(setq create-lockfiles nil)

(setq safe-local-variable-directories
      '("/Users/jake/work/cleo/meetcleo/"
        "/Users/jake/work/rusty-bucket/rusty-bucket/"))

(add-to-list 'custom-theme-load-path "~/.spacemacs.d/")
(load-file "~/.spacemacs.d/faces-init.el")

(setq dotspacemacs-startup-banner "~/.spacemacs.d/vaporwave-sun.png")
(setq lsp-ui-doc-border "#200933")

; need proportional width for nerdfonts or they overlap
(setq nerd-icons-font-family "MonaspiceAr Nerd Font Propo")

(defun my-open-chat-gpt ()
  (interactive)
  (gptel "*ChatGPT*")
  (let ((buffer (get-buffer "*ChatGPT*")))
    (switch-to-buffer buffer)))

(spacemacs/set-leader-keys
  "og" 'my-open-chat-gpt)

(defun jake/github-codeowners-this-file ()
  "Message the result of running github-codeowners on this file"
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (let* ((this-file (spacemacs/projectile-copy-file-path))
           (codeowner (shell-command-to-string (concat "bin/cleo-codeowners find_owner " this-file))))
      (message codeowner))))

(spacemacs/set-leader-keys
  "oo" 'jake/github-codeowners-this-file)

(with-eval-after-load 'bug-reference
  (remove-hook 'prog-mode-hook #'bug-reference-prog-mode))

(spacemacs/set-leader-keys "off" 'hs-toggle-hiding)
(spacemacs/set-leader-keys "ofl" 'hs-hide-level)
(spacemacs/set-leader-keys "ofa" 'hs-show-all)

(with-eval-after-load 'highlight-parentheses
  (setq highlight-parentheses-colors nil))

(global-set-key (kbd "s-<return>") 'company-complete)

(defun jake-disable-company-for-symbols (func &rest args)
  "Prevent Company from triggering if the current word starts with `:`"
  (if (and (derived-mode-p 'ruby-mode)
           (looking-back ":[[:alnum:]_]*" (line-beginning-position)))
      nil
    (apply func args)))

(advice-add 'company--should-complete :around #'jake-disable-company-for-symbols)

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

(setq lsp-disabled-clients '(rubocop-ls ruby-ls sorbet-ls))

(assq-delete-all 'ruby-Test::Unit compilation-error-regexp-alist-alist)
(add-to-list 'compilation-error-regexp-alist-alist '(ruby-Test::Unit "^ +\\([^ (].*\\):\\([1-9][0-9]*\\):in " 1 2))
(assoc 'ruby-Test::Unit compilation-error-regexp-alist-alist)

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(typespec "https://github.com/happenslol/tree-sitter-typespec")))

(add-hook 'dired-mode-hook 'diredfl-mode)

(setq insert-directory-program "gls")

(setq message-send-mail-function 'smtpmail-send-it
  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  smtpmail-auth-credentials (expand-file-name "~/.authinfo")
  smtpmail-default-smtp-server "smtp.gmail.com"
  smtpmail-smtp-user "jake@meetcleo.com"
  smtpmail-smtp-server "smtp.gmail.com"
  smtpmail-smtp-service 587
  smtpmail-debug-info t)

(with-eval-after-load 'mu4e
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Cleo"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/cleo" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "jake@meetcleo.com")
                  (user-full-name . "Jake Prime")
                  (mu4e-refile-folder . "/cleo/_Archive")
                  (mu4e-sent-folder . "/cleo/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/cleo/[Gmail]/Bin")
                  (mu4e-alert-interesting-mail-query . "flag:unread AND maildir:/cleo/Inbox")
                  (smtpmail-smtp-user . "jake@meetcleo.com")))
         (make-mu4e-context
          :name "Personal"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "jake@jakeprime.com")
                  (user-full-name . "Jake Prime")
                  (mu4e-refile-folder . "/personal/_Archive")
                  (mu4e-sent-folder . "/personal/[Google Mail]/Sent Mail")
                  (mu4e-trash-folder . "/personal/[Google Mail]/Bin")
                  (mu4e-alert-interesting-mail-query . "flag:unread AND maildir:/personal/Inbox")
                  (smtpmail-smtp-user . "jake.prime@gmail.com")))
         ))

  (setq mu4e-modeline-all-clear '("C:" . "󰄰 "))
  (setq mu4e-modeline-new-items '("N:" . "󰈸 "))
  (setq mu4e-modeline-read-items '("R:" . " "))
  (setq mu4e-modeline-unread-items '("U:" . " "))

  (setq mu4e-maildir-shortcuts
        '((:maildir "/cleo/Inbox" :key ?c :name "Cleo" :hide t)
          (:maildir "/personal/Inbox" :key ?p :name "Personal" :hide t))))

(with-eval-after-load 'mu4e
  (setf (alist-get 'refile mu4e-marks)
        '(:char ("r" . "▶")
          :prompt "refile"
          :show-target (lambda (target)
                         (if target target "Skip - sent message"))
          :dyn-target (lambda (target msg)
                        (let* ((maildir (mu4e-message-field msg :maildir))
                               (sent-folder (mu4e-get-sent-folder msg)))
                          (if (string= maildir sent-folder)
                              nil
                            (mu4e-get-refile-folder msg))))
          :action (lambda (docid msg target)
                    (if target
                        (mu4e--server-move docid
                                           (mu4e--mark-check-target target)
                                           "+S-N")
                      nil))))

  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
          :prompt "dtrash"
          :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
          :action (lambda (docid msg target)
                    (mu4e--server-move docid
                                       (mu4e--mark-check-target target) "+S-N"))))

  (setq mu4e-headers-attach-mark '("a" . "+"))
  (setq mu4e-headers-list-mark '("l" . "@"))
  (setq mu4e-headers-personal-mark '("p" . "."))
  (setq mu4e-headers-flagged-mark '("f" . "!"))
  (setq mu4e-headers-new-mark '("N" . "*")))

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
