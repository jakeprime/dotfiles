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

(spacemacs/set-leader-keys "off" 'hs-toggle-hiding)
(spacemacs/set-leader-keys "ofl" 'hs-hide-level)
(spacemacs/set-leader-keys "ofa" 'hs-show-all)

(assq-delete-all 'ruby-Test::Unit compilation-error-regexp-alist-alist)
(add-to-list 'compilation-error-regexp-alist-alist '(ruby-Test::Unit "^ +\\([^ (].*\\):\\([1-9][0-9]*\\):in " 1 2))
(assoc 'ruby-Test::Unit compilation-error-regexp-alist-alist)

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
                  (mu4e-refile-folder . "/cleo/[Gmail]/All Mail")
                  (mu4e-sent-folder . "/cleo/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/cleo/[Gmail]/Trash")
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
                  (mu4e-refile-folder . "/personal/[Google Mail]/All Mail")
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
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e--server-move docid
                                              (mu4e--mark-check-target target) "+S-N"))))
  (add-to-list 'mu4e-marks
               '(flag
                 :char ("+" . "★")
                 :prompt "dflag"
                 :dyn-target (lambda (target msg)
                               (replace-regexp-in-string "All Mail" "Starred"
                                                         (mu4e-get-refile-folder msg)))
                 :action (lambda (docid msg target)
                           (mu4e--server-move docid
                                              (mu4e--mark-check-target target))))))

(setq mu4e-headers-attach-mark '("a" . "+"))
(setq mu4e-headers-list-mark '("l" . "@"))
(setq mu4e-headers-personal-mark '("p" . "."))
(setq mu4e-headers-flagged-mark '("f" . "!"))
(setq mu4e-headers-new-mark '("N" . "*"))

(setq evil-escape-key-sequence [106 107])

(defalias 'forward-evil-word 'forward-evil-symbol)

(setq mac-right-option-modifier 'none)

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
