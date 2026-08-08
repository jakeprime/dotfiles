;; -*- lexical-binding: nil -*-

(let ((shims (expand-file-name "~/.local/share/mise/shims")))
  (setq exec-path (cons shims (delete shims exec-path)))
  (setenv "PATH" (concat shims path-separator (getenv "PATH"))))
