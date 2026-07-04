# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A dotfiles "castle" managed by **homesick** (the Ruby gem — not homeshick). Everything under `home/` is symlinked into `$HOME` by `homesick link main`. There is no build, lint, or test step; the only automated check is a gitleaks pre-commit hook (configured via `core.hooksPath` in `home/.config/git/config`).

## Common commands

```sh
homesick link main          # (re)create symlinks after adding new files
homesick track <file> main  # move a file from $HOME into the castle and symlink it
homesick cd main            # jump to this repo
```

`.homesick_subdir` lists `.config`, `.local/bin`, and `.oh-my-zsh` — entries *inside* those directories are symlinked individually (e.g. `~/.config/nvim` → `home/.config/nvim`) rather than symlinking the parent directory wholesale. Adding a new file/dir inside one of them requires re-running `homesick link main`.

Because files are symlinked, editing the live file in `$HOME` edits this repo, and vice versa.

## Layout

- `home/` — everything symlinked into `$HOME` (zsh, git, nvim, hyprland, ghostty, spacemacs/doom, mise, etc.)
- `files/` — assets not symlinked by homesick (fonts, system config); installed manually to system locations
- `scripts/` — currently empty

## Cross-platform structure

The same castle is used on Arch Linux and macOS:

- `home/.zshrc` is shared and sources `home/.zshrc.arch` or `home/.zshrc.mac` based on `uname` at the end
- Ghostty: per-OS configs (`arch.ghostty` / `mac.ghostty`) with a gitignored `local.ghostty` symlink created by the platform zshrc on first shell start
- On Arch, Hyprland config (`home/.config/hypr/hyprland.conf`) sources Omarchy defaults from `~/.local/share/omarchy/` and layers personal overrides from `home/.config/hypr/custom/` — put customizations in `custom/`, don't edit the Omarchy defaults
- `home/.config/jake/theme/current/` is an Omarchy-style theme directory holding per-app theme files (alacritty, btop, ghostty, emacs, wallpapers, etc.)

## Machine-local and secret files

`.gitignore` deliberately excludes machine-specific and secret files: `secrets.sh`, `home/.config/spacemacs/.spacemacs.env`, `home/.config/ghostty/local.ghostty`, and `home/.config/hypr/custom/{input/keyboard,monitors/monitors}.conf`. Don't try to commit these or be surprised when edits to them don't show in `git status`. Secrets are read from `pass` at shell startup, never stored in the repo.

## Conventions

- Commit messages are short imperative one-liners ("Link .local/bin", "Move git config into XDG")
- Configs live under XDG paths (`~/.config`, `~/.local`) — ongoing effort to move stray dotfiles there
