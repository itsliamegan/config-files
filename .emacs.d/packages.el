;; Evaluate this buffer to install all the necessary packages.

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq packages '(avy
                 company
                 consult
                 ctrlf
                 dashboard
                 elixir-mode
                 evil
                 evil-args
                 evil-collection
                 evil-commentary
                 evil-surround
                 exec-path-from-shell
                 flycheck
                 lsp-mode
                 magit
                 marginalia
                 markdown-mode
                 modus-operandi-theme
                 modus-vivendi-theme
                 olivetti
                 org-bullets
                 page-break-lines
                 prescient
                 projectile
                 selectrum
                 selectrum-prescient
                 solarized-theme
                 undo-fu
                 visual-regexp
                 xterm-color))

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))
