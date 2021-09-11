;;---------;;
;; STARTUP ;;
;;---------;;

;; Increase the garbage collection threshold at startup, then set it to a
;; reasonable amount after Emacs has initialized.
(defconst *16-mb* (* 16 1024 1024))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold *16-mb*
                  gc-cons-percentage 0.1)))

;; Increase the garbage collection threshold when using the minibuffer, then
;; restore it afterwards.
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (run-at-time 1 nil (lambda ()
                                 (setq gc-cons-threshold *16-mb*)))))

;; Don't show the default startup screen or startup messages.
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(defun startup-echo-area-message () "")

;; Show an improved startup screen.
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Emacs"
      dashboard-center-content t
      dashboard-set-footer nil
      dashboard-set-init-info nil
      dashboard-show-shortcuts nil
      dashboard-startup-banner 'logo
      dashboard-items '((recents  . 5)
                        (projects . 5)))

;; Store customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;-----------;;
;; UTILITIES ;;
;;-----------;;

(defun delete-current-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-buffer)
    (message "Deleted '%s'" filename)))

(defun system-dark-mode-p ()
  (string=
   (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"")
   "true"))

;;---------;;
;; TESTING ;;
;;---------;;

(setq last-test-command "")

(defun test-command ()
  (cond ((eq major-mode 'ruby-mode) "bundle exec rake test")
        ((eq major-mode 'elixir-mode) "mix test")))

(defun test-project ()
  (interactive)
  (let ((test-project-command (test-command)))
    (setq last-test-command test-project-command)
    (compile test-project-command)))

(defun test-file ()
  (interactive)
  (let ((test-file-command (concat (test-command)
                                   (cond ((eq major-mode 'ruby-mode) (concat " TEST=" buffer-file-name))
                                         ((eq major-mode 'elixir-mode) buffer-file-name)))))
    (setq last-test-command test-file-command)
    (compile test-file-command)))

(defun test-rerun ()
  (interactive)
  (compile last-test-command))

;;-------------------;;
;; FILES AND BUFFERS ;;
;;-------------------;;

;; Start in the home directory.
(cd (getenv "HOME"))

;; Use $PATH.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; End sentences with a single space.
(setq sentence-end-double-space nil)

;; End files with a newline.
(setq require-final-newline t)

;; Refresh buffers automatically when the file changes.
(global-auto-revert-mode +1)

;; Always follow symlinks.
(setq vc-follow-symlinks t)

;; Improve dired output:
;; G - Don't show the group that owns the file.
;; h - Show human readable file sizes.
(setq dired-listing-switches "-aGhl"
      dired-use-ls-dired nil)

;; Always accept abbreviated answers "y" and "n" instead of "yes" and "no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Store backups and auto-save files in the system temporary directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Remove extraneous buffers.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")

;; Line length of 80 characters, except when writing commit messages.
(setq-default fill-column 80)
(add-hook 'git-commit-mode-hook (lambda ()
                                  (setq fill-column 70)))

;; Indent using 2 spaces.
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence 2 60 2))

;; Treat camel case words as separate words.
(add-hook 'prog-mode-hook 'subword-mode)

;; Automatically break lines in text modes at 80 characters.
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Use ANSI colors in compilation buffers.
(setq compilation-environment '("TERM=xterm-256color"))
(advice-add 'compilation-filter
            :around (lambda (f proc string)
                      (funcall f proc (xterm-color-filter string))))

;; Switch to the compilation buffer after it finishes.
(add-hook 'compilation-finish-functions
          (lambda (buffer string)
            (other-window 1)))

;;-----------;;
;; INTERFACE ;;
;;-----------;;

;; Hide extraneous GUI elements.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Set font to a reasonable size both for when an external monitor is connected
;; and when it's just the laptop screen.
(setq font-size (if (<= (x-display-pixel-height) 1080)
                    22
                  26))
(set-face-font 'default (concat "Iosevka " (number-to-string font-size)))

;; Start in fullscreen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Switch to the help window after it opens.
(setq help-window-select t)

;; Don't ring/show any bell.
(setq ring-bell-function 'ignore)

;; Display relative line numbers on all lines but the current line. On the
;; current line, show the absolute line number.
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Hide the extra space on the left and right.
(fringe-mode -1)

;; Highlight offending characters:
;; face - Required to highlight anything.
;; tabs - Tab characters.
;; trailing - Trailing whitespace.
(setq whitespace-style '(face tabs trailing))
(global-whitespace-mode +1)

;; Customize the modeline.
(defun custom-mode-line-render (left center right &optional lpad rpad)
  "Return a string the width of the current window with LEFT, CENTER, and RIGHT
spaced out accordingly, LPAD and RPAD, can be used to add a number of spaces to
the front and back of the string."
  (condition-case err
      (let* ((left (if lpad (concat (make-string lpad ?\s) left) left))
             (right (if rpad (concat right (make-string rpad ?\s)) right))
             (width (apply '+ (window-width) (let ((m (window-margins))) (list (or (car m) 0) (or (cdr m) 0)))))
             (total-length (+ (length left) (length center) (length right) 2)))
        (when (> total-length width) (setq left "" right ""))
        (let* ((left-space (/ (- width (length center)) 2))
               (right-space (- width left-space (length center)))
               (lspaces (max (- left-space (length left)) 1))
               (rspaces (max (- right-space (length right)) 1 0)))
          (concat left (make-string lspaces  ?\s)
                  center
                  (make-string rspaces ?\s)
                  right)))
    (error (format "[%s]: (%s) (%s) (%s)" err left center right))))

;; Some mode names aren't simple strings, but instead are lists of special
;; values. Special case those mode names with custom ones.
(defun formatted-mode-name ()
  (downcase (cond ((and (listp mode-name)
                        (listp (car mode-name))
                        (member "HTML+" (car mode-name))) "html+")
                  ((and (listp mode-name)
                        (member "JavaScript" mode-name)) "javascript")
                  (t mode-name))))

(setq-default mode-line-format
              '((:eval (custom-mode-line-render
                        (concat (format-mode-line "%b ")
                                (concat "(" (formatted-mode-name) ")")
                                (if (and (buffer-modified-p) (not buffer-read-only))
                                    " [+]"
                                  ""))
                        ""
                        (format-mode-line "%l,%c")
                        1
                        20))))

;; Use an appropriate theme depending on the system dark mode.
(load-theme (if (system-dark-mode-p)
                'modus-vivendi
              'modus-operandi) t)

;;--------;;
;; EDITOR ;;
;;--------;;

;; Use selectrum for a better completion menu and prescient for better completion
;; sorting.
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(setq prescient-sort-full-matches-first t)

;; Show lots of extra info in the minibuffer margin.
(marginalia-mode +1)
(setq marginalia-annotators '(marginalia-annotators-heavy
                              marginalia-annotators-light
                              nil))

;; Manage projects with projectile. When switching to a project, open the
;; project's root directory.
(projectile-mode +1)
(setq projectile-switch-project-action 'projectile-dired)

;; Configure language server protocol and enable it for supported languages.
(setq lsp-headerline-breadcrumb-enable nil)
(add-hook 'ruby-mode-hook #'lsp)

;; Autocomplete with company.
(add-hook 'prog-mode-hook 'company-mode)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;; Show syntax errors with flycheck and disable some annoying checkers.
(add-hook 'prog-mode-hook 'flycheck-mode)
(setq flycheck-disabled-checkers '(emacs-lisp
                                   emacs-lisp-checkdoc
                                   ruby-reek
                                   ruby-rubocop
                                   ruby-standard))

;; Improve search in file.
(ctrlf-mode +1)

;;-----------;;
;; LANGUAGES ;;
;;-----------;;

;; Properly indent in Ruby.
(setq ruby-use-smie nil
      ruby-deep-indent-paren nil)

;; Indent using two spaces in JS.
(setq js-indent-level 2)

;; Use fancy bullets in org.
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode +1)))

;;-------------;;
;; KEYBINDINGS ;;
;;-------------;;

;; Use vi-style keybindings for most modes.
(setq evil-want-keybinding nil)
(evil-mode +1)
(evil-collection-init)
(evil-commentary-mode +1)
(global-evil-surround-mode +1)

;; Start editing in insert mode.
(add-hook 'with-editor-mode-hook 'evil-insert-state)

(setq evil-undo-system 'undo-fu)

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; Use Escape instead of Ctrl-g to cancel any command.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Configure sensible, macOS-inspired keybindings.
;;
;; ---
;;
;; The keybindings are made up of "prefixes" and "modifiers". Prefixes group
;; together commands with a common theme or purpose. Modifiers can be applied to
;; certain commands within prefixes to give them different meanings.
;;
;; Prefixes:
;;
;;   Cmd   (s)   - Common macOS bindings for global actions on the current file
;;                 or buffer.
;;   Ctrl  (C)   - Context-based commands which are slightly less granular than
;;                 motions with text objects.
;;   Space (SPC) - Global, context-independent commands.
;;
;; Modifiers:
;;
;;   Shift  (S) - Indicates that the command performs the opposite of its
;;                typical action.
;;   Option (M) - Indicates that the command performs its typical action on a
;;                "higher level" than the standard command.

;; Cmd prefix
(global-set-key (kbd "s-M-p") 'projectile-switch-project) ; Switch project.
(global-set-key (kbd "s-p")   'projectile-find-file)      ; Find file in project.
(global-set-key (kbd "s-M-o") 'consult-buffer)            ; Open specific buffer.
(global-set-key (kbd "s-o")   'find-file)                 ; Open specific file.
(global-set-key (kbd "s-t")   'consult-imenu)             ; Find symbol in file.
(global-set-key (kbd "s-z")   'undo-fu-only-undo)         ; Undo.
(global-set-key (kbd "s-Z")   'undo-fu-only-redo)         ; Redo.
(global-set-key (kbd "s-,")   'previous-buffer)           ; Previous buffer.
(global-set-key (kbd "s-.")   'next-buffer)               ; Next buffer.

;; Ctrl prefix
(define-prefix-command 'jump-prefix-map)
(define-key global-map (kbd "C-j") 'jump-prefix-map)
(define-key jump-prefix-map (kbd "c") 'avy-goto-char)
(define-key jump-prefix-map (kbd "d") 'lsp-find-definition)

;; Space prefix.
(define-prefix-command 'space-prefix-map)
(define-key evil-normal-state-map (kbd "SPC") 'space-prefix-map)
(define-key space-prefix-map (kbd "SPC") 'execute-extended-command) ; Interactively execute command.
(define-key space-prefix-map (kbd "g")   'magit-status)             ; Git status.
(define-key space-prefix-map (kbd "t p") 'test-project)             ; Run tests for the current project.
(define-key space-prefix-map (kbd "t f") 'test-file)                ; Run tests for the current file.
(define-key space-prefix-map (kbd "t r") 'test-rerun)               ; Run tests for the current file.
