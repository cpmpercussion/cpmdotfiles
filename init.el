;; charles' init.el

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (package
         '(ag
           all-the-icons ; icons for neotree
           auctex ; latex
           auto-complete           
           elpy ; python
           epl ; something about package.el not needed now?
           exec-path-from-shell
           flx-ido
           ido-completing-read+
           imenu-anywhere
           markdown-mode
           multiple-cursors
           neotree
           pandoc-mode
           solarized-theme
           smex
           telephone-line
           unicode-fonts
           yaml-mode
           ))
  (if (not (package-installed-p package))
      (package-install package)))

(global-set-key (kbd "C-c p") 'list-packages)

(global-prettify-symbols-mode +1) ;; prettify symbols

;; cross-platform setup
(exec-path-from-shell-initialize)

(defun charles-setup-keybindings ()
  (define-key global-map (kbd "s-a") 'mark-whole-buffer)
  (define-key global-map (kbd "s-k") 'kill-this-buffer)
  (define-key global-map (kbd "s-l") 'goto-line)
  (define-key global-map (kbd "s-n") 'make-frame)
  (define-key global-map (kbd "s-q") 'save-buffers-kill-emacs)
  (define-key global-map (kbd "s-s") 'save-buffer)
  (define-key global-map (kbd "s-u") 'revert-buffer)
  (define-key global-map (kbd "s-v") 'yank)
  (define-key global-map (kbd "s-c") 'kill-ring-save)
  (define-key global-map (kbd "s-w") 'delete-frame)
  (define-key global-map (kbd "s-x") 'kill-region))

;; Open init.el with C-x c
(defun initel () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x c") 'initel)

;; linux
(defun charles-linux-setup ()
  (set-frame-font "Inconsolata-11")
  (setq base-face-height 110)
  (setq frame-maximization-mode 'maximized)
  (charles-setup-keybindings))

;; OSX
(defun spotlight-locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

(defun charles-osx-setup ()
  (setq base-face-height 160)
  (setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'super)
  (setq helm-locate-command "mdfind -name %s %s")
  (setq locate-make-command-line 'spotlight-locate-make-command-line)
  (setq x-bitmap-file-path '("/usr/X11/include/X11/bitmaps"))
  (setq source-directory "/Library/Caches/Homebrew/emacs--git")
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open")))
  (setq frame-maximization-mode 'fullscreen)
  (charles-setup-keybindings))

(cond ((equal system-type 'gnu/linux) (charles-linux-setup))
      ((equal system-type 'darwin) (charles-osx-setup)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; flx/ido
;; (require 'ido-completing-read+)
;; (ido-ubiquitous-mode 1)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-faces nil
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c i") 'imenu-anywhere)

;; garbage collection
(setq gc-cons-threshold 50000000)

;; display & appearance
;; visible bell workaround for el capitan
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq inhibit-startup-message t)
(setq color-theme-is-global t)
(setq bidi-display-reordering nil)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(show-paren-mode 1)
(column-number-mode 1)
(hl-line-mode t)
(global-display-line-numbers-mode)

;; show time and battery status in mode line
(display-time-mode 1)
(setq display-time-format "%H:%M")
(display-battery-mode -1)

;; whitespace
(setq sentence-end-double-space nil)
(setq shift-select-mode nil)
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; mark region commands as safe
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; text mode tweaks
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'smart-spacing-mode)

;; file visiting stuff
(setq save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq recentf-max-saved-items 100)

(global-auto-revert-mode t)

;; other niceties
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches "-u")
(setq ispell-dictionary "en_GB")
(setq ispell-program-name "aspell")

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defalias 'yes-or-no-p 'y-or-n-p)

;; transparency
(defun set-current-frame-alpha (value)
  (interactive
   (list (read-number "Frame alpha: " 1.0)))
   (set-frame-parameter (selected-frame) 'alpha value))

(global-set-key (kbd "C-c t") 'set-current-frame-alpha)

;; fullscreen
(defcustom frame-maximization-mode 'maximized
  "The maximization style of \\[toggle-frame-maximized]."
  :type '(choice
          (const :tab "Respect window manager screen decorations." maximized)
          (const :tab "Ignore window manager screen decorations." fullscreen))
  :group 'frames)

(defun toggle-frame-maximized ()
  "Maximize/un-maximize Emacs frame according to `frame-maximization-mode'."
  (interactive)
  (modify-frame-parameters
   nil `((fullscreen . ,(if (frame-parameter nil 'fullscreen)
                            nil frame-maximization-mode)))))

(define-key global-map (kbd "<f11>") 'toggle-frame-maximized)

;; hide certain minor modes from mode line
(setq eldoc-minor-mode-string "")

;; solarized-dark theme
(load-theme 'solarized-dark t)
(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)

;; faces
(set-face-attribute 'default nil :height base-face-height :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :height base-face-height :family "Lucida Grande")
(require 'all-the-icons)

(require 'unicode-fonts)
(unicode-fonts-setup)

;; keybindings

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; handy shortcuts
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "C-c g") 'ag)
(global-set-key (kbd "C-c u") 'find-dired)

;; window navigation
(global-set-key (kbd "s-[") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-]") 'other-window)

(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)

;; Mac OS X-like
(global-set-key (kbd "s-z") 'undo)

(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<M-delete>") 'kill-word)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<s-backspace>") (lambda () (interactive) (kill-visual-line 0)))

;; telephone-line
(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-gradient
      telephone-line-secondary-left-separator 'telephone-line-nil
      telephone-line-primary-right-separator 'telephone-line-gradient
      telephone-line-secondary-right-separator 'telephone-line-nil)
(setq telephone-line-height 30
      telephone-line-evil-use-short-tag t)
(telephone-line-mode 1)

;; eshell
(setq eshell-aliases-file "~/.dotfiles/eshell-alias")
(global-set-key (kbd "C-c s") 'eshell)

(defun charles-eshell-mode-hook ()
  ;; config vars
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-save-history-on-exit t)
  (setq eshell-buffer-shorthand t)
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  ;; environment vars
  (setenv "EDITOR" "export EDITOR=\"emacsclient --alternate-editor=emacs --no-wait\"")
  ;; keybindings
  (define-key eshell-mode-map (kbd "<C-up>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<C-down>") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<up>") 'previous-line)
  (define-key eshell-mode-map (kbd "<down>") 'next-line)
  ;;faces
  (monokai-with-color-variables
    (set-face-attribute 'eshell-prompt nil :foreground monokai-orange))
  ;; prompt helpers
  (setq eshell-directory-name (concat user-emacs-directory "eshell/"))
  (setq eshell-prompt-regexp "^[^@]*@[^ ]* [^ ]* [$#] ")
  (setq eshell-prompt-function
        (lambda ()
          (concat (user-login-name) "@" (host-name) " "
                  (base-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " $ "))))
  ;; helpful bits and pieces
  (turn-on-eldoc-mode)
  (add-to-list 'eshell-command-completions-alist
               '("gunzip" "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
  (add-to-list 'eshell-visual-commands "ssh"))

(add-hook 'eshell-mode-hook 'charles-eshell-mode-hook)

(defun base-name (path)
  "Returns the base name of the given path."
  (let ((path (abbreviate-file-name path)))
    (if (string-match "\\(.*\\)/\\(.*\\)$" path)
        (if (= 0 (length (match-string 1 path)))
            (concat "/" (match-string 2 path))
          (match-string 2 path))
      path)))

(defun host-name ()
  "Returns the name of the current host minus the domain."
  (let ((hostname (downcase (system-name))))
    (save-match-data
      (substring hostname
                 (string-match "^[^.]+" hostname)
                 (match-end 0)))))

; elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;; files and stuff

; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)

;; dired
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)

;; LaTeX & reftex
(require 'latex)
(require 'reftex)

(defun charles-latex-mode-hook ()
  (setq TeX-master t)
  (setq TeX-PDF-mode t)
  (setq TeX-auto-untabify t)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (add-to-list 'auto-mode-alist '("\\.cls" . LaTeX-mode))
  ;; synctex
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  ;; latex keybindings
  (define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count)
  ;; reftex
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t)
  ;; reftex keybindings
  (define-key LaTeX-mode-map (kbd "C-c =") 'reftex-toc)
  (define-key LaTeX-mode-map (kbd "C-c c") 'reftex-citation)
  (define-key LaTeX-mode-map (kbd "C-c r") 'reftex-reference))
  ;; So that RefTeX finds my bibliography
  (setq reftex-default-bibliography '("~/Dropbox/writing/2013ComputerMusic.bib"))
  '(reftex-use-external-file-finders t)

(defun latex-word-count ()
  (interactive)
  (let* ((tex-file (if (stringp TeX-master)
		       TeX-master
		     (buffer-file-name)))
         (enc-str (symbol-name buffer-file-coding-system))
         (enc-opt (cond
                   ((string-match "utf-8" enc-str) "-utf8")
                   ((string-match "latin" enc-str) "-latin1")
                   ("-encoding=guess")))
         (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-1" "-merge" enc-opt tex-file)))))
    (message word-count)))

(add-hook 'LaTeX-mode-hook 'charles-latex-mode-hook)

;; markdown
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

(defun charles-markdown-mode-hook ()
  ;; unset these keys in markdown-mode-map
  (define-key markdown-mode-map (kbd "<M-left>") nil)
  (define-key markdown-mode-map (kbd "<M-right>") nil)
)

(add-hook 'markdown-mode-hook 'charles-markdown-mode-hook)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; git
(add-to-list 'auto-mode-alist '(".*gitconfig$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '(".*gitignore$" . conf-unix-mode))


;; Python Programming
;; Enable Elpy for Python development
;; https://elpy.readthedocs.io/en/latest/
(setq elpy-rpc-python-command "python3")
(elpy-enable)

;; Run black on save
(add-hook 'elpy-mode-hook (lambda ()
  (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))

;; Set C-8 to format Python code
(global-set-key (kbd "C-8") 'elpy-black-fix-code)

; from python.el
(require 'python)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args (if (equal system-type 'darwin)
			   "--matplotlib=osx --colors=Linux -i --simple-prompt"
                               (if (equal system-type 'gnu/linux) 
		       "--colors=Linux -i --simple-prompt"))
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; pyflakes flymake integration
;; http://stackoverflow.com/a/1257306/347942
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (unless (eq buffer-file-name nil) (flymake-mode 1))))

; Set PYTHONPATH, because we don't load .bashrc
(defun set-python-path-from-shell-PYTHONPATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))

(if window-system (set-python-path-from-shell-PYTHONPATH))

(setq auto-mode-alist
      (append 
       (list '("\\.pyx" . python-mode)
             '("SConstruct" . python-mode))
       auto-mode-alist))

; keybindings
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c !") 'python-shell-switch-to-shell))
(eval-after-load 'python
  '(define-key python-mode-map (kbd "C-c |") 'python-shell-send-region))




;; view pdfs in doc view
;; allows moving through documents from other windows. handy.
(fset 'doc-prev "\C-xo\C-x[\C-xo")
(fset 'doc-next "\C-xo\C-x]\C-xo")
(global-set-key (kbd "M-[") 'doc-prev)
(global-set-key (kbd "M-]") 'doc-next)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "<C-S-up>") 'mc/edit-lines)
(global-set-key (kbd "<C-S-down>") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "<C-S-right>") 'mc/mark-next-like-this)
(global-set-key (kbd "<C-S-left>") 'mc/mark-previous-like-this)

;; misc
(defun read-lines (fpath)
  "Return a list of lines of a file at at FPATH."
  (with-temp-buffer
    (insert-file-contents fpath)
    (split-string (buffer-string) "\n" t)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; from http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of `comment-dwim', when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "s-'") 'comment-dwim-line)

;; from
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs

(defun duplicate-line ()
  "Clone line at cursor, leaving the latter intact."
  (interactive "*")
  (save-excursion
    ;; The last line of the buffer cannot be killed
    ;; if it is empty. Instead, simply add a new line.
    (if (and (eobp) (bolp))
	(newline)
      ;; Otherwise kill the whole line, and yank it back.
      (let ((kill-read-only-ok t)
	    deactivate-mark)
	(read-only-mode 1)
	(kill-whole-line)
	(read-only-mode 0)
	(yank)))))

(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c b") 'comment-box)

;; toggle fullscreen

(if (display-graphic-p)
    (toggle-frame-maximized))

;; scroll one line at a time (less "jumpy" than defaults)
(setq scroll-step 1)
(setq scroll-conservatively 1000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
