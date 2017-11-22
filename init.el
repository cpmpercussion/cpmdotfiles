;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;: charles martin's .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;
;; processing mode ;;
;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;
;; elpa ;;
;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(unless package--initialized
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (package
         '(ac-nrepl
           ag
           auctex
           auto-complete
           bookmark+
           cider
           elpy
           epl
           ess
           exec-path-from-shell
           flx-ido
           gist
           htmlize
           ido-ubiquitous
           imenu-anywhere
           isearch+
           less-css-mode
           magit
           markdown-mode
           monokai-theme
           multiple-cursors
           org
           paredit
           pandoc-mode
           powerline
           scss-mode
           smex
           yaml-mode
           yasnippet))
  (if (not (package-installed-p package))
      (package-install package)))



(global-set-key (kbd "C-c p") 'list-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cross-platform setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("EDITOR" "EXT_LLVM_DIR" "LD_LIBRARY_PATH"))

;; linux

(defun charles-linux-setup ()
  (set-frame-font "Inconsolata-11")
  (setq base-face-height 110)
  (setq frame-maximization-mode 'maximized)
  (charles-setup-keybindings))

;; OSX
(defun spotlight-locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

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
  ;; for railwaycat emacs-mac
  (charles-setup-keybindings))

;; Windows
(defun charles-windows-setup ()
  (setq base-face-height 160)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (charles-setup-keybindings))

(cond ((equal system-type 'gnu/linux) (charles-linux-setup))
      ((equal system-type 'darwin) (charles-osx-setup))
      ((equal system-type 'windows-nt) (charles-windows-setup)))

;;;;;;;;;;;;;;;;;;;
;; customisation ;;
;;;;;;;;;;;;;;;;;;;
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;;;;;;;;;;;;;;;;
;; smex & ido ;;
;;;;;;;;;;;;;;;;
(require 'flx-ido)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c i") 'imenu-anywhere)

;;;;;;;;;;;;;;;;;;;;;;;;
;; garbage collection ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 50000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display & appearance ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visible bell workaround for el capitan
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
(invert-face 'mode-line)
(run-with-timer 0.1 nil 'invert-face 'mode-line)))
;;(setq visible-bell t)

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

;; pretty lambdas

(add-hook 'prog-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil `(("(?\\(lambda\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 107))
                               nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solarized-dark theme ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(if (display-graphic-p)
    (progn (load-theme 'solarized t)
           (add-to-list 'default-frame-alist
                        '(background-mode . dark))
           (set-cursor-color "white")))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;

(set-face-attribute 'default nil :height base-face-height :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :height base-face-height :family "Lucida Grande")

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;; handy shortcuts

(global-set-key (kbd "<f5>") 'magit-status)
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

;;;;;;;;;;;;;;;
;; powerline ;;
;;;;;;;;;;;;;;;

(require 'powerline)

(setq powerline-default-separator 'slant)
(setq powerline-height 30)

;(powerline-default-theme)

(defun powerline-charles-theme ()
  "Charles' powerline theme, based on \\[powerline-default-theme]"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-charles-theme)

;;;;;;;;;;;;
;; eshell ;;
;;;;;;;;;;;;

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

;;;;;;;;;;;
;; elisp ;;
;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;

(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;

(setq vc-display-status nil)
(setq magit-save-some-buffers nil)

;;;;;;;;;;;;;
;; cc-mode ;;
;;;;;;;;;;;;;

;; (setq c-default-style "k&r")

;;;;;;;;;;;;;
;; ebrowse ;;
;;;;;;;;;;;;;

(defun charles-ebrowse-set-faces ()
  (set-face-attribute 'ebrowse-root-class nil :foreground nil :inherit font-lock-type-face)
  (set-face-attribute 'ebrowse-member-class nil :foreground nil :inherit font-lock-function-name-face)
  (set-face-attribute 'ebrowse-member-attribute nil :foreground nil :inherit font-lock-string-face))

(add-hook 'ebrowse-tree-mode 'charles-ebrowse-set-faces)

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(require 'org)

(setq org-completion-use-ido t)
(setq org-export-with-toc nil)

(defun charles-org-mode-hook ()
  ;; ;; org-latex export
  ;; (add-to-list 'org-export-latex-classes
  ;;              '("scrartcl"
  ;;                "\\documentclass[12pt,a4paper]{scrartcl}"
  ;;                ("\\section{%s}" . "\\section*{%s}")
  ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; (setq org-export-latex-default-class "scrartcl")
  ;; keymappings
  (define-key org-mode-map (kbd "<M-left>") 'backward-word)
  (define-key org-mode-map (kbd "<M-right>") 'forward-word)
  (define-key org-mode-map (kbd "<C-left>") 'org-metaleft)
  (define-key org-mode-map (kbd "<C-right>") 'org-metaright)
  ;; stop org-mode shadowing the mc keybindings
  (define-key org-mode-map (kbd "<C-S-up>") nil)
  (define-key org-mode-map (kbd "<C-S-down>") nil)
  (define-key org-mode-map (kbd "<C-S-right>") nil)
  (define-key org-mode-map (kbd "<C-S-left>") nil))

(add-hook 'org-mode-hook 'charles-org-mode-hook)

;; (setq org-format-latex-header
;;       "\\documentclass[12pt,a4paper]{scrartcl}
;; \\usepackage{libertineotf}
;; \\usepackage{fontspec}
;; \\setmonofont[Scale=MatchLowercase,Mapping=tex-text]{Source Code Pro}

;; \\usepackage{booktabs}
;; \\usepackage{tabularx}
;; \\renewcommand{\\arraystretch}{1.2}

;; % biblatex

;; \\usepackage[%
;; backend=biber,
;; natbib=true,
;; backref=true,
;; citecounter=true,
;; dashed=false,
;; backrefstyle=three,
;; citestyle=authoryear-icomp,
;; firstinits=true,
;; maxcitenames=2,
;; maxbibnames=10,
;; uniquename=mininit,
;; bibstyle=authoryear,
;; % refsegment=chapter,
;; % ibidtracker=strict,
;; url=false,
;; doi=false]{biblatex}

;; % to use year-only bib format
;; \\AtEveryBibitem{\\clearfield{month}}
;; \\AtEveryCitekey{\\clearfield{month}}

;; % specify the bib file here
;; \\addbibresource{papers.bib}

;; % IMPORTANT: to actually print the bibliography in the document,
;; % insert the command: \\printbibliography[title=References]

;; % csquotes

;; \\usepackage[english=british,threshold=15,thresholdtype=words]{csquotes}
;; \\SetCiteCommand{\\parencite}

;; \\newenvironment*{smallquote}
;;   {\\quote\\small}
;;   {\\endquote}
;; \\SetBlockEnvironment{smallquote}

;; % hyperref & bookmark

;; \\usepackage[svgnames,hyperref]{xcolor}

;; \\usepackage[%
;; unicode=true,
;; hyperindex=true,
;; bookmarks=true,
;; colorlinks=true, % change to false for final
;; pdfborder=0,
;; allcolors=DarkBlue,
;; % plainpages=false,
;; pdfpagelabels,
;; hyperfootnotes=true]{hyperref}

;; ")

;;;;;;;;;
;; erc ;;
;;;;;;;;;

(erc-services-mode 1)
(setq erc-nick "charlesmatarles")
(setq erc-autojoin-channels-alist '(("freenode.net" "#extempore")))

;;;;;;;;;;;;;;;;;;;;
;; LaTeX & reftex ;;
;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;
;; extempore ;;
;;;;;;;;;;;;;;;

(setq user-extempore-directory "/Applications/extempore/")
(autoload 'extempore-mode (concat user-extempore-directory "extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; extempore customisation
(setq extempore-tab-completion nil)

(defun charles-extempore-mode-hook ()
  (turn-on-eldoc-mode)
  (setq eldoc-documentation-function
        'extempore-eldoc-documentation-function)
  (yas-minor-mode-on)
  ;; (if (and (not extempore-logger-mode)
  ;;          (yes-or-no-p "Do you want to log this session?"))
  ;;     (extempore-logger-mode 1))
  )

(add-hook 'extempore-mode-hook 'charles-extempore-mode-hook)

(autoload #'llvm-mode (concat user-extempore-directory "extras/llvm-mode.el")
  "Major mode for editing LLVM IR files" t)

(add-to-list 'auto-mode-alist '("\\.ir$" . llvm-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode))

;; session setup

(defun charles-create-extempore-template-file (base-path filename &optional header)
  (let ((full-path (format "%s/%s" base-path filename)))
    (unless (file-exists-p full-path)
      (progn
        (find-file full-path)
        (if header (insert header))
        (save-buffer)
        (kill-buffer)))))

(defun charles-create-extempore-template-dir (name)
  "Set up the directory structure and files for a new extempore session/gig."
  (interactive "sSession name: ")
  (let* ((xtm-dir (expand-file-name "~/Code/xtm/"))
         (base-path (concat xtm-dir "sessions/" name))
         (setup-header
          (concat ";;; setup.xtm --- setup file for " name "\n"
                  "(sys:load \"libs/xtm.xtm\")\n"
                  "(sys:load \"" xtm-dir "lib/charles-lib.xtm\")\n"
                  "(ipc:load \"utility\" \"" xtm-dir "lib/charles-lib.xtm\")\n"
                  "(sys:load \"" xtm-dir "lib/sampler-maps.xtm\")\n"
                  "(ipc:load \"utility\" \"" xtm-dir "lib/sampler-maps.xtm\")\n")))
    (if (file-exists-p base-path)
        (error "Cannot create xtm session: directory already exists."))
    (make-directory base-path)
    ;; practice files
    (charles-create-extempore-template-file
     base-path "prac-utility.xtm" "headeru")
    (charles-create-extempore-template-file
     base-path "prac-primary.xtm" "headerp")
    ;; gig files
    (charles-create-extempore-template-file
     base-path "gig-utility.xtm" "headeru")
    (charles-create-extempore-template-file
     base-path "gig-primary.xtm" "headerp")
    ;; setup file
    (charles-create-extempore-template-file
     base-path "setup.xtm" setup-header)
    (dired base-path)))

;;;;;;;;;;;;;
;; paredit ;;
;;;;;;;;;;;;;

;; from https://gist.github.com/malk/4962126

(defun point-is-inside-list ()
  "Whether point is currently inside list or not."
  (nth 1 (syntax-ppss)))

(defun point-is-inside-string ()
  "Whether point is currently inside string or not."
  (nth 3 (syntax-ppss)))

(defun point-is-inside-comment ()
  "Whether point is currently inside a comment or not."
  (nth 4 (syntax-ppss)))

(defun paredit--is-at-opening-paren ()
  (and (looking-at "\\s(")
       (not (point-is-inside-string))
       (not (point-is-inside-comment))))

(defun paredit-skip-to-start-of-sexp-at-point ()
  "Skips to start of current sexp."
  (interactive)
  (while (not (paredit--is-at-opening-paren))
    (if (point-is-inside-string)
        (paredit-backward-up)
      (paredit-backward))))

(defun paredit-duplicate-rest-of-closest-sexp ()
  (interactive)
  (cond
   ((paredit--is-at-opening-paren)
    (paredit-copy-sexps-as-kill)
    (forward-sexp)
    (paredit-newline)
    (yank)
    (exchange-point-and-mark))
   ((point-is-inside-list)
    (while (looking-at " ") (forward-char))
    (if (not (= (point) (car (bounds-of-thing-at-point 'sexp))))
        (progn (forward-sexp)
               (while (looking-at " ") (forward-char))))
    (let ((sexp-inside-end (- (paredit-next-up/down-point 1 1) 1)))
      (kill-ring-save (point) sexp-inside-end)
      (goto-char sexp-inside-end))
    (paredit-newline)
    (yank)
    (exchange-point-and-mark))))

(defface paredit-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face for parentheses.  Taken from ESK.")

(defun charles-paredit-mode-hook ()
  (define-key paredit-mode-map (kbd "<M-delete>") 'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "<M-backspace>") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "<s-left>") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "<s-S-left>") 'paredit-backward-down)
  (define-key paredit-mode-map (kbd "<s-right>") 'paredit-forward-up)
  (define-key paredit-mode-map (kbd "<s-S-right>") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "<M-S-up>") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "<M-S-down>") 'paredit-wrap-sexp)
  (define-key paredit-mode-map (kbd "<M-S-left>") 'paredit-convolute-sexp)
  (define-key paredit-mode-map (kbd "<M-S-right>") 'transpose-sexps)
  (define-key paredit-mode-map (kbd "<s-S-down>") 'paredit-duplicate-rest-of-closest-sexp))

(add-hook 'paredit-mode-hook 'charles-paredit-mode-hook)

;; turn on paredit by default in all 'lispy' modes

(dolist (mode '(scheme emacs-lisp lisp clojure cider-repl clojurescript extempore))
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                            '(("(\\|)" . 'paredit-paren-face))))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'paredit-mode))

;; taken from
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/

(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(eval-after-load "paredit"
  '(cl-nsubstitute-if '(paredit-mode " pe")
                      (lambda (x) (equalp (car x) 'paredit-mode))
                      minor-mode-alist))

;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

;; cider

;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-use-pretty-printing nil)

;;;;;;;;;;;;;;
;; markdown ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

(defun charles-markdown-mode-hook ()
  ;; unset these keys in markdown-mode-map
  (define-key markdown-mode-map (kbd "<M-left>") nil)
  (define-key markdown-mode-map (kbd "<M-right>") nil))

(add-hook 'markdown-mode-hook 'charles-markdown-mode-hook)

(add-hook 'markdown-mode-hook 'pandoc-mode)

;;;;;;;;;;
;; yaml ;;
;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;
;; git ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '(".*gitconfig$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '(".*gitignore$" . conf-unix-mode))

;;;;;;;
;; R ;;
;;;;;;;

(require 'ess-site)

(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

;------------------------;
;;; Python Programming ;;;
;------------------------;

;; -----------------------
;; python.el configuration
;; -----------------------

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

;; -----------------------------
;; emacs ipython notebook config
;; -----------------------------

;;(package-initialize)
(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)

; use autocompletion, but don't start to autocomplete after a dot
;(setq ein:complete-on-dot -1)
;(setq ein:use-auto-complete 1)

; set python console args
;(setq ein:console-args '("--gui=osx" "--matplotlib=osx" "--colors=Linux"))

;(setq ein:console-args
;      (if (system-is-mac)
;	  '("--gui=osx" "--matplotlib=osx" "--colors=Linux")
;	(if (system-is-linux)
;	    '("--gui=wx" "--matplotlib=wx" "--colors=Linux"))))

; timeout settings
;(setq ein:query-timeout 1000)

; IPython notebook
;(require 'ein)

; shortcut function to load notebooklist
;; (defun load-ein () 
;;   (ein:notebooklist-load)
;;   (interactive)
;;   (ein:notebooklist-open))


;; ------------------
;; misc python config
;; ------------------

; pydoc info

;; (require 'pydoc-info)

;; ; jedi python completion
;; (include-elget-plugin "ctable")   ; required for epc
;; (include-elget-plugin "deferred") ; required for epc
;; (include-elget-plugin "epc")      ; required for jedi
;; (include-elget-plugin "jedi")
;; (require 'jedi)
;; (setq jedi:setup-keys t)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)

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

;; elpy setup

;; (elpy-enable)
;; (setq elpy-rpc-backend 'jedi)
;; (setq python-indent-offset 2)

;; (setq  elpy-default-minor-modes
;;        '(eldoc-mode
;;          flymake-mode
;;          ;; highlight-indentation-mode
;;          auto-complete-mode))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(require 'yasnippet)

(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode 1)

(eval-after-load "yasnippet"
  '(cl-nsubstitute-if '(yas-minor-mode "")
                      (lambda (x) (equalp (car x) 'yas-minor-mode))
                      minor-mode-alist))

;;;;;;;;;;;;;;;;;;
;; autocomplete ;;
;;;;;;;;;;;;;;;;;;

;; autocomplete needs to be set up after yasnippet

(require 'auto-complete-config)

;; (ac-set-trigger-key "<tab>")
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory ".ac-dict"))
(setq ac-auto-start 2)
(ac-config-default)

(eval-after-load "auto-complete"
  '(cl-nsubstitute-if '(auto-complete-mode "")
                      (lambda (x) (equalp (car x) 'auto-complete-mode))
                      minor-mode-alist))

;; for using Clang autocomplete

;; (require 'auto-complete-clang)

;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang ac-source-yasnippet ac-source-gtags) ac-sources)))

;; (defun my-ac-config ()
;;   (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

;;;;;;;;;;;;;;;;;;;;;;;
;; ttl (Turtle) mode ;;
;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
(add-to-list 'auto-mode-alist '("\\.n3" . ttl-mode))
(add-to-list 'auto-mode-alist '("\\.ttl" . ttl-mode))
(add-hook 'ttl-mode-hook 'turn-on-font-lock)

;;;;;;;;;
;; abc ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.abc\\'"  . abc-mode))
(add-to-list 'auto-mode-alist '("\\.abp\\'"  . abc-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors ;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

(global-set-key (kbd "<C-S-up>") 'mc/edit-lines)
(global-set-key (kbd "<C-S-down>") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "<C-S-right>") 'mc/mark-next-like-this)
(global-set-key (kbd "<C-S-left>") 'mc/mark-previous-like-this)

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;
;; emacs server ;;
;;;;;;;;;;;;;;;;;;

(require 'server)
;; (setq server-name "charles")

(unless (server-running-p)
  (server-start))

;; toggle fullscreen

(if (display-graphic-p)
    (toggle-frame-maximized))

;;;;;;;;;;;;;;;;;;
;; Scrolling    ;;
;;;;;;;;;;;;;;;;;;
;; scroll one line at a time (less "jumpy" than defaults)
(setq scroll-step 1)
(setq scroll-conservatively 1000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)




