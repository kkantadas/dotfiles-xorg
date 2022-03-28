;; Initialize package sources
  (require 'package)

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t)
;;  (setq use-package-verbose t)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun kk/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

 ;; display this startuptime at Startup
 ;; (add-hook 'emacs-startup-hook #'kk/display-startup-time)

(defun kk/display-Gouranga ()
    (message " "))

    (add-hook 'emacs-startup-hook #'kk/display-Gouranga)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;Jump back to the minibuffer 
(global-set-key (kbd "C-c m") 'select-minibuffer)

(use-package general
  :after evil
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package windresize
  :bind ("C-c r" . windresize))

(defun kk/split-horizontal ()
  "Split a window and open Ibuffer to the next window"
  (interactive)
  (split-window-vertically)
  (other-window '1)
  (ibuffer))

(global-set-key (kbd "C-c s")
                '(lambda () (interactive)
                   (kk/split-horizontal)))

(defun kk/split-vertical ()
  "Split a window and open the Ibuffer to the next window"
  (interactive)
  (split-window-horizontally)
  (other-window '1)
  (ibuffer))

(global-set-key (kbd "C-c S")
                '(lambda () (interactive)
                   (kk/split-vertical)))

(defun kk/small-split ()
  (interactive)
  (kk/split-horizontal)
  (kk/split-horizontal)
  (split-window-vertically)
  (delete-window)
  (delete-window)
  (windmove-down))

(global-set-key (kbd "s-x S")
                '(lambda () (interactive)
                   (kk/small-split)))

(use-package windmove
  :config
  (setq windmove-create-window nil) 
  :bind (("C-c <S-up>" . windmove-swap-states-up)
         ("C-c <S-right> " . windmove-swap-states-right)
         ("C-c <S-down>" . windmove-swap-states-down)
         ("C-c <S-left>" . windmove-swap-states-left)))


;; (use-package windmove
;;   :config
;;   (setq windmove-create-window nil) 
;;   :bind (("s-<S-up>" . windmove-swap-states-up)
;;          ("s-<S-right> " . windmove-swap-states-right)
;;          ("s-<S-down>" . windmove-swap-states-down)
;;          ("s-<S-left>" . windmove-swap-states-left)))

(use-package windmove
  :config
  (setq windmove-create-window nil) 
  :bind (("C-c <up>" . windmove-up)
         ("C-c <right> " . windmove-right)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)))

(use-package emacs
  :config
  (defvar kk/window-configuration nil
    "Current window configuration.
    Intended for use by `kk/window-monocle'.")

  (defun kk/window-single-toggle ()
    "Toggle between multiple windows and single window.
    This is the equivalent of maximising a window.  Tiling window
    managers such as DWM, BSPWM refer to this state as 'monocle'."
    (interactive)
    (if (one-window-p)
        (when kk/window-configuration
          (set-window-configuration kk/window-configuration))
      (setq kk/window-configuration (current-window-configuration))
      (delete-other-windows))))
                                        ;:bind ("s-m" . kk/window-single-toggle)

;;Replacing Keybindings Portably
(substitute-key-definition 'delete-other-windows 'kk/window-single-toggle global-map)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
                                        ;(mouse-avoidance-mode 'banish)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(show-paren-mode 1)
(menu-bar-mode -1)          ; Disable the menu bar

                                        ;; hide messages and info buffers in ivy
                                        (setq ivy-ignore-buffers '("\\` " "\\`\\*"))

(global-set-key (kbd "C-c c") 'kk/scratch)
(defun kk/scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


;; Set up the visible bell
(setq visible-bell t)

                                        ; (column-number-mode)
                                        ; (global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "C-c m") 'select-minibuffer)

(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)
(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
                                        ;/;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-font-size :weight 'regular)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-default 'curser-type 'hbar)
(fset 'yes-or-no-p 'y-or-n-p)


(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OS

(use-package centered-cursor-mode
  :diminish centered-cursor-mode)

(defun jib/pulse-area (&rest _)
  "Pulse +-5 chars of point."
  (pulse-momentary-highlight-region (- (point) 5) (+ 5 (point))))

(dolist (command '(org-forward-sentence org-backward-sentence))
  (advice-add command :after #'pulse-area))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :bind ("s-x x" . dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))


;; Do not show details of dired
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("html" . "firefox")
                                ("mp4" . "mpv")
                                ("mkv" . "mpv")
                                ("webm" . "mpv")
                                ("mp3" . "mpv"))))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
(setq system-trash-exclude-matches '("#[^/]+#$" ".*~$" "\\.emacs\\"))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :ensure
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-right>" . dired-subtree-toggle)
              ("<C-left>" . dired-subtree-cycle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))


;;Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(define-key dired-mode-map (kbd "s-?") 'dired-get-size)

;;highlight line only in dired and other modes
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook 'hl-line-mode)

(setq savehist-file "~/.emacs.d/savehist")
(setq savehist-additional-variables
      '(buffer-name-history
        compile-command
        extended-command-history
        file-name-history
        kill-ring
        regexp-search-ring
        search-ring))
'(save-place-limit 200)
(savehist-mode 1)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
                                        ;(setq global-auto-revert-non-file-buffers t)
                                        ;(setq auto-revert-verbose nil)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package writeroom-mode
  :bind (("s-s" . writeroom-mode)
         ("s-," . writeroom-increase-width)
         ("s-." . writeroom-decrease-width)))

(use-package ereader
  :after dired)

(use-package ereader
  :after dired
  :magic ("%epub" . ereader-mode))


(use-package pdf-tools
  :after dired
  :config
  (pdf-tools-install))

;;(global-visual-line-mode t) 
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'ereader-mode-hook 'visual-line-mode)

;;select active minibuffer if unfocused 
(defun select-minibuffer ()
  "Make the active minibuffer the selected window."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(save-place-mode 1)

(use-package all-the-icons)

                                        ;(use-package doom-modeline
                                        ; :init (doom-modeline-mode 1)
                                        ; :custom ((doom-modeline-height 15)))

;;(set-face-attribute 'mode-line-inactive nil
;;                   :underline t
;;                 :background (face-background 'default))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package    feebleline
  :ensure       t
  :config       (setq feebleline-msg-functions
                      '((feebleline-line-number         :post "" :fmt "%5s")
                        (feebleline-column-number       :pre ":" :fmt "%-2s")
                        (feebleline-file-directory      :face feebleline-dir-face :post "")
                        (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
                        (feebleline-file-modified-star  :face font-lock-warning-face :post "")
                        (feebleline-git-branch          :face feebleline-git-face :pre " : ")
                        (feebleline-project-name        :align right)))
  (feebleline-mode 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :diminish
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         ("M-x" . 'counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("TEXT" (mode . text-mode))
               ("ORG" (mode . org-mode))
               ("EBOOK" (name . "^.*epub$"))
               ("PDF" (name . "^.*pdf$"))
               ("Editor" (or (name . "^.*sh$") (name . "^.*html$")
                             (name . "^.*js$") (name . "^.*css$")
                             (name . "^.*el$") (name . "^.*txt$")))
               ("Web" (or (mode . web-mode) (mode . js2-mode)))
               ("Edit" (or (mode . bash-mode) (mode . lisp-mode)))
               ("Images" (or (mode . picture-mode) (mode . image-mode)))
               ("Shell" (or (mode . eshell-mode) (mode . shell-mode)
                            (mode . python-mode) (mode . c++-eode)))

               ("Shell" (name . "\*vterm\*"))
               ("Magit" (name . "magit\*"))
               ("Media" (name . "mpv\*"))
               ("Web" (name . "firefox\*"))
               ("Info" (or
                        (name . "^\\*help\\*$")
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Buffer list\\*$")
                        (name . "^\\*directory\\*$")
                        (name . "^\\*bookmark list\\*$")
                        (name . "^\\*Compile-Log\\*$")
                        (name . "^\\*Packages\\*$")
                        (name . "^\\*directory\\*$")
                        (name . "^\\*backtrace\\*$")
                        (name . "^\\*Dired log\\*$")
                        (name . "^\\*Disabled Command\\*$")
                        (name . "^\\*Shell Command Output\\*$")
                        (name . "^\\*Messages\\*$")))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
                                        ;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)

(defun kk/sudo-find-file (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))

(global-set-key (kbd "C-c f") 'kk/sudo-find-file)

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))
(global-set-key (kbd "C-c t") 'vterm)
(global-set-key (kbd "C-c T") 'kk/vterm-more)
(global-set-key (kbd "s-x s") 'kk/pop-shell)
(global-set-key (kbd "C-c u") 'universal-argument)

(defun kk/vterm-more ()
  (interactive)
  (if (string= "*vterm*" (buffer-name))
      (rename-uniquely))
  (vterm "vterm"))

(defun kk/pop-shell (arg)
  "Pop a shell in a side window.
 Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'vterm))
      (current-buffer))
    '((side . bottom)))))

(exwm-input-set-key (kbd "s-x X")
                    (lambda () (interactive) (start-process "" nil "xranger.sh")))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun kk/suspend ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (sit-for 2)
  (shell-command "sudo systemctl suspend")
  (message "...Z.Z.Z.Z.z.z.z.z... "))

(exwm-input-set-key (kbd "C-c )") 'kk/suspend) 

                                        ; (exwm-input-set-key (kbd "C-c )")
                                        ;                    (lambda () (interactive) (shell-command "sudo systemctl suspend")))

(exwm-input-set-key (kbd "s-x w")
                    (lambda () (interactive) (start-process "" nil "firefox")))



(custom-set-variables
 '(exwm-manage-configurations
   '((t
      floating-mode-line nil tiling-mode-line nil)
     ((and
       (stringp exwm-class-name)
       (string-match-p "Firefox" exwm-class-name))
      whatever whatever))))
                                        ;second option
                                        ; (custom-set-variables
                                        ; '(exwm-manage-configurations
                                        ;  '(((and
                                        ;     (stringp exwm-class-name)
                                        ;    (string-match-p "Firefox" exwm-class-name))
                                        ;  floating-mode-line nil tiling-mode-line nil))))

(exwm-input-set-key (kbd "s-x j")
                    (lambda () (interactive) (start-process "" nil "alsamix.sh")))

(exwm-input-set-key (kbd "s--")
                    (lambda () (interactive) (shell-command "amixer set Master 5%-")))
(exwm-input-set-key (kbd "s-=")
                    (lambda () (interactive) (shell-command "amixer set Master 5%+")))
(exwm-input-set-key (kbd "s-0")
                    (lambda () (interactive) (shell-command "amixer set Master 1+ toggle")))

;; Print screen
(global-set-key (kbd "s-x m")
                (lambda ()
                  (interactive)
                  (let ((path (concat "~/.screenshot/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
                    (start-process-shell-command
                     "import" nil (concat "import -window root " path))
                    (message (concat "Screenshot saved to " path)))
                  ))

(exwm-input-set-key (kbd "s-R")
                    (lambda () (interactive)
                      (shell-command "rm -r /home/kanta/.Trash/{*,.[^.]*}")
                      (message "Trash is freed")))

(defun kk/empty ()
  "Remove the Trash ; Super-R"
  (interactive)
  (shell-command "rm -r /home/kanta/.Trash/{*,.[^.]*}")
  (message "Free Trash  ... or press Super-R"))

(defun internet-up-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))  

(defun kk/internet-connect ()
  (interactive)
  (message (if (internet-up-p) "Up" "Down")))

;;  (global-set-key (kbd "C-c a") 'kk/internet-connect)


(exwm-input-set-key (kbd "s--")
                    (lambda () (interactive) (shell-command "amixer set Master 5%-")))


(defun kk/sys-info()
  (interactive)
  (shell-command "echo  Net:`iw wlan0 info|grep ssid|cut -c6-50` `cat /sys/class/net/enp2s0f1/operstate` .. Mem: `free -h | grep Mem | cut -c27-30` ... Themp:`acpi -t|cut -c15-19`C ... `date '+Time: %H:%M '` ...  Date: `date|cut -c1-10 ` ")
  (sit-for 6)
  (message " "))  

(exwm-input-set-key (kbd "s-x a") 'kk/sys-info)

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (centered-cursor-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (message "Org Mode is loaded")
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Emacs-project/Org/Tasks.org"
          "~/Emacs-project/Org/Habits.org"
          "~/Emacs-project/Org/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "/Emacs-project/Org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "/Emacs-project/Org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "/Emacs-project/Org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "/Emacs-project/Org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "/Emacs-procject/Org/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))



;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.emacs.d/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package auto-complete
  :commands auto-complete-mode
  :init
  (progn
    (auto-complete-mode t))
  :config
  (progn 
                                        ; (use-package auto-complete-config)

    (ac-set-trigger-key "TAB")
    (ac-config-default)

    (add-to-list 'ac-modes 'text-mode)
    (setq ac-delay 0.02)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 50)
    (setq ac-use-quick-help nil) 
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim  t)
    (setq ac-fuzzy-enable t)
    )) 



                                        ;  (use-package auto-complete
                                        ;   :ensure t
                                        ;  :init
                                        ;    (progn
                                        ;     (ac-config-default)
                                        ;    (add-to-list 'ac-modes 'org-mode 'name-of-mode)
                                        ;   (global-auto-complete-mode t)))
                                        ;
                                        ;       ;; switch auto-complet on or off 
                                        ;
(global-set-key (kbd "M-<tab>") 'auto-complete-mode)

;; enable IDE-like functionality 
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; a set of UI enhancements 
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; provides nice tree views
(use-package lsp-treemacs
  :after lsp)

;;integrates Ivy with lsp-mode to make it easy to search
(use-package lsp-ivy
  :after lsp)

;;  basic configuration for the TypeScript
;;Important note! For lsp-mode to work with TypeScript (and JavaScript) you will need to install a language server on your machine. If you have Node.js installed, the easiest way to do that is by running the following command:

;;npm install -g typescript-language-server typescript

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package prescient
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000) ;; More prescient history
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after ivy
  :config
  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

(use-package company-prescient
  :defer 2
  :after company
  :config
  (company-prescient-mode +1))

;;provides a nicer in-buffer completion interface

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(add-hook 'magit-mode-hook
          (lambda ()
            (let ((dir (abbreviate-file-name default-directory)))
              (setq list-buffers-directory dir))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  (require 'exwm-randr)
  (exwm-randr-enable)

  (start-process-shell-command "xrandr" nil "xrandr --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP1 --off --output HDMI1 --off --output VIRTUAL1 --off")

  ;; Load the system tray before exwm-init
                                        ;   (require 'exwm-systemtray)
                                        ;  (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\C-c
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-:] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))
