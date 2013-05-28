;; Map meta to cmd
(setq ns-command-modifier 'meta)
;; No start-up screen
(setq inhibit-startup-screen t)
;; Use spaces instead of tab chars, make tabs 4 spaces
(setq tab-width 4)
;; Never require typing full yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; Highlight matching parens
(show-paren-mode 1)
;; Hide toolbar & Menu - moved to registry
(when window-system (tool-bar-mode -1))
;; (menu-bar-mode -1)
;; Maintain line position when scrolling up then down a screen
(setq scroll-preserve-screen-position t)
;; Unbind ctrl-z
(global-unset-key [(control z)])
;; Use ctrl-; and ctrl-: to comment/uncomment regions
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
;; Show col number
(column-number-mode 1)
;; Open files in new buffer instead of frame
(setq ns-pop-up-frames nil)
;; Maintain margin
(setq scroll-margin 5)

;; If no region is set, comment/uncomment current line
(defadvice comment-region (before linecomment)
  "Comments current line if region isn't selected"
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2 )))))

(defadvice uncomment-region (before lineuncomment)
  "Uncomments current line if region isn't selected."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2 )))))

;; If not region is set, copy/kill current line
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Put autosave files (eg #foo#) and backup files (eg foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-trigger-commands nil)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(initial-buffer-choice "~/org/tasks.org"))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(setq delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)

;; Allow up/downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Only provide default buffer when opening GUI. Command line will probably be passed a filename.
(when window-system (custom-set-variables '(initial-buffer-choice "~/org/tasks.org")))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
;; Installed packages:
;; magit
;; evil
;; yaml-mode
;; haskell-mode
;; clojure-mode
;; gitconfig-mode
;; nrepl
;; auto-complete
;; ac-nrepl
;; paredit
;; erlang
;; color-theme-solarized
;; helm
;; org-mode

;; SQL mode config
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

(setq tramp-default-method "ssh")

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

;; Set up org mode
(add-hook 'org-mode-hook 'visual-line-mode)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-src-fontify-natively t)

;; Evil mode config
(evil-mode 1)
(setq evil-want-fine-undo t)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Custom macro to eval clojure fn in nrepl
(defun add-clojure-eval-fn ()
  (fset 'mycloj-eval-fun
	(lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([71 63 41 13 97 3 5 escape] 0 "%d")) arg))))
(add-hook 'clojure-mode-hook 'add-clojure-eval-fn)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (auto-complete-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (local-set-key (kbd "M-/") #'auto-complete)))

(add-hook 'shell-mode-hook (lambda () (evil-local-mode 0)))

(setq ispell-program-name "/usr/local/bin/ispell")

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")

(add-hook 'nrepl-mode-hook (lambda () (auto-complete-mode 1)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; End sentences with 1 space
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; bigger font
(set-face-attribute 'default nil :height 165)

;; Reopen files on restart
(desktop-save-mode 1)

;; Helm config (like Goto Anything)
(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))
