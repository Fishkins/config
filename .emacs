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
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))))

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
(package-initialize)
;; Installed packages:
;; evil
;; yaml-mode
;; haskell-mode
;; clojure-mode
;; gitconfig-mode
;; nrepl
;; paredit
;; erlang

;; SQL mode config
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

(setq tramp-default-method "ssh")

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
;; (add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; yaml editing
;; (add-to-list 'load-path "/Users/fishkins/.emacs.d/modes/yaml-mode")
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Set up org mode
(add-hook 'org-mode-hook 'visual-line-mode)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(add-hook 'org-mode-hook (lambda () (global-set-key (kbd "M-/") 'hippie-expand)))
(setq org-src-fontify-natively t)

;; ;; LaTeX export of tex files
;; (require 'org-latex)
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
;; (add-to-list 'org-export-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))  


(add-to-list 'load-path "/Users/fishkins/.emacs.d/modes")

;; ;; Set up latex mode
;; (setq reftex-plug-into-AUCTeX t)
;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;; (setq tex-directory "c:/Users/cjudkins/desktop/TestingInstructions")

;; (setq backup-by-copying-when-linked t)


;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  )

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

(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
