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
;; (tool-bar-mode -1)
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

(load "~/.emacs.d/modes/haskell-mode/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))

;; Don't wrap lines in sql mode
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

;; Set up org mode
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/bugz.org" "~/org/devnotes.org" "~/org/donorjson.org" "~/org/notes.org" "~/org/tasks.org" "~/org/storage.org"))
(add-hook 'org-mode-hook (lambda () (global-set-key (kbd "M-/") 'hippie-expand)))

;; ;; LaTeX export of tex files
;; (require 'org-latex)
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
;; (add-to-list 'org-export-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))  

;; Put autosave files (eg #foo#) and backup files (eg foo~) in ~/.emacs.d/.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(initial-buffer-choice "~/org/tasks.org"))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(add-to-list 'load-path "/Users/fishkins/.emacs.d/modes")

;; ;; Set up latex mode
;; (setq reftex-plug-into-AUCTeX t)

;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
;; (setq tex-directory "c:/Users/cjudkins/desktop/TestingInstructions")

;; (setq backup-by-copying-when-linked t)

(setq delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  )

;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

;; Evil mode for better VIM
(add-to-list 'load-path "~/.emacs.d/modes/evil")
(require 'evil)
(evil-mode 1)
(setq evil-want-fine-undo t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
