;; Map meta to cmd
(setq ns-command-modifier 'meta)
;; No start-up screen
(setq inhibit-startup-screen t)
;; Make tabs 4 spaces
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'nxml-child-indent 'tab-width)
(setq c-basic-indent 4)
;; Never require typing full yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; Highlight matching parens
(show-paren-mode 1)
;; Hide toolbar & Menu - moved to registry
(when window-system (tool-bar-mode -1))
(menu-bar-mode -1)
;; Maintain line position when scrolling up then down a screen
(setq scroll-preserve-screen-position t)
;; Unbind ctrl-z
(global-unset-key [(control z)])
;; Show col number
(column-number-mode 1)
;; Open files in new buffer instead of frame
(setq ns-pop-up-frames nil)
;; Maintain margin
;; (setq scroll-margin 4)
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-;") 'evilnc-comment-or-uncomment-lines)

;; Put autosave files (eg #foo#) and backup files (eg foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-trigger-commands nil)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(tab-width 4)
 '(vc-follow-symlinks t)
 '(vcl-indent-tabs-mode t)
 '(visible-bell nil))

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
;; (when window-system (custom-set-variables '(initial-buffer-choice "~/org/tasks.org")))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-pinned-packages '(color-theme . "melpa") t)
(add-to-list 'package-pinned-packages '(color-theme-solarized . "melpa") t)
(package-initialize)

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(
   auto-complete
   cider
   clojure-mode
   company
   erlang
   evil
   evil-numbers
   evil-leader
   evil-nerd-commenter
   gitconfig-mode
   haskell-mode
   helm
   magit
   org
   paredit
   popup
   scala-mode2
   undo-tree
   yaml-mode
   key-chord
   auctex
   sr-speedbar
   ace-jump-mode
   linum-relative
   projectile
   helm-projectile
   ))


;; SQL mode config
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

;; tramp mode config
(setq tramp-default-method "ssh")
;; chris@test-search1.donorschoose.org:/var/log/solr/solr.out

(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.(rdf|xul)$" . xml-mode))
(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))

;; Set up org mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/org" "~/Dropbox/org"))
(setq org-src-fontify-natively t)

;; Completes an item and moves it under completed heading if it exists
(fset 'myorg-complete
      (lambda (&optional arg)
        (interactive "p")
        (evil-set-marker ?z)
        (org-todo)
        (org-demote-subtree)
        (org-cut-subtree)
        (beginning-of-buffer)
        (org-cycle)
        (next-line)
        (org-paste-subtree)
        (previous-line)
        (org-shifttab)
        (evil-goto-mark ?z)))

(fset 'myorg-insert-src-tag
      (lambda (&optional arg) "Keyboard macro."
        (interactive "p")
        (insert "#+BEGIN_SRC ")
        (org-return)
        (insert "#+END_SRC")
        (evil-previous-line)
        (evil-append-line 1)
        ))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "RET") 'org-return-indent)
            (define-key org-mode-map "\C-ct" 'myorg-complete)
            (define-key org-mode-map "\C-cs" 'myorg-insert-src-tag)))

;; vi hotkeys
(require 'evil-leader)
(setq evil-leader/leader "," evil-leader/in-all-states t)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "k" 'kill-this-buffer
  "40" 'kill-buffer-and-window
  "0" 'delete-window
  "b" 'helm-mini
  "w" 'save-buffer
  "e" 'helm-find-files
  "f" 'helm-find-files
  "o" 'other-window
  "ii" 'ispell
  "iw" 'flyspell-check-previous-highlighted-word
  "x" (lambda () (interactive) (save-buffer) (kill-buffer)))
(global-evil-leader-mode)

(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)

;; Evil mode config
(evil-mode 1)
;; (setq evil-want-fine-undo t)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map (kbd "C-[") 'keyboard-escape-quit)
;; (define-key minibuffer-local-ns-map (kbd "C-[") 'keyboard-escape-quit)
;; (define-key minibuffer-local-completion-map (kbd "C-[") 'keyboard-escape-quit)
;; (define-key minibuffer-local-must-match-map (kbd "C-[") 'keyboard-escape-quit)
;; (define-key minibuffer-local-isearch-map (kbd "C-[") 'keyboard-escape-quit)

(setq evil-esc-delay 0)

(defun evil-org-after (fun)
  (evil-append-line 1)
  (funcall fun))

(defun evil-org-before (fun)
  (evil-insert-line 1)
  (funcall fun))

(evil-declare-key 'normal org-mode-map
  "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
  "L" 'org-end-of-line ; smarter behaviour on headlines etc.
  (kbd "TAB") 'org-cycle
  "-" 'org-ctrl-c-minus ; change bullet style
  "<" 'org-metaleft ; out-dent
  ">" 'org-metaright ; indent
  "(" 'org-backward-sentence
  ")" 'org-forward-sentence
  "{" 'org-backward-paragraph
  "}" 'org-forward-paragraph
  "gt" 'myorg-complete
  "gT" '(lambda () (interactive) (evil-org-after (lambda() (org-insert-todo-heading nil))))
  "gO" '(lambda () (interactive) (evil-org-before 'org-insert-heading))
  "go" '(lambda () (interactive) (evil-org-after 'org-insert-heading))
  "gl" 'org-open-at-point
  "gc" 'org-table-iterate
  "gn" 'outline-next-visible-heading
  "gp" 'outline-previous-visible-heading
  "gs" 'myorg-insert-src-tag
  "g=" 'evil-numbers/inc-at-pt
  "g-" 'evil-numbers/dec-at-pt
  )

(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'paredit-mode-hook 'evil-paredit-mode)

;; More vi hotkeys
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

;; Custom macro to eval clojure fn in nrepl
(defun add-clojure-eval-fn ()
  (fset 'mycloj-eval-fun
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([71 63 41 13 97 3 5 escape] 0 "%d")) arg))))
(add-hook 'clojure-mode-hook 'add-clojure-eval-fn)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
;; (add-hook 'clojure-mode-hook (lambda () (auto-complete-mode 1)))

;; cider config
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)

(setq cider-lein-command "/usr/local/bin/lein")

;; (require 'ac-nrepl)
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))
;; (setq ac-auto-start nil)
;; (ac-set-trigger-key "TAB")

;; (add-hook 'nrepl-mode-hook (lambda () (auto-complete-mode 1)))

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Disable evil in certain buffers
(add-hook 'shell-mode-hook (lambda () (evil-local-mode 0)))
(add-hook 'artist-mode-hook (lambda () (evil-local-mode 0)))

;; ispell/aspell config
(setq ispell-silently-savep t)
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--ignore-case"))
(setq ispell-list-command "list")

(setq markdown-command "/usr/local/bin/markdown")

;; End sentences with 1 space
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")
(setq sentence-end-double-space nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; bigger font
(set-face-attribute 'default nil :height 165)

;; Reopen files on restart
(desktop-save-mode 1)

;; Helm config
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-q")  'helm-select-action) ; list actions using C-q

(global-set-key (kbd "M-x") 'helm-M-x) 

(defun my-helm-multi-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;; yasnippet
(require 'yasnippet)
(require 'angular-snippets)
(eval-after-load "sgml-mode"
  '(define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point))

;; Always indent after return
(define-key global-map (kbd "RET") 'newline-and-indent)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(load-theme 'solarized-dark t)

(require 'linum-relative)
(linum-on)

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(define-key global-map (kbd "C-c e") 'replace-last-sexp)

;; VCL mode
(add-to-list 'auto-mode-alist '("\\.vcl" . vcl-mode))
(setq vcl-indent-level 4)
