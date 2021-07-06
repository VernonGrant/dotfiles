;;; .emacs --- My personal emacs config file.
;;; Commentary:

;; This file contains some binding reminders and the minimal settings
;; required for me to get my work done.

;; General:
;;
;; C-h m           | find help regarding a mode.
;; C-h f           | find help regarding a function.
;; C-k             | delete rest of line, or line if empty.
;; C-/             | undo.
;; C-g C-/         | redo.
;; C-y             | yank from kill ring.
;; M-y             | yank down the kill ring.
;; M-%             | query replace.
;; M-/             | word completion.
;; C-s             | incremental search.
;; C-M-s           | regex search.
;; C-s C-s         | repeat incremental search.
;; C-r             | incremental search backward.
;; C-r C-r         | repeat incremental search backward.
;; C-s C-y         | start incremental search using yanked text.
;; C-s M-y         | start incremental search from kill ring string.
;; C-x s           | save all buffers.
;; C-x c-s         | save current buffer.
;; C-x c-w         | write the current buffer, can provide new file name.
;; C-o             | add blank line below.
;; C-x C-o         | delete all blank lines below.
;; C-x c-c         | quite Emacs.
;; M--             | prefix to perform negative meta commands.
;; C--             | prefix to perform negative commands
;; M-m             | move cursor to the true biggining of the line.
;; C-x f           | set fill column number.
;; M-q             | wrap/reformat to fill column.
;; M-v             | page up.
;; C-v             | page down.
;; M-^             | join previous line
;; M-- M-^         | join following line
;; M-<             | go to start of file
;; M->             | go to end of file
;; C-x C-f         | find file.
;; C-x C-v         | find alternative file.
;; C-x k           | kill current or named buffer.
;; C-x O           | go back to previous window, split.  (Capital O not zero).
;; C-x i           | insert a file into the current file
;; C-x h           | highlight entire buffer
;; C-x h, C-M \    | reindent entire buffer
;; C-S-&           | run async command
;; C-x C-j         | open dired jump
;; C-x SPC         | rectangle edit mode, can use string-insert-rectangle.
;; C-x +           | resize all splits to equal sizes
;; C-x 5 2         | open in new frame
;; C-x r SPC       | point-to-register
;; C-x r j         | jump-to-register
;; g               | refresh page, worked on many UI pages.
;; M-s o           | list lines using regex.
;; M-s h r         | highlights regex expression.

;; Tags:
;;
;; M-.         | visit tag at point
;; M-,         | jump back.
;; C-x 4 .     | visit tag in new split
;; C-x 5 .     | visit tag in new frame

;; Isearch:
;;
;; Use the following while in isearch.
;;
;; M-s o        | show all lines matching the search term in occur.
;; M-s e        | to edit the search string in the minibuffer.
;; M-s h r      | highlights the last search string.

;; Occur:
;;
;; Allows you to operate on lines.
;;
;; e            | directly edit the entry in place.
;; C-c C-c      | return back to occur mode after edit.

;; Dired:
;;
;; C-o          | preview file but stay in dired buffer
;;              | find-name-dired, searches for file

;; Grep:
;;
;; C-c C-K      | kill process
;; C-c C-P      | enable wgrep

;; Compile:
;; ?            | describe-mode
;; g            | recompile
;; h            | describe-mode
;; q            | quit-window

;; Ido:
;;
;; M-f          | find file recursively.  Search into sub directory.
;; ?            | see a full list of all matching buffers.

;; Bookmarks:
;;
;; C-x r m  | create / set bookmark
;; C-x r b  | open bookmark
;; C-x r l  | list bookmarks:
;;            d | to mark the current item for remove
;;            x | to remove all D marked ones
;;            r | rename current item's title
;;            s | save the change

;; Ispell:
;;
;; M $                | Check and correct spelling of the word at point (ispell-word).  If the region is active, do it for all words in the region instead.
;; C-M i              | Complete the word before point based on the spelling dictionary (ispell-complete-word).
;; flyspell-mode      | Enable Fly-spell mode, which highlights all misspelled words.
;; flyspell-prog-mode | Enable Fly-spell mode for comments and strings only.
;; flyspell-buffer    | Check and correct spelling in the buffer.

;; Org:
;;
;; S-M-RET     | insert a new todo entry below the current one
;; C-c / t     | view todo items in a sparse tree
;; C-c C-t     | toggle todo item state
;; C-c C-s     | schedule todo item
;; C-c C-d     | add deadline to todo item
;; C-c [       | add file to agenda
;; C-c ]       | remove file to agenda
;; C-c .       | insert current date
;; C-c C-e     | open exporter

;; Emmet:
;;
;; C-j         | expand emmet

;; Rg:
;;
;; C-c s     | start a new rg (ripgrep) search
;; e         | directly edit search results
;; m         | bring up settngs menu

;; Dependencies:
;;
;; - ripgrep  | used by rg.el
;; - Ispell   | helps mark misspelled words
;; - basictex | For org not exportasion to PDF

;; Melpa:

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))



;; Emacs Configuration:

;; performance.
(setq gc-cons-threshold 100000000) ;; 100mb
(setq read-process-output-max (* 10240 10240)) ;; 10mb

;; disable bell.
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; increate the undo limit.
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; remove GUI menus and scrollbar.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; show matching parenthesis.
(show-paren-mode 1)

;; disable line wrapping.
(set-default 'truncate-lines -1)

;; delete reqions.
(delete-selection-mode 1)

;; show line numbers.
(global-display-line-numbers-mode)

;; show ruler
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; IMPORTANT, helps increase the nextline performace by 10x.
(setq auto-window-vscroll nil)

;; save backup files to specific folder.
(setq backup-directory-alist `(("." . "~/.saves")))

;; set the tramp mode. /ssh:Name:path.
(setq tramp-default-method "ssh")

;; trim whitespaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; toggle saving of minibuffer history (Savehist mode).
(savehist-mode 1)

;; automatically save bookmarks in custom file.
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs-bookmarks")

;; minibuffer settings.
(setq resize-mini-windows t)
(setq max-mini-window-height 0.5)

;; auto completion
(setq dabbrev-check-all-buffers t)

;; enable recentf, to open recent files.
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)

;; org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; grep, rgrep config
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories ".source-completions")
     (add-to-list 'grep-find-ignored-directories ".bundle")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; php mode settings.
(setq  php-mode-template-compatibility nil)



;; IDO Config:

(ido-mode t)
(ido-everywhere)

;; keep the minibuffer at x height.

;; set the min height.
(setq ido-enable-prefix nil
      ido-enable-flex-matching t                 ;; show any name that has the chars you typed
      ido-case-fold t                            ;; ignore upper or lower case searches
      ido-auto-merge-work-directories-length -1  ;; after how many characters before looking in other folders.
      ido-default-file-method 'selected-window   ;; use current pane for newly opened file
      ido-default-buffer-method 'selected-window ;; use current pane for newly switched buffer
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-max-window-height 3
      ido-create-new-buffer 'always
      )

;; (setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" "
;;   [No match]" " [Matched]" " [Not readable]" " [Too big]" "
;;   [Confirm]")))

;; stop ido from suggesting when naming new file.
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

;; make m-x use ido.
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))
   )
 )



;; Packages:

(eval-when-compile
  (require 'use-package))

;; core pacakges.
;; (use-package glsl-mode)
(use-package cmake-mode :ensure t)
(use-package dart-mode :ensure t)
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package php-mode :ensure t)
(use-package gitignore-mode :ensure t)
(use-package jinja2-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package pip-requirements :ensure t)
(use-package emmet-mode
  :ensure t
  :hook
  (html-mode . emmet-mode)
  (web-mode . emmet-mode))
(use-package web-mode
  :ensure t
  :init
  (setq web-mode-block-padding 0)
  (setq web-mode-markup-indent-offset 2)
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode +1))

;; this solved the conflict with php, web-mode.
(add-hook 'editorconfig-custom-hooks
	  (lambda (hash) (setq web-mode-block-padding 0)))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
	'("~/.emacs-snippets"))
  (yas-global-mode 1))

(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
  (global-company-mode))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults).
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)
;;   ;; Enable flashing mode-line on errors.
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))



;; Flycheck

(use-package flycheck
  ;; linters:
  ;; js - sudo npm -g i eslint
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-php-phpcs-executable "/home/vernon/.config/composer/vendor/bin/phpcs")
  (setq flycheck-phpcs-standard "WordPress")

  ;; add web-mode linters.
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'php-phpcs 'web-mode)
    (flycheck-add-mode 'php-phpcs 'php-mode)
    (flycheck-add-mode 'css-csslint 'css-mode)
    (flycheck-add-mode 'html-tidy 'web-mode)
    (flycheck-add-mode 'css-csslint 'web-mode))
  :bind ("<f5>" . flycheck-list-errors-toggle))



;; LSP Mode:
;; language servers:
;; - c          | sudo apt-get install clangd-9
;;  -c          | sudo apt install clangd
;; - php        | sudo npm i intelephense -g
;; - css        | sudo npm install -g vscode-css-languageserver-bin
;; - json       | lsp-install-server json-ls
;; - html       | lsp-install-server html-ls
;; - javascript | lsp-install-server jsts-ls
;; - markdown   | sudo npm i -g unified-language-server
;; - yaml       | sudo npm install -g yaml-language-server

(use-package lsp-mode
  :ensure t
  :defer t
  :config
  ;; performance.
  (setq lsp-log-io nil) ;; turn off logs.
  (setq lsp-file-watch-threshold 5000)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-diagnostic-package :none) ;; use just plain flycheck instead. TODO: how to also include lsp errors.
  ;; code
  (setq lsp-clients-clangd-args
	'("-j=2"
	  "--background-index"
	  "--clang-tidy"
	  "--completion-style=bundled"
	  "--pch-storage=memory"
	  "--header-insertion=never"
	  "--header-insertion-decorators=0"))
  :hook ((php-mode . lsp)
         (js-mode . lsp)
         (css-mode . lsp)
         (c-mode . lsp)
	 (html-mode . lsp)
	 (web-mode . lsp)
         (json-mode . lsp)
         (yaml-mode . lsp)
         (javascript-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :init
  ;; (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 1)
  :bind
  (("C-c l l" . 'lsp-ui-doc-glance))
  (("C-c l d" . 'lsp-ui-peek-find-definitions))
  (("C-c l D" . 'lsp-ui-peek-find-references))
  :commands lsp-ui-mode)



;; Helper Functions:

(defun open-emacs-init-file()
  "Will locate and open the Emacs init file (.emacs)."
  (interactive)
  (find-file user-init-file)
  )
(global-set-key (kbd "C-c e") 'open-emacs-init-file)

(defun automatically-eval-init-file()
  "Automatically load the EMACS init file after change."
  (when (equal user-init-file buffer-file-name)
    (load user-init-file))
  )
(add-hook 'after-save-hook 'automatically-eval-init-file)

(defun vg-duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun flycheck-list-errors-toggle ()
  "Toggle the error list for the current buffer."
  (interactive)
  (let ((flycheck-errors-window (get-buffer-window flycheck-error-list-buffer)))
    (if (not (window-live-p flycheck-errors-window))
        (call-interactively 'flycheck-list-errors)
      (delete-window flycheck-errors-window))))

(defun xah-cursor-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n))
  (back-to-indentation))

(defun xah-cursor-backward-block(&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        ($i 1))
    (while (<= $i n)
      (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq $i n)))
      (setq $i (1+ $i))))
  (back-to-indentation))

(defun xah-bookmark-open-file-fast ()
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.  This
command is similar to `bookmark-jump', but uses the `ido-mode'
interface, and ignore cursor position in bookmark.  URL
`http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($this-bookmark
         (ido-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $this-bookmark))
    (bookmark-jump $this-bookmark)
    ))



;; Keymaps:

;; dired jump, require is needed to load it from the start.
(require 'dired-x)
(global-set-key (kbd "C-x j") #'dired-jump)

;; remove most retarded key binding of all time.
(global-unset-key (kbd "C-z"))

;; window sizing.
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; quick open recent files.
(global-set-key (kbd "C-c r") 'recentf-open-files)

;; search inside files.
(global-set-key (kbd "C-c s") 'rgrep)

;; increase and decrease text scale.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)

;; editing.
(global-set-key (kbd "M-s M-s") 'sort-lines)

;; file navigation.
(global-set-key (kbd "C-x ,") 'previous-buffer)
(global-set-key (kbd "C-x .") 'next-buffer)

;; custom function bindings.
(global-set-key (kbd "C-x r b") 'xah-bookmark-open-file-fast)
(global-set-key (kbd "C-c d") 'vg-duplicate-line-or-region)

;; vertical movement.
(global-set-key (kbd "M-p") 'xah-cursor-backward-block)
(global-set-key (kbd "M-n") 'xah-cursor-forward-block)


;; Editor Generated Options:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(package-selected-packages
   (quote
    (cmake-mode yasnippet yaml-mode web-mode use-package pip-requirements php-mode magit lsp-ui json-mode jinja2-mode gitignore-mode emmet-mode editorconfig doom-themes dart-mode company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
