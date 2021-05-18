;; General -------------------------------------------
;;
;; C-h m           | find help regarding a mode.
;; C-h f           | find help regarding a function.
;; C-k             | delete rest of line, or line if empty.
;; C-/             | undo
;; C-/             | redo
;; C-y             | yank from kill ring.
;; M-y             | yank down the kill ring.
;; M-%             | query replace.
;; C-s             | incremental search.
;; C-s C-s         | repeat incremental search.
;; C-r             | incremental search backward.
;; C-r C-r         | repeat incremental search backward.
;; C-s C-y         | start incremental search using yanked text.
;; C-s M-y         | start incremental search from kill ring string.
;; C-x s           | save all buffers.
;; C-x c-s         | save current buffer.
;; C-x c-w         | write the current buffer, possible to pick the name of .
;; C-o             | add blank line below
;; C-x C-o         | delete all blank lines below
;; C-x c-c         | quite emacs.
;; M--             | prefix to perform negative commands for example M-- M-u
;; C--             | prefix to perform negative commands
;; M-m             | to true biggining of line
;; M-v             | page up.
;; C-v             | page down.
;; M-^             | join previous line
;; M-- M-^         | join following line
;; M-<             | go to start of file
;; M->             | go to end of file
;; C-x C-f         | find file.
;; C-x C-v         | find alternative file.
;; C-x O           | go back to previous window, split. (Capital O not zero).
;; C-x i           | insert a file into the current file
;; C-x h           | highlight entire buffer
;; C-x h, C-M \    | reindent entire buffer
;; C-S-&           | run async command
;; C-x C-j         | open dired jump
;; C-x +           | resize all splits to equal sizes
;; C-x 5 2         | open in new frame
;; C-x r SPC       | point-to-register
;; C-x r j         | jump-to-register

;; dired  ------------------------------------------------
;;
;; C-o         | preview file but stay in dired buffer
;;             | find-name-dired, searches for file

;; tags -------------------------------------------------
;;
;; M-.         | visit tag at point
;; M-,         | jump back
;; C-x 4 .     | visit tag in new split
;; C-x 5 .     | visit tag in new frame

;; isearch ---------------------------------------------
;;
;; M-s o       | occur that shows the last search string.
;; M-s e       | to edit the search string in the minibuffer.
;; M-s h r     | highlights the last search string.

;; occur  ----------------------------------------------
;;
;; e           | directly edit the entry in place.
;; C-c C-c     | return back to occur mode after edit.

;; org  -----------------------------------------------
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

;; grep  ---------------------------------------------
;;
;; C-c C-K     | kill process
;; C-c C-P     | enable wgrep

;; compile  ------------------------------------------
;; ?            | describe-mode
;; g            | recompile
;; h            | describe-mode
;; q            | quit-window

;; bookmarks  ----------------------------------------
;;
;; C-x r m  | create / set bookmark
;; C-x r b  | open bookmark
;; C-x r l  | list bookmarks:
;;            d | to mark the current item for remove
;;            x | to remove all D marked ones
;;            r | rename current item's title
;;            s | save the change

;; ispell  --------------------------------------------
;;
;; M $                | Check and correct spelling of the word at point (ispell-word). If the region is active, do it for all words in the region instead.
;; C-M i              | Complete the word before point based on the spelling dictionary (ispell-complete-word).
;; flyspell-mode      | Enable Fly-spell mode, which highlights all misspelled words.
;; flyspell-prog-mode | Enable Fly-spell mode for comments and strings only.
;; flyspell-buffer    | Check and correct spelling in the buffer.

;; emmet ----------------------------------------------
;;
;; C-j         | expand emmet

;; rg  ------------------------------------------------
;;
;; C-c s     | start a new rg (ripgrep) search
;; e         | directly edit search results
;; m         | bring up settngs menu

;; dependencies  --------------------------------------
;;
;; - ripgrep  | used by rg.el
;; - ispell   | helps mark misspelled words
;; - basictex | For org not exportasion to PDF

;; todo  ---------------------------------------------
;;
;; Implement some sort of linting in emacs.
;; Work on more productive Yasnippets.
;; Sometimes Yas does not load snippets... why?
;;
;; Learn more about the kill ring.
;;
;; TODO: Check if ido can look into sub folders.
;; TODO: Check if flycheck can work with web-mode for php.

;; -------------------------------------
;; Melpa
;; -------------------------------------

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; comment/uncomment these two lines to enable/disable melpa and melpa stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; for important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

;; packages.
(setq package-list '(
                     ;; core.
                     exec-path-from-shell
                     use-package

		     ;; theme.
		     zenburn-theme

                     ;; modes.
                     dart-mode
                     json-mode
                     php-mode
                     gitignore-mode
                     jinja2-mode
                     js2-mode
                     markdown-mode
                     pip-requirements
                     yaml-mode
                     ))

;; fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; -------------------------------------
;; General Configuration
;; -------------------------------------

;; disable bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; increate the undo limit
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; remove GUI menus and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; show matching parenthesis
(show-paren-mode 1)

;; disable line wrapping
(set-default 'truncate-lines -1)

;; delete reqions
(delete-selection-mode 1)

;; show line numbers
(global-display-line-numbers-mode)

;; IMPORTANT, helps increase the nextline performace by 10x
(setq auto-window-vscroll nil)

;; save backup files to specific folder
(setq backup-directory-alist `(("." . "~/.saves")))

;; set the tramp mode. /ssh:Name:path
(setq tramp-default-method "ssh")

;; trim whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; toggle saving of minibuffer history (Savehist mode).
(savehist-mode 1)

;; automatically save bookmarks in custom file
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs-bookmarks")

;; minibuffer settings
(setq resize-mini-windows t)
(setq max-mini-window-height 0.5)

;; dynamic abbreviation (buffer completions)
;; auto completion
(setq dabbrev-check-all-buffers t)

;; enable recentf, to open recent files.
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)

;; grep, rgrep config
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories ".bundle")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; -------------------------------------
;; Packages Configuration
;; -------------------------------------

;; this is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq flycheck-php-phpcs-executable "/home/vernon/.config/composer/vendor/bin/phpcs")
  (setq flycheck-phpcs-standard "WordPress")

  ;; determine how the flycheck error list should be show.
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.20)))

  ;; add web-mode linters.
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'php-phpcs 'web-mode)
    (flycheck-add-mode 'html-tidy 'web-mode)
    (flycheck-add-mode 'css-csslint 'web-mode))

  :bind ("<f5>" . flycheck-list-errors-toggle))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-block-padding 0)
  (setq web-mode-markup-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode +1))

;; this solved the conflict with php, web-mode.
(add-hook 'editorconfig-custom-hooks
	  (lambda (hash) (setq web-mode-block-padding 0)))

(use-package emmet-mode
  :ensure t
  :hook
  (html-mode . emmet-mode)
  (web-mode . emmet-mode))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'column)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; -------------------------------------
;; IDO CONFIG
;; -------------------------------------

(ido-mode t)
(ido-everywhere)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t                ;; show any name that has the chars you typed
      ido-case-fold t                           ;; ignore upper or lower case searches
      ido-auto-merge-work-directories-length -1 ;; after how many characters before looking in other folders.
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-default-file-method 'selected-window   ;; use current pane for newly opened file
      ido-default-buffer-method 'selected-window ;; use current pane for newly switched buffer
      ;; ido-create-new-buffer 'always
      )

;; make ido display vertically.
(progn
  (make-local-variable 'ido-decorations)
  (setf (nth 2 ido-decorations) "\n"))

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

;; -------------------------------------
;; Custom functions
;; -------------------------------------

;; TODO: When duplicating a region add a blank space at the end.
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

(defun vg-ctags-create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name)))
  )

(defun vg-ctags-add-tags (dir-name)
  "Add tags to file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f TAGS -e -a -R %s" (directory-file-name dir-name)))
  )

(defun vg-make-target-command (target)
  "Compile make target from within any file, will automaticly look for the root makefile."
  (interactive "sTarget Name: ")
  (let* ((makefileDir (locate-dominating-file buffer-file-name "makefile"))
        (makefileCommand (format "make %s --directory=%s" target makefileDir)))
    (compile makefileCommand))
  )

(defun flycheck-list-errors-toggle ()
  "Toggle the error list for the current buffer."
  (interactive)
  (let ((flycheck-errors-window (get-buffer-window flycheck-error-list-buffer)))
    (if (not (window-live-p flycheck-errors-window))
        (call-interactively 'flycheck-list-errors)
      (delete-window flycheck-errors-window))))

;; -------------------------------------
;; Xah Functions
;; -------------------------------------

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
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.
This command is similar to `bookmark-jump', but uses the `ido-mode' interface, and ignore cursor position in bookmark.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($this-bookmark
         (ido-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $this-bookmark))
    (bookmark-jump $this-bookmark)
    ))

;; -------------------------------------
;; Convince the editing of my emacs config
;; -------------------------------------

(defun open-emacs-init-file()
  "Will locate and open the emacs init file. (.emacs)"
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

;; -------------------------------------
;; Keymaps
;; -------------------------------------
;; Keymaps: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

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

;; editing.
(global-set-key (kbd "M-s M-s") 'sort-lines)

;; file navigation.
(global-set-key (kbd "C-x ,") 'previous-buffer)
(global-set-key (kbd "C-x .") 'next-buffer)

;; custom function bindings.
(global-set-key (kbd "C-x r b") 'xah-bookmark-open-file-fast)
(global-set-key (kbd "C-c d") 'vg-duplicate-line-or-region)
(global-set-key (kbd "C-c m") 'vg-make-target-command)

;; vertical movement.
(global-set-key (kbd "M-p") 'xah-cursor-backward-block)
(global-set-key (kbd "M-n") 'xah-cursor-forward-block)

;; dired jump, require is needed to load it from the start.
(require 'dired-x)
(global-set-key (kbd "C-x j") #'dired-jump)

;; disable arrow keys completely.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;; -------------------------------------
;; EDITOR OPTIONS
;; -------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme yaml-mode web-mode use-package pip-requirements php-mode monokai-theme markdown-mode magit json-mode js2-mode jinja2-mode highlight-indent-guides gitignore-mode flycheck exec-path-from-shell emmet-mode editorconfig dart-mode company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
