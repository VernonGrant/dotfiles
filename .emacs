;; General -------------------------------------------
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
;; C-x c-c         | quite emacs.
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
;; g               | refresh page, worked on many UI pages.
;; M-s o           | list lines using regex.
;; M-s h r         | highlights regex expression.

;; Tags -------------------------------------------------
;;
;; M-.         | visit tag at point
;; M-,         | jump back.
;; C-x 4 .     | visit tag in new split
;; C-x 5 .     | visit tag in new frame

;; Isearch ---------------------------------------------
;;
;; Use the following while in isearch.
;;
;; M-s o       | show all lines matching the search term in occur.
;; M-s e       | to edit the search string in the minibuffer.
;; M-s h r     | highlights the last search string.

;; Occur  ----------------------------------------------
;;
;; Allows you to operate on lines.
;;
;; e           | directly edit the entry in place.
;; C-c C-c     | return back to occur mode after edit.

;; Dired  ------------------------------------------------
;;
;; C-o         | preview file but stay in dired buffer
;;             | find-name-dired, searches for file

;; Grep  ---------------------------------------------
;;
;; C-c C-K     | kill process
;; C-c C-P     | enable wgrep

;; Compile  ------------------------------------------
;; ?            | describe-mode
;; g            | recompile
;; h            | describe-mode
;; q            | quit-window

;; IDO  ------------------------------------------
;;
;; M-f            | wide find file. Search into sub directory.
;; ?              | see a full list of all matching buffers.

;; Bookmarks  ----------------------------------------
;;
;; C-x r m  | create / set bookmark
;; C-x r b  | open bookmark
;; C-x r l  | list bookmarks:
;;            d | to mark the current item for remove
;;            x | to remove all D marked ones
;;            r | rename current item's title
;;            s | save the change

;; Ispell  --------------------------------------------
;;
;; M $                | Check and correct spelling of the word at point (ispell-word). If the region is active, do it for all words in the region instead.
;; C-M i              | Complete the word before point based on the spelling dictionary (ispell-complete-word).
;; flyspell-mode      | Enable Fly-spell mode, which highlights all misspelled words.
;; flyspell-prog-mode | Enable Fly-spell mode for comments and strings only.
;; flyspell-buffer    | Check and correct spelling in the buffer.

;; Org  -----------------------------------------------
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

;; Emmet ----------------------------------------------
;;
;; C-j         | expand emmet

;; Rg  ------------------------------------------------
;;
;; C-c s     | start a new rg (ripgrep) search
;; e         | directly edit search results
;; m         | bring up settngs menu

;; Dependencies  --------------------------------------
;;
;; - ripgrep  | used by rg.el
;; - ispell   | helps mark misspelled words
;; - basictex | For org not exportasion to PDF

;; Todo  ---------------------------------------------
;;
;; Implement some sort of linting in emacs.
;; Work on more productive Yasnippets.
;; Sometimes Yas does not load snippets... why?
;; Learn more about the kill ring.
;;
;; TODO: Check if ido can look into sub folders.
;; TODO: Check if flycheck can work with web-mode for php.
;; TODO: check how to redo things.
;; TODO: when doing a grep dont use multiple windows for previewing the results.

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
		     monokai-theme

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

;; org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

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

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
	'("~/.emacs-snippets"))
  (yas-global-mode 1))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode +1))

(use-package zeal-at-point
  :ensure t
  :bind ("<f9>" . zeal-at-point))

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
  (global-company-mode))

;; tutorial related.
(defun vg-presentation-setup ()
  (text-scale-increase 5))

(defun vg-presentation-end ()
  (text-scale-increase 0))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . vg-presentation-setup)
	 (org-tree-slide-stop . vg-presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil)
  :bind
  ("<f9>" . org-tree-slide-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode))

;; -------------------------------------
;; IDO CONFIG
;; -------------------------------------

(ido-mode t)
(ido-everywhere)

;; set the min height.
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

(setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" "
  [No match]" " [Matched]" " [Not readable]" " [Too big]" "
  [Confirm]")))
;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; make ido display vertically.
;; (progn
;;   (make-local-variable 'ido-decorations)
;;   (setf (nth 2 ido-decorations) "\n"))

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

;; increase and decrease text scale.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)

;; ctags
(global-set-key (kbd "<f7>") 'vg-ctags-create-tags)
(global-set-key (kbd "<f8>") 'vg-ctags-add-tags)

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
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (zeal-at-point yasnippets yas-snippets org-tree-slide zenburn-theme yaml-mode web-mode use-package pip-requirements php-mode monokai-theme markdown-mode magit json-mode js2-mode jinja2-mode highlight-indent-guides gitignore-mode flycheck exec-path-from-shell emmet-mode editorconfig dart-mode company)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
