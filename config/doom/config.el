;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 12 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Fira Code" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox-custom)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; add extra scripts and requires
(add-load-path! "~/.config/doom/scripts/")
(require 'speed-type) ;; practice speed typing!
(require 'thingatpt)
(require 'yara-mode)

;; take care of emacs looks
;; (scroll-bar-mode 0)
;; (menu-bar-mode 0)
;; (show-paren-mode 1)
;; (column-number-mode 1)
;; (tool-bar-mode 0)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; show clock
(display-time-mode 1)

;; insert lambda symbol
(global-set-key (kbd "M-a") (lambda ()
  (interactive)
  (insert "λ")
  (move-to-column (1+ (current-column)))))

;; display lambda as symbol
(global-prettify-symbols-mode 1)

;; ;; jupyter {{
;; (require 'jupyter)
;; (after! jupyter
;;   (defun jupyter-locate-python () ;; hacky thing as I use 3.11, not latest
;;     local/python-interpreter ))
;; ;; }}

;; vterm {{{
(use-package vterm
  :defer t
  :custom
  (vterm-shell "zsh")
  (setq vterm-timer-delay 0))
;; }}}

;; tabs {{{
(if (not (daemonp))
    tab-bar-mode)
;; }}}

;; dired {{{
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-open
  :config
  (setq dired-open-extensions '(
                                ("svg" . "open")
                                ("jpg" . "open")
                                ("png" . "open"))))
;;}}}

;; org {{{
(after! org
  (use-package org-fancy-priorities
    :ensure t
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("⚑" "⚑" "⚑" "☕︎")))

  (setq org-directory "~/Documents/zettelkasten"
        org-agenda-files `("~/Documents/zettelkasten"))

  ;; higly based on http://doc.norang.ca/org-mode.html#tasksandstates
  (setq org-todo-keywords
        `((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-todo-keyword-faces
        `(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))

  (setq org-default-notes-file (concat org-directory "/agenda.org"))
  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        `(("t" "todo" entry (file org-default-notes-file)
           "* TODO %? %^g\n%U\n")
          ("n" "note" entry (file org-default-notes-file)
           "* %? :NOTE:\n%U\n")))

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)

  ;; custom tags
  (setq org-tag-alist `((:startgroup)
                        ("@errand" . ?h)
                        ("@home" . ?h)
                        ("@cyberia" . ?l)
                        ("@misc" . ?m)
                        (:endgroup)
                        ("STUDY" . ?S)
                        ("FINANCE" . ?B)
                        ("LANG" . ?L)
                        ("PERSONAL" . ?P)
                        ("WORK" . ?O)
                        ("HEALTH" . ?H)
                        ("FAM" . ?F)))

  (advice-remove #'org-babel-do-load-languages #'ignore)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      ;; (jupyter . t)
      ))
  )
;;}}}

;; rss feed {{{
(use-package elfeed
  :defer t
  :config
  (setq elfeed-search-filter "@1-week-ago")
  (setq elfeed-feeds '(("https://www.artofmanliness.com/rss" artofmanliness)
                       ("https://news.ycombinator.com/rss" hn)
                       ("http://krebsonsecurity.com/feed/" krebbo-stabbo)
                       ("https://journal.miso.town/atom?url=https://wiki.xxiivv.com/site/now.html" xxiivv)
                       ("https://talkback.sh/home/feed/" talkback security)
                       ("https://hackaday.com/blog/feed/" hackaday))))

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-goodies/entry-pane-size 0.65)
  (add-hook 'elfeed-show-mode-hook #'writeroom-mode)) ;; display entry in zen mode
;; }}}

;; macos {{{
(if (featurep :system 'macos) (progn
                                ;; define the python3 version
                                (defvar local/python-interpreter "python3.11")
                                (setq python-shell-interpreter local/python-interpreter)

                                ;; file deletion
                                (setq delete-by-moving-to-trash t)
                                (setq trash-directory "~/.Trash")))
;; }}}

;; keybindings {{{
(define-leader-key!
 "e"   '(:ignore t)
 "e b" '(eval-buffer :wk "Evaluate buffer")
 "e d" '(eval-defun :wk "Evaluate defun")
 "e e" '(eval-expression :wk "Evaluate expression")
 "e l" '(eval-last-sexp :wk "Evaluate expression before point")
 "e r" '(eval-region :wk "Evaluate region"))

(define-leader-key!
 "z" '(zettelkasten :wk "Zettelkasten"))

(define-leader-key!
 "o e" '(pre-elfeed :wk "Open RSS feed (and update)"))

(define-leader-key!
 "g d" '(magit-diff-dwim :wk "Magit diff (Dwim)"))

(map! :leader
      (:when (modulep! :ui workspaces)
        (:prefix-map ("TAB" . "workspace")
         :desc "Next workspace"            "<right>"    #'+workspace/switch-right
         :desc "Previous workspace"        "<left>"     #'+workspace/switch-left)))

(global-set-key (kbd "C-x <up>") 'ibuffer)
(global-set-key (kbd "C-x t <right>") (cmd! (tab-bar-switch-to-next-tab) :wk "Move to right tab"))
(global-set-key (kbd "C-x t <left>") (cmd! (tab-bar-switch-to-prev-tab) :wk "Move to left tab"))
;; }}}
;;

;; functions {{{
(defun exec-all-list (cmds)
  "Just execute a LIST of commands, one-by-one"
  (setq proc-name "exec-all-list-proc")
  (setq buffer-name "*Messages*")

  (if (not (null cmds))
      (progn
        (set-process-sentinel
         (start-process-shell-command proc-name buffer-name
                                      (format "/bin/zsh -c '%s'" (car cmds)))
         (lambda (p e)
           (message (format "%s => %s" (car cmds) (s-trim e)))
           (exec-all-list (cdr cmds)))))))

(defun zettelkasten ()
  "Copy zettelkasten to and from Google Drive backup.

This is an interactive function so it will require to choose: up or down."
  (interactive)
  (let ((cmds '("up" "down")))
    (setq cmd (completing-read-default "Direction: " cmds))
        (if (and (not (string-equal cmd "up")) (not (string-equal cmd "down")))
        (message "Unknown command")
        (progn
        ;; assume always is uploading
        (setq zettelkasten-to "gdrive:zettelkasten")
        (setq zettelkasten-from "~/Documents/zettelkasten")
        (if (string-equal cmd "down")
                (progn
                (setq zettelkasten-to "~/Documents/zettelkasten")
                (setq zettelkasten-from "gdrive:zettelkasten")
                )))

      ;; rsync command
      (setq zettelkasten-rsync-cmd
            (format "rclone copy --update --verbose \
--transfers 30 --checkers 8 --contimeout 60s \
--timeout 300s --retries 3 --low-level-retries 10 --stats 1s %s %s"
                    zettelkasten-from zettelkasten-to))
      (async-shell-command zettelkasten-rsync-cmd "*Messages*")
      )))

(defun file-sum (path)
  "Print file checksums and information to buffer.

If PATH is a directory then recursively check all files with a depth of 1."
  (interactive "G")
  (if (file-exists-p path)
      (cond
       ((file-directory-p path)
            (let ((files (directory-files path 'full directory-files-no-dot-files-regexp)))
              (while files
                (let ((file (pop files)))
                  (if (file-regular-p file)
                      (file-sum-print file)))
                )))
        ((file-regular-p path)
           (file-sum-print path)))))

(defun file-sum-print (file)
  "Given a FILE, print checksum in a new buffer (see `file-sum')."
  (get-buffer-create "file-sum-output")
  (with-current-buffer "file-sum-output"
    (insert (concat
             "File: " file "\n"
             "Size: " (shell-command-to-string (concat "stat -f '%z' " file))
             "Type: " (shell-command-to-string (concat "file -b " file))
             "MD5: " (shell-command-to-string (concat "md5 -q " file))
             "SHA1: " (shell-command-to-string (concat "shasum -a 1 " file "| cut -d' ' -f1"))
             "SHA256: " (shell-command-to-string (concat "shasum -a 256 " file "| cut -d' ' -f1"))
             ))
    (switch-to-buffer (buffer-name))))

(defun qr-gen (s e)
  "Generate a QR code from REGION using DuckDuckGo API."
  (interactive "r")
  (defvar qr-gen-buffer-name "qr-gen")
  (defvar qr-gen-url "https://api.duckduckgo.com/?q=qr+code+%s&format=json")

  (let ((data (shr-encode-url (thing-at-point 'region)))
         (url-request-method "GET"))
    (if (not (string-empty-p data))
        (url-retrieve (url-encode-url (format qr-gen-url (princ data)))
                      (lambda ()
                        (let ((answer (gethash "Answer" (json-parse-string
                                  (car (last (s-lines (buffer-string))))))))
                          (if (not (string-empty-p answer))
                              (progn
                                (setq answer (substring answer
                                                        (+ (s-index-of "base64," answer) 7)
                                                        (s-index-of "\" alt=\"A QR Code\"" answer)))
                                (shell-command-to-string (format "echo -n '%s' | base64 -D | open -f -a Preview" answer)))
                            (message "qr-gen: No answer from DDG.")))
                        ))
      nil)))

(if (featurep :system 'macos)
    (defun tidy-brew ()
      "Tidy up homebrew"
      (interactive)

      (exec-all-list '(
                       "brew update && brew upgrade"
                       "brew cleanup -s"
                       "brew doctor"
                       "brew missing"))))

(defun url-defang (url)
  "'Defangs' an URL and copies it into the GUI clipboard."
  (interactive "sURL: ")
  (with-temp-buffer
    (insert (->> url    ;; sequence of replacements
                 (replace-regexp-in-string "//\\(.+?\\)\.\\([a-zA-Z0-9_\-]+\\)/" "//\\1[.]\\2/")
                 (replace-regexp-in-string "^\\([^:]+\\)://" "\\1[:]//")))
    (message (buffer-string))
    (mark-whole-buffer)
    (clipboard-kill-region 0 0 (region-active-p))))

(defun curl-site ()
  "Curl site following redirects and silently create new HTML buffer."
  (interactive
   (let ((website (read-string "URL: " nil 'url))
          (name (concat "curl-output-" (number-to-string (random)))))
     (with-current-buffer
         (get-buffer-create name)
       (insert (shell-command-to-string (concat "curl -k -fsSL '" website "'"))))
     (switch-to-buffer name)
     (html-mode)
     (beginning-of-buffer))))

(defun pre-elfeed ()
  "Calls `elfeed' and updates its feeds database."
  (interactive)
  (elfeed)
  (elfeed-update))

;; credit: https://github.com/rexim/dotfiles
(defconst rc/frame-transparency 97)

(defun rc/toggle-transparency ()
  (interactive)
  (let ((frame-alpha (frame-parameter nil 'alpha)))
    (if (or (not frame-alpha)
            (= (cadr frame-alpha) 100))
        (rc/set-transparency rc/frame-transparency)
      (rc/set-transparency 100))))

(defun rc/set-transparency (n)
  (set-frame-parameter nil 'alpha (list n n)))

(rc/set-transparency rc/frame-transparency)
;; }}}
