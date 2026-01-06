;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Greybeard 11px" :size 14)
     doom-variable-pitch-font (font-spec :family "Fira Code" :size 13))
(setq doom-theme 'doom-gruvbox-custom)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")

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

;; minimap {{
(use-package minimap
  :config
  (setq minimap-window-location 'right))
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle minimap-mode" "m" #'minimap-mode))
;; }}

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
                                ("pdf" . "open")
                                ("png" . "open"))))
;;}}}

;; org {{{
(after! org
  (use-package org-bullets
    :hook
    (org-mode . org-bullets-mode))

  (use-package org-fancy-priorities
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("⚑" "⚑" "⚑" "☕︎")
          org-ellipsis " ⨁"))

  (setq org-directory "~/Documents/zettelkasten"
        org-agenda-files `("~/Documents/zettelkasten"))

  ;; stole some stuff from [[file+emacs:~/Documents/zettelkasten/links.org::12]]
  (setq org-todo-keywords
        `((sequence "TODO(t)" "NEXT(n)" "ONIT(o)" "|" "DONE(d@)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
          (sequence "IDEA(i@/!)" "IMPLEMENTED(I@/!)" "|" "DISCARDED(D@/!)")))

  (setq org-todo-keyword-faces
        `(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("ONIT" :foreground "purple" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ;; intermediary states
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)
          ;; ideas
          ("IDEA" :foreground "LightSkyBlue" )
          ("IMPLEMENTED" :foreground "LightSkyBlue" :weight bold)
          ("DISCARDED" :foreground "LightSkyBlue" :strike-through t)))

  (setq org-default-notes-file (concat org-directory "/agenda.org")
        org-custom-links-file (concat org-directory "/links.org"))

  (setq org-capture-templates
        `(("t" "todo" entry (file org-default-notes-file)
           "* TODO %? %^g\n%U\n")
          ("n" "note" entry (file org-default-notes-file)
           "* %? :NOTE:\n%U\n")
          ("i" "idea" entry (file org-default-notes-file)
           "* %? :IDEA:\n%U\n")
          ("l" "link" entry (file org-custom-links-file)
           "* %c %U %?\n%^{DESCRIPTION}p")))

  ;; custom tags
  (setq org-tag-alist `((:startgroup)
                        ("@errand" . ?h)
                        ("@home" . ?h)
                        ("@cyberia" . ?l)
                        (:endgroup)
                        ("ENHANCEMENT" . ?S)
                        ("STUDY" . ?S)
                        ("FINANCE" . ?B)
                        ("LANG" . ?L)
                        ("PERSONAL" . ?P)
                        ("WORK" . ?O)
                        ("HEALTH" . ?H)
                        ("FAM" . ?F)))

  ;; custom views
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-custom-commands
        '(("v" "Custom agenda view"
           ((agenda "" nil)
            (tags-todo "-TODO/!ONIT"
                  ((org-agenda-overriding-header "In Progress Tasks")
                   (org-tags-match-list-sublevels nil)))
            (tags "PRIORITY=\"A\""
                  ((org-agenda-overriding-header "All High Priority Tasks")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-TODO/!NEXT"
                  ((org-agenda-overriding-header "Next Tasks")
                   (org-tags-match-list-sublevels nil)))
            (tags "WORK"
                  ((org-agenda-overriding-header "Work Tasks")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-sorting-strategy
                    '(todo-state-down category-keep))))))
          ("l" "Links view"
           ((tags "LINK")))))

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
                       ("https://www.reddit.com/r/netsec/.rss" netsec)
                       ("https://news.ycombinator.com/rss" hn)
                       ("https://geohot.github.io/blog/feed.xml" geohot)
                       ("https://hackcur.io/feed/" hacker_curio)
                       ("http://krebsonsecurity.com/feed/" krebbo-stabbo)
                       ("https://malpedia.caad.fkie.fraunhofer.de/feeds/rss/latest" malpedia)
                       ("https://journal.miso.town/atom?url=https://wiki.xxiivv.com/site/now.html" xxiivv)
                       ("https://www.dazeddigital.com/rss" dazed)
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
 "z" '(int/zettelkasten :wk "Zettelkasten"))

(define-leader-key!
 "o B" '(browse-url-at-point :wk "Browse URL at point")
 "o e" '(rc/pre-elfeed :wk "Open RSS feed (and update)"))

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
(defun int/zettelkasten ()
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

(defun util/file-sum (path)
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
                      (util/file-sum-print file)))
                )))
        ((file-regular-p path)
           (util/file-sum-print path)))))

(defun util/file-sum-print (file)
  "Given a FILE, print checksum in a new buffer (see `file-sum')."
  (get-buffer-create "file-sum-output")
  (with-current-buffer "file-sum-output"
    (insert (concat
             "File: " file "\n"
             "Size: " (shell-command-to-string (concat "stat -f '%z' \'" file "\'"))
             "Type: " (shell-command-to-string (concat "file -b \"" file "\""))
             "MD5: " (shell-command-to-string (concat "md5 -q \'" file "\'"))
             "SHA1: " (shell-command-to-string (concat "shasum -a 1 \'" file "\' | cut -d' ' -f1"))
             "SHA256: " (shell-command-to-string (concat "shasum -a 256 \'" file "\' | cut -d' ' -f1"))
             ))
    (switch-to-buffer (buffer-name))))

(defun util/qr-gen (s e)
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
    (defun sys/restart-coreaudio ()
      (interactive)
      (async-shell-command "sudo kill $(pgrep 'coreaudiod')"))

    (defun sys/tidy-brew ()
      "Tidy up homebrew"
      (interactive)

      (rc/exec-all-list '(
                       "brew update && brew upgrade"
                       "brew cleanup -s"
                       "brew doctor"
                       "brew missing"))))

(defun util/url-defang (url)
  "'Defangs' an URL and copies it into the GUI clipboard."
  (interactive "sURL: ")
  (with-temp-buffer
    (insert (->> url    ;; sequence of replacements
                 (replace-regexp-in-string "//\\(.+?\\)\.\\([a-zA-Z0-9_\-]+\\)/" "//\\1[.]\\2/")
                 (replace-regexp-in-string "^\\([^:]+\\)://" "\\1[:]//")))
    (message (buffer-string))
    (mark-whole-buffer)
    (clipboard-kill-region 0 0 (region-active-p))))

(defun util/curl-site ()
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

(defun rc/exec-all-list (cmds)
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
           (rc/exec-all-list (cdr cmds)))))))

(defun rc/pre-elfeed ()
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
