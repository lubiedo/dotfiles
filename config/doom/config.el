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

;; enable alpha background
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; insert lambda symbol
(global-set-key (kbd "M-a") (lambda ()
  (interactive)
  (insert "λ")
  (move-to-column (1+ (current-column)))))

;; display lambda as symbol
(global-prettify-symbols-mode 1)

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

;; dired {{
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-open
  :config
  (setq dired-open-extensions '(
                                ("svg" . "open")
                                ("jpg" . "open")
                                ("png" . "open"))));;
;; }}

;; rss feed {{{
(use-package elfeed
  :defer t
  :config
  (add-hook 'elfeed-show-mode-hook #'elfeed-update)
  (setq elfeed-search-filter "@2-week-ago")
  (setq elfeed-feeds '(("https://www.artofmanliness.com/rss" artofmanliness)
                      ("https://talkback.sh/home/feed/" talkback security)
                      ("https://hackaday.com/blog/feed/" hackaday linux))))
(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-goodies/entry-pane-size 0.50)

  (add-hook 'elfeed-show-mode-hook #'writeroom-mode)) ;; display entry in zen mode
;; }}}

;; macos {{{
(if IS-MAC (progn
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
 "o e" '(elfeed :wk "Open RSS feed"))

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
    (setq cmd (completing-read-default "up/down: \n" cmds))
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

(defun qr-gen (s e)
  "Generate a QR code from REGION using DuckDuckGo API."
  (interactive "r")
  (defvar qr-gen-buffer-name "qr-gen")
  (defvar qr-gen-url "https://api.duckduckgo.com/?q=qr+code+%s&format=json")

  (let ((data (shr-encode-url (thing-at-point 'region)))
         (url-request-method "GET"))
    (if (not (string-empty-p data))
        (url-retrieve (url-encode-url (format qr-gen-url (princ data)))
                      (lambda (status )
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

(defun tidy-brew ()
  "Tidy up homebrew"
  (interactive)

  (if IS-MAC
      (exec-all-list '(
                       "brew update && brew upgrade"
                       "brew cleanup -s"
                       "brew doctor"
                       "brew missing"))))

(defun curl-site ()
  "Curl site following redirects and silently create new HTML buffer."
  (interactive
   (let ((website (read-string "URL: " nil 'url))
          (name (concat "curl-output-" (number-to-string (random)))))
     (with-current-buffer
         (get-buffer-create name)
       (insert (shell-command-to-string (concat "curl -fsSL " website))))
     (switch-to-buffer name)
     (html-mode)
     (beginning-of-buffer))))
;; }}}
