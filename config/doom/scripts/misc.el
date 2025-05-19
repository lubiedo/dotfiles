;;; misc.el -*- lexical-binding: t; -*-

(defun rc/misc-ip-vt ()
  (interactive)
    (let ((pos (point))
          (beg (prog1
                   (skip-chars-backward "^ " (line-beginning-position))
                 (point)))
          (end (prog1
                 (skip-chars-forward "^ " (line-end-position))
                 (point))))
      (message (concat (number-to-string beg) " " (number-to-string pos) " " (number-to-string end)))
      (copy-region-as-kill (+ beg pos) (+ beg pos end))
      (with-current-buffer (get-buffer-create "*ip-vt*")
        (insert (shell-command-to-string (concat "vt search " (car kill-ring)))))))

(provide 'rc/misc)
