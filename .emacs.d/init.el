;; init.el

(let ((custom-dir (expand-file-name "customisations/" user-emacs-directory)))
  (dolist (file (directory-files custom-dir t "\\.el$"))
    (message "Loading %s" file)
    (load file)))

;; Automatically tangle org files when saved in customisations folder
(defun tangle-config-org ()
  "Tangle org files automatically if they are in customisations/"
  (when (string-prefix-p (expand-file-name "customisations/" user-emacs-directory)
                         (buffer-file-name))
    (org-babel-tangle)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'tangle-config-org nil 'make-it-local)))

