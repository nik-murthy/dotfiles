;; init.el

(let ((custom-dir (expand-file-name "customisations/" user-emacs-directory)))
  (dolist (file (directory-files custom-dir t "\\.el$"))
    (message "Loading %s" file)
    (load file)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-palenight))
 '(package-selected-packages
   '(consult corfu doom-themes exec-path-from-shell heaven-and-hell magit
	     marginalia moody orderless org-modern org-present ox-hugo
	     vertico visual-fill-column yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
