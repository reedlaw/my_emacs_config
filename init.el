(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("7fe1e3de3e04afc43f9a3d3a8d38cd0a0efd9d4c" "d14db41612953d22506af16ef7a23c4d112150e5" "3df5d5f3dc4b09cb2aa7c0748cfa37f63025230e" default)))
 '(mac-option-modifier (quote meta))
 '(nxhtml-skip-welcome t)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.mailgun.org")
 '(smtpmail-smtp-service 25)
 '(speedbar-show-unknown-files t)
 '(weblogger-config-alist (quote (("chinagoer" ("user" . "reedlaw") ("server-url" . "http://www.chinagoer.com/xmlrpc.php") ("weblog" . "1"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

