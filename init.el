(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'textile-mode)
(require 'weblogger)
(require 'yasnippet-bundle)
(require 'snippet)
(require 'find-recursive)
(require 'inf-ruby)
(require 'ruby-mode)
(require 'rspec-mode)
(require 'css-mode)
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 
(require 'multiple-line-edit)
(require 'edit-server)
(edit-server-start)
(require 'browse-kill-ring)
(setq auto-mode-alist  (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(require 'keywiz)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))
(set-face-attribute 'default nil :family "Anonymous Pro" :height 180)
;; autoindent in Ruby
(add-hook 'ruby-mode-hook '(lambda ()
			     (local-set-key (kbd "RET") 'ruby-reindent-then-newline-and-indent)))
;; Rails functions
(defun tmr-devlog-shell ()
  "Tail the development log, shell"
  (interactive)
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name "devlog")))
  (shell (current-buffer))
  (process-send-string nil "cd .\n"); makes sure rvm variables set with .rvmrc
  (process-send-string nil "tail -f log/development.log\n"))
;; auto indent yanked text
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     plain-tex-mode  html-mode
						     js2-mode        css-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))
;; I use version control, don't annoy me with backup files everywhere
(setq make-backup-files nil)
(setq auto-save-default nil)
;; Speedbar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
;; eshell path in Mac
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
	 (replace-regexp-in-string "[[:space:]\n]*$" "" 
				   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
;; Ubuntu Gnome tricks
(menu-bar-enable-clipboard)
(setq x-select-enable-clipboard t)
;; Mode Compile
(autoload 'mode-compile "mode-compile" "Command to compile current buffer file based on the major mode" t)
(setq mode-compile-expert-p t)
(setq mode-compile-always-save-buffer-p t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile" "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
	 (message "Build maybe successful: closing window.")
	 (run-with-timer 1 nil                      
			 'delete-window              
			 (get-buffer-window buffer t)))
	(t                                                                    
	 (message "Compilation exited abnormally: %s" string))))
;; Color theme
(setq load-path (cons "~/.emacs.d/plugins/color-theme" load-path))
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/color-theme-bleu.el")
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
(require 'smtpmail)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(color-theme-bleu)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(ido-mode 'files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; My own functions
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun textilized-preview ()
  (interactive)
  (setq cur (get-buffer (current-buffer)))
  (mark-whole-buffer)
  (get-buffer-create "textilized")
  (shell-command-on-region (point-min) (point-max) "textile.rb" "textilized")
  (switch-to-buffer "textilized")
  (write-file "/tmp/1.html")
  (browse-url-of-file "/tmp/1.html")
  (kill-buffer "1.html")
  (switch-to-buffer cur)
  (delete-other-windows)
  (c-electric-backspace)
  )

(require 'http-post-simple)
(require 'url-auth)
(defun blog-post ()
  (interactive)
  (url-auth-user-prompt "http://weblog.reedglaw.com/admin/" "")
  (http-post-simple "http://weblog.reedglaw.com/admin/posts" (append(get-parms))))

(defun get-parms ()
  (list (cons 'post%5Bbody%5D (buffer-string))
	(cons 'post%5Btitle%5D (read-from-minibuffer "Post title: "))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KEY BINDINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f7] 'php-tokens)
(global-set-key [backtab] 'tab-next-buffer) ;forward reference
(global-set-key [f1] 'tab-next-buffer)
(global-set-key [f2] 'tab-prev-buffer)
(global-set-key [f3] 'kill-this-buffer)
(global-set-key [f11] 'delete-other-windows)
(global-set-key [f4] 'buffer-menu)
(global-set-key [f5] 'textilized-preview)
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TAB BROWSING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tab-next-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*scratch\\*" str)
   (string-match "^ " str)

   (memq str
	 (mapcar
	  (lambda (x)
	    (buffer-name
	     (window-buffer
	      (frame-selected-window x))))
	  (visible-frame-list)))
   ))

(defun tab-next (ls)
  "Switch to next buffer while skipping unwanted ones."
  (let* ((ptr ls)
	 bf bn go
	 )
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (tab-next-ignore bn))
	  (setq go bf)
	(setq ptr (cdr ptr))
	)
      )
    (if go
	(switch-to-buffer go))))

(defun tab-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (tab-next (reverse (buffer-list))))

(defun tab-next-buffer ()
  "Switch to the other buffer (2nd in list-buffers) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (tab-next (buffer-list)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nxhtml-skip-welcome t)
 '(speedbar-show-unknown-files t)
 '(weblogger-config-alist (quote (("chinagoer" ("user" . "reedlaw") ("server-url" . "http://www.chinagoer.com/xmlrpc.php") ("weblog" . "1"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(put 'erase-buffer 'disabled nil)

(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

(defun save-macro (name)                  
  "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro    
  (kmacro-name-last-macro name)         ; use this name for the macro    
  (find-file "~/.emacs.d/init.el")                ; open the .emacs file 
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro 
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(put 'downcase-region 'disabled nil)

(kmacro-push-ring (list 'country 0 "%d"))
(kmacro-pop-ring)

