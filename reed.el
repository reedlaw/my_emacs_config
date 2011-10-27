(setq nobreak-char-display nil)

(require 'multiple-line-edit)

(global-set-key (kbd "C-c s") 'speedbar-toggle)

;; Set up look
(load-theme 'tango-dark)
(set-face-attribute 'default nil :family "Anonymous Pro" :height 160)

;; Auto revert buffers
(global-auto-revert-mode)

(setq-default tab-width 2)

;; Prevent emacs from adding # -*- coding: utf-8 -* at the top of files
(setq ruby-insert-encoding-magic-comment nil)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
    "/usr/local/bin"
    "/usr/bin"
    "/home/reed/local/node/bin/node"
    ))

(setq debug-on-error nil)
(setq no-debug t)
;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(set-clipboard-coding-system 'utf-8)
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KEY BINDINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [backtab] 'tab-next-buffer) ;forward reference
(global-set-key [f1] 'tab-next-buffer)
(global-set-key [f2] 'tab-prev-buffer)
(global-set-key [f3] 'kill-this-buffer)
(global-set-key [f11] 'delete-other-windows)
(global-set-key [f4] 'buffer-menu)
(global-set-key [f5] 'textilized-preview)
(windmove-default-keybindings 'shift)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-c f") 'find-grep-dired-do-search)
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c d") 'remove-dos-eol)
(global-set-key "\C-c]" 'mulled/edit-trailing-edges)
(global-set-key "\C-c[" 'mulled/edit-leading-edges)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-t") 'switch-to-terminal)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TAB BROWSING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tab-next-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "-template-indent-buffer$" str)
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
 '(mac-option-modifier (quote meta))
 '(nxhtml-skip-welcome t)
 '(speedbar-show-unknown-files t)
 '(weblogger-config-alist (quote (("chinagoer" ("user" . "reedlaw") ("server-url" . "http://www.chinagoer.com/xmlrpc.php") ("weblog" . "1"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun switch-to-terminal ()
  "Switch to terminal buffer"
  (interactive)
  (switch-to-buffer (get-buffer "*terminal*")))

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
  (interactive "SName of the macro :") ; ask for the name of the macro    
  (kmacro-name-last-macro name)	     ; use this name for the macro    
  (find-file "~/.emacs.d/init.el")   ; open the .emacs file 
  (goto-char (point-max))	     ; go to the end of the .emacs
  (newline)			     ; insert a newline
  (insert-kbd-macro name)	     ; copy the macro 
  (newline)			     ; insert a newline
  (switch-to-buffer nil))	     ; return to the initial buffer

(put 'downcase-region 'disabled nil)

(kmacro-push-ring (list 'country 0 "%d"))
(kmacro-pop-ring)

(server-start)

(setq emerge-diff-options "--ignore-all-space")

(defconst ediff-ignore-similar-regions t)

;; Hack to yank the last yanked text http://stackoverflow.com/questions/5823495/emacs-how-to-yank-the-last-yanked-text-regardless-of-subsequent-kills
(defun jp/yank (&optional arg)
  "Yank and save text to register Y"
  (interactive)
  (set-register ?Y (current-kill 0 t))
  (yank arg))

(defun jp/yank-pop (&optional arg)
  "If yank-pop fails, then insert register Y"
  (interactive)
  (condition-case nil
      (yank-pop arg)
    (error (insert (get-register ?Y)))))

(global-set-key (kbd "M-y") (quote jp/yank-pop))
(global-set-key (kbd "C-y") (quote jp/yank))

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

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(setenv "PATH" (concat (getenv "PATH") ":/bin"))
(setq exec-path (append exec-path '("/bin")))

(defun find-grep-dired-do-search (dir regexp)
  "First perform `find-grep-dired', and wait for it to finish.
Then, using the same REGEXP as provided to `find-grep-dired',
perform `dired-do-search' on all files in the *Find* buffer."
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  (find-grep-dired dir regexp)
  (while (get-buffer-process (get-buffer "*Find*"))
    (sit-for 1))
  (with-current-buffer "*Find*"
    (dired-toggle-marks)
    (dired-do-search regexp)))
