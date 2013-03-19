;; add git to path
(setenv "PATH"
  (concat
   (getenv "PATH")
   ":" "/usr/local/git/bin"
  )
)

;; stop beeping!
(setq ring-bell-function 'ignore)

;; save history but not session
(savehist-mode 1)

;; global font lock mode
(global-font-lock-mode t)

;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; disable X selection copy-paste semantics
(setq mouse-drag-copy-region nil)
(delete-selection-mode 1)

;;
;; configure font & frame size
;;
(defun setup-frame (frame)
  (set-frame-parameter
   frame 'font
   "-apple-Monaco-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
  (if (window-system)
      (set-frame-height frame 60)))

;; Fontify current frame
(setup-frame (selected-frame))
;; Fontify any future frames
(push 'setup-frame after-make-frame-functions)

;;
;; use python-mode.el
;;
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;
;; whitespace, long lines, tabs in Python mode
;;
(setq whitespace-line-column 79)
(setq whitespace-style '(face tabs trailing lines-tail))

(add-hook 'python-mode-hook (lambda () (whitespace-mode 1)))
(add-hook 'python-mode-hook (lambda () (column-number-mode 1)))
(add-hook 'python-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'python-mode-hook (lambda () (setq py-indent-offset 4)))
(add-hook 'python-mode-hook (lambda () (setq python-indent 4)))

(add-hook 'python-mode-hook
	  (lambda () (setq whitespace-line-column 79
			     whitespace-style
			     '(face tabs trailing lines-tail))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun delete-trailing-blank-lines ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'delete-trailing-blank-lines)

(setq-default indicate-empty-lines t)

;;
;; Python outline mode
;;
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-variable-buffer-local 'beginning-of-defun-function)
		 'py-beginning-of-def-or-class)
	    (setq outline-regexp "def\\|class ")))

;;
;; external libraries
;;

;; set up load path
(add-to-list 'load-path "~/.emacs.d/")
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

;; smart tab completion
(require 'smart-tab)
(global-smart-tab-mode 1)

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
