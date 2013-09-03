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
;; use python-mode.el
;;
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; but don't start the Python interpreter automatically
(custom-set-variables '(py-start-run-py-shell nil))


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

;; coffeescript mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(setq coffee-tab-width 2)

(add-hook 'coffee-mode-hook (lambda () (whitespace-mode 1)))
(add-hook 'coffee-mode-hook (lambda () (column-number-mode 1)))

(add-hook 'coffee-mode-hook
	  (lambda () (setq whitespace-line-column 79
			     whitespace-style
			     '(face tabs trailing lines-tail))))

(add-hook 'js-mode-hook (lambda () (whitespace-mode 1)))
(add-hook 'js-mode-hook (lambda () (column-number-mode 1)))

(add-hook 'js-mode-hook
	  (lambda () (setq whitespace-line-column 79
			     whitespace-style
			     '(face tabs trailing lines-tail))))

;; optionally, load local settings (e.g. font size)
(load "local.el" t)
