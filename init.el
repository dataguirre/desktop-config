;; (when (file-exists-p "~/.Xmodmap")
;;   (shell-command "xmodmap ~/.Xmodmap"))

(require 'use-package)
(setq use-package-always-ensure t)


;;  Basic configuration (initial config)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :height 100)
(global-display-line-numbers-mode t)
;; para que tome snake_case como una palabra
(global-superword-mode 1)
(global-auto-revert-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		inferior-python-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-<") 'python-indent-shift-left) 
(global-set-key (kbd "C->") 'python-indent-shift-right)
(global-set-key (kbd "C-7") 'comment-line)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)
(setq use-package-always-ensure t)

;; Ojo, tener instalado Cmake y otras librerías, revisar en la documentación oficial
(use-package vterm
  :ensure t)


(use-package magit
  :config

  (defun exec-commitizen ()
  "Run 'uv run cz commit' in a vterm shell within the Git repository, then close the buffer."
  (interactive)
  (let* ((default-directory (magit-toplevel))
         (buffer-name "*magit-vterm*"))
    ;; Open or switch to a named vterm buffer
    (vterm buffer-name) 
    ;; Send the command
    (vterm-send-string "uv run cz commit") 
    (vterm-send-return) 
    (message "Command 'uv run cz commit' executed in vterm.")))

  ;; Add the custom option to the Magit commit popup
  (transient-append-suffix 'magit-commit "c"
    '("z" "commitizen" exec-commitizen))
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

; THEME SETUP
(use-package gruvbox-theme
  :ensure t
  :config
  (setq custom-safe-themes t)
  (load-theme 'gruvbox-dark-medium)
  )

;; (use-package modus-themes
;;   :ensure t
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil)

;;   ;; ;; Maybe define some palette overrides, such as by using our presets
;;   ;; (setq modus-themes-common-palette-overrides
;;         ;; modus-themes-preset-overrides-intense)
;;   (setq custom-safe-themes t)
;;   (setq modus-themes-to-toggle '(zenburn-theme modus-vivendi-tinted))
;;   ;; Load the theme of your choice.

;;   (modus-themes-load-theme 'zenburn-theme)

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(defun my-lsp-format-before-save ()
  "Formatea el buffer usando lsp antes de guardarlo."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))

;; (add-hook 'before-save-hook 'my-lsp-format-before-save)

(use-package ivy
  :diminish
  :bind (;("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
   :init
    (add-hook 'after-init-hook 'ivy-mode))

;; CONFIGURACION REMOTA
;; OJO, en el servidor remoto, se debe poner lo que está en .bashrc a .profile
;; Por ejemplo, la configuración de conda
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; CONFIGURACIÓN DE PYTHON

(add-hook 'python-mode-hook
          (lambda ()
            (setq display-fill-column-indicator-column 88)  ;; Ajusta 88 a tu preferencia
            (display-fill-column-indicator-mode t)))


(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Instalar silversearch antes
(use-package ag
  :ensure t
  :config
  ;; Aquí puedes poner configuraciones adicionales si es necesario
  )

;; Para que utilice ipdb en lugar de pdb
(setenv "PYTHONBREAKPOINT" "ipdb.set_trace")
(defun insert-python-breakpoint ()
  "Insert a Python breakpoint() statement at point."
  (interactive)
  (insert "breakpoint()")
  (newline-and-indent))
(global-set-key (kbd "C-c b") 'insert-python-breakpoint)

;; Autocompletado con company-mode
;; (use-package company
;;   :ensure t
;;   :hook (python-mode . company-mode))

(use-package numpydoc
  :ensure t
  :init
  (setq numpydoc-insert-examples-block nil) ; Skip Examples block
  :config
  (add-hook 'python-mode-hook 'numpydoc-mode)
  :bind (("C-c n" . numpydoc-generate)))


(defun my-ipython-autoreload-setup ()
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 2"))



(defun get-path-ipython ()
  "Return the file path if it exists, otherwise nil, based on a constructed file path."
  (let ((file (concat (pet-virtualenv-root) "bin/ipython")))
    (if (file-exists-p file)
        (file-local-name file)  ;; Return the file path if it exists
      nil))
  )  ;; Return nil if the file does not exist

(defun get-path-jedi-lsp ()
  "Return the file path if it exists, otherwise nil, based on a constructed file path."
  (let ((file (concat (pet-virtualenv-root) "bin/jedi-language-server")))
    (if (file-exists-p file)
        (if (file-remote-p file)
            "jedi-language-server"  ;; Return 'ruff' if the file is remote
          (file-local-name file))  ;; Otherwise, return the local file path
      nil)))  ;; Return nil if the file does not exist

(defun get-path-ruff-lsp ()
  "Return the file path if it exists, otherwise nil, based on a constructed file path."
  (let ((file (concat (pet-virtualenv-root) "bin/ruff")))
    (if (file-exists-p file)
        (if (file-remote-p file)
            "ruff"  ;; Return 'ruff' if the file is remote
          (file-local-name file))  ;; Otherwise, return the local file path
      nil)))  ;; Return nil if the file does not exist

;; OJO: para que lsp funcione en remoto, toca agregar el bin de los proyecto en .bashrc del remoto
(use-package pet
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (get-path-ipython)
			  python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True --InteractiveShell.colors='Linux' --InteractiveShell.autoindent=True"
			  python-shell-completion-native-enable nil
                          python-shell-virtualenv-root (pet-virtualenv-root)
			  )
	      (setq-local lsp-jedi-executable-command (get-path-jedi-lsp)
			  )
	      (setq-local lsp-ruff-server-command (list (get-path-ruff-lsp) "server")
			  )
	      ))
  (add-hook 'inferior-python-mode-hook 'my-ipython-autoreload-setup))
(add-hook 'python-mode-hook(lambda () (electric-pair-mode 1)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map [tab] nil)  ; Unbind the existing TAB binding
  (define-key lsp-mode-map (kbd "TAB") 'my/tab-completion-or-indent)  ; Rebind to custom function
    ;; Especifica solo los servidores LSP que quieres usar
  (setq lsp-disabled-clients '(lsp-copilot copilot-lsp copilot-ls))
  :commands lsp)
(use-package lsp-jedi
  :ensure t)

;; Para manipular celdas
(use-package code-cells
  :hook (python-mode . code-cells-mode)
  :bind (:map code-cells-mode-map
	      ("C-c C-c" . code-cells-eval)
	      ("C-c C-l" . code-cells-load)
	      ("C-c C-d" . code-cells-delete)
	      ("C-M-<up>" . code-cells-backward-cell)
	      ("C-M-<down>" . code-cells-forward-cell)
	     ; ("C-c C-<up>" . code-cells-move-cell-up)
	     ; ("C-c C-<down>" . code-cells-move-cell-down)
	      ("C-c C-n" . insert-new-cell)
          ("C-c C-m" . code-cells-move-mode))
  :config
  (defvar code-cells-move-mode-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<up>") 'code-cells-move-cell-up)
      (define-key map (kbd "<down>") 'code-cells-move-cell-down)
      map)
    "Keymap para `code-cells-move-mode`.")

  (define-minor-mode code-cells-move-mode
    "Modo menor para moverse entre celdas en `code-cells-mode`."
    :lighter " CellMove"
    :keymap code-cells-move-mode-keymap
    (if code-cells-move-mode
        (message "Move mode for code cells activated.")
      (message "Move mode for code cells deactivated.")))
  (defun insert-new-cell ()
  "Insert a new code cell below the current position."
  (interactive)
  (end-of-line)
  ;; Ensure there is one line between cells.
  (unless (looking-at-p "\n#%%")
    (newline))
  (newline)
  (insert "#%%\n")))

(defun my-save-and-eval-cell ()
  "Guarda el buffer y luego evalúa la celda de código."
  (interactive)
  (save-buffer)                      ; Guarda el buffer actual
  (call-interactively 'code-cells-eval)) ; Llama a code-cells-eval interactivamente

(with-eval-after-load 'code-cells
  (define-key code-cells-mode-map (kbd "C-c C-c") 'my-save-and-eval-cell))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  ;; :custom ((doom-modeline-height 25))
  )


;; personal commands
(defun connect-vpn (vpn-name)
  "Open a buffer and run the VPN connection using the Bash function `connect-vpn` with VPN-NAME as an argument."
  (interactive "sEnter VPN name: ")
  (let ((buffer-name (format "*VPN-%s*" vpn-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (progn
        (vterm buffer-name)
        (vterm-send-string (format "connect-vpn %s" vpn-name))
        (vterm-send-return)))))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
