(when (file-exists-p "~/.Xmodmap")
  (shell-command "xmodmap ~/.Xmodmap"))

;;  Basic configuration (initial config)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :height 110)

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		inferior-python-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;(load-theme 'tango-dark)

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

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
  :config
  (ivy-mode 1))

;; CONFIGURACIÓN DE PYTHON
;; para manejar projectos (debe haber .git o crear un .projectile en la carpeta raiz)
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; manejar ambientes virtuales con conda
(use-package conda
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))  ; Ajusta la ruta según tu instalación
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t))

;; utilizar ipython por defecto
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

;; cargar autoreload siempre cuando se abra un ipython
(defun my-ipython-autoreload-setup ()
  (python-shell-send-string "%load_ext autoreload")
  (python-shell-send-string "%autoreload 2"))
(add-hook 'inferior-python-mode-hook 'my-ipython-autoreload-setup)

; mejorar autocompletado con company
(use-package company
  :hook ((python-mode . company-mode)
	 )
  :bind (:map company-active-map ("TAB" . company-complete-selection))
  )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (define-key lsp-mode-map [tab] nil)  ; Unbind the existing TAB binding
  (define-key lsp-mode-map (kbd "TAB") 'my/tab-completion-or-indent)  ; Rebind to custom function
  :commands lsp)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection)
(defun my/tab-completion-or-indent ()
  "Indents or completes depending on the context.
If preceding character is part of a word or a parenthetical, invoke `completion-at-point`.
Otherwise, indent the current line."
  (interactive)
  (if (looking-back "[\\w\\_\\-\\(\\)\\[\\]{}]" 1)
      (completion-at-point)
    (indent-for-tab-command)))
)


(use-package lsp-jedi)


;; autodocstrings
(use-package sphinx-doc
  :hook ((python-mode . sphinx-doc-mode))
  )

;; hover
(use-package lsp-ui :commands lsp-ui-mode)

(use-package which-key
    :config
    (which-key-mode))


(with-eval-after-load 'python
      (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
      (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input))


(add-hook 'python-mode-hook(lambda () (electric-pair-mode 1)))

(use-package code-cells
  :hook (python-mode . code-cells-mode)
  :bind (:map code-cells-mode-map
	      ("C-c C-c" . code-cells-eval)
	      ("C-c C-l" . code-cells-load)
	      ("C-c C-d" . code-cells-clear)
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

;; UI DOOM emacs
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))


;; personal commands
(defun conda-ac (&optional name)
  "Switch to environment NAME, prompting if called interactively."
  (interactive)
  (let* ((env-name (or name (conda--read-env-name)))
         (env-dir (conda-env-name-to-dir env-name)))
    (conda-env-activate-path env-dir))
  (revert-buffer :ignore-auto :noconfirm))

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
