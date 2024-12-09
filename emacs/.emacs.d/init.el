(setq inhibit-startup-message t)

(require' package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contends)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package tex
  :ensure auctex
  :config

  ;; Indent items by two spaces.
  (setq LaTeX-item-indent 0)
  
  (setq TeX-PDF-mode t)

  ;; Generate sync file and sync with C-v
  (eval-after-load
      "tex" '(add-to-list 'TeX-command-list
                          '("latexmk" "latexmk -pdf %t --synctex=1" TeX-run-TeX)))

  (setq latex-run-command "pdflatex")
  (setq LaTeX-command "latex --synctex=1")

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex)

  (setq TeX-auto-save t
        TeX-parse-self t)

  ;; Needed to sync TeX and PDF
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (TeX-source-correlate-mode 1)))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  ;; (setq auto-revert-interval 0.5)

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-view-fit-page-to-window) ))

  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (reftex-mode))))



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

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0blayout which-key try pdf-tools org-bullets gruvbox-theme auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
