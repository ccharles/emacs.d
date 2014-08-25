;;; init.el --- Personal Emacs configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Chris Charles

;; Author: Chris <chris@ccharles.ca>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Basic settings for built-in stuff

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t)

(display-time-mode t)
(setq column-number-mode t)
(show-paren-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(if (require 'uniquify nil t)
    (setq uniquify-buffer-name-style 'forward))

;; From http://www.reddit.com/r/emacs/comments/d2t4q/scratch_buffers_for_emacs/c0x7a68
;;
;; Automatically select the appropriate major mode for buffers
;; (setq-default major-mode
;;  (lambda ()
;;    (let ((buffer-file-name (or buffer-file-name (buffer-name))))
;;      (set-auto-mode))))

(setq enable-remote-dir-locals t)
(recentf-mode 1)

;; Separate the custom file
;; http://www.emacsblog.org/2008/12/06/quick-tip-detaching-the-custom-file/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; http://whattheemacsd.com/init.el-02.html
;;
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; http://whattheemacsd.com/init.el-03.html
;;
;; Save point position between sessions

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Typing `yes' all the time is a pain
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 77)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Enable windmove and winner
(when (locate-library "windmove")
  (global-set-key (kbd "<left>") 'windmove-left)
  (global-set-key (kbd "<right>") 'windmove-right)
  (global-set-key (kbd "<up>") 'windmove-up)
  (global-set-key (kbd "<down>") 'windmove-down))

(winner-mode t)

;; Use Ediff in one frame
(eval-after-load "ediff"
  '(set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain))


;;; Appearance

;; Enable current line highlighting
(if (fboundp 'global-hl-line-mode)
    (global-hl-line-mode))

;; Show empty lines in the fringe
(setq-default indicate-empty-lines t)

(if (window-system)
  (set-face-attribute 'default nil :font "Source Code Pro-12"))

(global-prettify-symbols-mode)

(provide 'init)


;;; Packages
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;;; Ace-jump
(use-package ace-jump-mode
  :ensure ace-jump-mode
  :init (define-key global-map (kbd "<insert>") 'ace-jump-mode))

;;; Clojure
(use-package cider
  :ensure cider
  :config (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package clojure-mode
  :ensure clojure-mode
  :config (add-hook 'clojure-mode-hook 'cider-mode))

;;; CoffeeScript
(use-package coffee-mode
  :ensure coffee-mode)

;;; Common Lisp (need to play with this a bit)
;; (use-package slime
;;   :init
;;   (require 'slime-autoloads)
;;   (setq inferior-lisp-program "/usr/bin/sbcl")
;;   (setq slime-contribs '(slime-fancy))
;;   (add-to-list 'slime-contribs 'slime-autodoc))

;; (use-package slime-company
;;   :config
;;   (eval-after-load "slime" '(slime-setup '(slime-company))))

;;; Colour theme
(use-package color-theme-sanityinc-tomorrow
  :ensure color-theme-sanityinc-tomorrow
  :config (load-theme 'sanityinc-tomorrow-day))

;;; Company
(use-package company
  :ensure company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (bind-key (kbd "M-<tab>") 'company-complete company-mode-map)))

;;; Drag stuff
(use-package drag-stuff
  :ensure drag-stuff
  :init
  (progn
    (drag-stuff-global-mode)
    (add-to-list 'drag-stuff-except-modes 'org-mode)))

;;; Edit server
(use-package edit-server
  :ensure edit-server
  :config (edit-server-start))

;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook (eldoc-mode))

;;; Expand-region
(use-package expand-region
  :ensure expand-region
  :bind ("C-=" . er/expand-region))

;;; Flycheck
(use-package flycheck
  :ensure flycheck
  :config (global-flycheck-mode 1))

;;; Ido
(use-package ido
  :config
  (progn
    (ido-mode +1)
    (ido-everywhere +1)))

(use-package ido-ubiquitous
  :ensure ido-ubiquitous
  :config (ido-ubiquitous-mode +1))

(use-package flx-ido
  :ensure flx-ido
  :config (flx-ido-mode +1))

;;; JavaScript
(use-package tern
  :ensure tern
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (tern-mode t)))
    (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js-mode))
    (add-to-list 'auto-mode-alist '("\\.tern-config\\'" . js-mode))))

(use-package company-tern
  :ensure company-tern
  :config
  (eval-after-load 'tern
    '(add-to-list 'company-backends 'company-tern)))

;;; Lua
(use-package lua-mode
  :ensure lua-mode
  :config (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

;;; Magit
(use-package magit
  :ensure magit
  :bind ("C-c g" . magit-status))

;; From http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  "Make magit use the full frame."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;;; Markdown
(use-package markdown-mode
  :ensure markdown-mode
  :init
  (add-hook 'markdown-mode-hook
            (lambda () (electric-indent-local-mode -1))))

;;; Multiple cursors
(use-package multiple-cursors
  :ensure multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;;; Org
(use-package org
  :bind ("C-c a" . org-agenda)
  :config
  (progn
    ;; I guess this isn't autoloaded anymore?
    (require 'org-agenda nil t)

    (setq-default org-src-fontify-natively t)
    (setq-default org-log-done 'time)
    (setq-default org-clock-out-remove-zero-time-clocks t)
    (setq-default org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 5 :compact t))
    (setq-default org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    (plist-put org-format-latex-options :scale 1.2)
    (plist-put org-format-latex-options :foreground 'default)
    (plist-put org-format-latex-options :background 'default)))

;;; Popwin
(use-package popwin
  :ensure popwin
  :config
  (add-to-list 'display-buffer-alist
               '(popwin:display-buffer-condition popwin:display-buffer-action)))

;;; Projectile
(use-package projectile
  :ensure projectile
  :config (projectile-global-mode))

;;; Python
(use-package jedi
  :ensure jedi
  :config (add-hook 'python-mode-hook 'jedi:setup))

(use-package pyvenv
  :ensure pyvenv)

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

;;; Smart parens (apparently not available via MELPA-stable)
;; (use-package smartparens
;;   :ensure smartparens
;;   :config
;;   (progn
;;     (require 'smartparens-config nil t)
;;     (smartparens-global-mode)
;;     (sp-use-smartparens-bindings)
;;     (eval-after-load "web-mode" '(add-to-list 'sp-ignore-modes-list 'web-mode))))

;;; Web mode
(use-package web-mode
  :ensure web-mode
  :init (add-to-list 'auto-mode-alist
                     '("\\.html.django\\'" . web-mode)))

;;; Whitespace butler
(use-package ws-butler
  :ensure ws-butler
  :config (ws-butler-global-mode))

;;; Wrap region
(use-package wrap-region
  :ensure wrap-region
  :init (wrap-region-global-mode))

;;; Yasnippet (missing dropdown-list)
;; (use-package yasnippet
;;   :ensure yasnippet
;;   :config
;;   (progn
;;     (setq-default yas-prompt-functions
;;           '(yas-x-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt))
;;     (yas-global-mode 1)))

(provide 'init)

;;; init.el ends here
