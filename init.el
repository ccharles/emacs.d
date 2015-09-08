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
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
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

;; ERC is awesome, but I don't need to know when people show up or leave
(eval-after-load "erc" '(setf erc-hide-list '("JOIN" "PART" "QUIT" "AWAY" "MODE" "NICK")))

;;; Appearance

;; Enable current line highlighting
(if (fboundp 'global-hl-line-mode)
    (global-hl-line-mode))

;; Show empty lines in the fringe
(setq-default indicate-empty-lines t)

(when (fboundp 'prettify-symbols-mode)
  (global-prettify-symbols-mode))

(auto-insert-mode)

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(when (boundp 'electric-pair-mode)
  (electric-pair-mode))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Packages
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setf package-archive-priorities
      '(("melpa" . 20)
        ("gnu" . 0)))

(package-refresh-contents)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

;;; Avy
(use-package avy
  :ensure t
  :config
  (progn
    (global-set-key (kbd "<insert>") 'avy-goto-char)
    (global-set-key (kbd "M-g g") 'avy-goto-line)
    (global-set-key (kbd "M-g M-g") 'avy-goto-line)))

;;; C
(defun my-c-mode-hook ()
  "Turn on semantic."
  (semantic-mode)
  (semantic-idle-summary-mode)

  (eval-after-load "irony"
    '(when (member major-mode irony-supported-major-modes)
       (irony-mode 1))))

(add-hook 'c-mode-hook #'my-c-mode-hook)

;;; Clojure
(use-package cider
  :ensure cider
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))

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
(use-package highlight-quoted
  :ensure highlight-quoted
  :config (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :config (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

;;; Expand-region
(use-package expand-region
  :ensure expand-region
  :bind ("C-=" . er/expand-region))

;;; Floobits
(use-package floobits
  :ensure t)

;;; Flycheck
(use-package flycheck
  :ensure flycheck
  :config (global-flycheck-mode 1))

;;; Fonts
(use-package dynamic-fonts
  :ensure dynamic-fonts
  :config
  (progn
    (setf dynamic-fonts-preferred-monospace-fonts
          '("Source Code Pro" "Roboto Mono" "Ubuntu Mono"
            "Consolas" "Courier New" "Monospace"))
    (setf dynamic-fonts-preferred-monospace-point-size 11)
    (setf dynamic-fonts-preferred-proportional-point-size 11)
    (dynamic-fonts-setup)))

;;; Gmail
(use-package gmail-message-mode
  :ensure gmail-message-mode)

(use-package ham-mode
  :ensure ham-mode
  :init
  (setf ham-mode-markdown-command
        '("/usr/bin/pandoc" "--from" "markdown" "--to" "html" "--standalone" file)))

;;; Helm
;;;
;;; Much of this is inspired by
;;;     http://tuhdo.github.io/helm-intro.html

;; Temporary workaround for Emacs trunk
;;   From https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19552
(unless (functionp 'class-slot-initarg)
  (defun class-slot-initarg (class-name slot)
    (eieio--class-slot-initarg (eieio--class-v class-name) slot)))

(use-package helm-config
  :ensure helm
  :config
  (progn
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-unset-key (kbd "C-x c"))

    (eval-after-load "helm"
       '(progn
          (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-z") 'helm-select-action)

          ;; http://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
          (setf helm-display-header-line nil)
          ;; (set-face-attribute 'helm-source-header nil :height 0)
          (helm-autoresize-mode 1)
          (setf helm-split-window-in-side-p t)))

    (helm-mode 1)))

;;; JavaScript
(use-package tern
  :ensure tern
  :config
  (progn
    (add-hook 'js-mode-hook #'tern-mode)
    (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js-mode))
    (add-to-list 'auto-mode-alist '("\\.tern-config\\'" . js-mode))))

(use-package company-tern
  :ensure company-tern
  :config
  (eval-after-load 'tern
    '(add-to-list 'company-backends 'company-tern)))

(eval-after-load "js-mode"
  (progn
    (setf js-indent-level 2)
    (add-hook 'js-mode-hook #'subword-mode)))

;;; Less
(use-package less-css-mode
  :ensure less-css-mode)

;;; Lua
(use-package lua-mode
  :ensure lua-mode
  :config (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

;;; Magit
(use-package magit
  :ensure magit
  :bind ("C-c g" . magit-status)
  :init (setf magit-last-seen-setup-instructions "1.4.0"))

(use-package magit-gitflow
  :ensure magit-gitflow
  :config (eval-after-load "magit"
            '(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

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
(defun my-disable-electric-indent ()
  "Disable electric indenting."
  (electric-indent-local-mode -1))

(use-package markdown-mode
  :ensure markdown-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
  :init
  (add-hook 'markdown-mode-hook
            #'my-disable-electric-indent))

;;; Multiple cursors
(use-package multiple-cursors
  :ensure multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;;; Olivetti
(use-package olivetti
  :ensure olivetti
  :config
  (progn
    (setf olivetti-body-width 80)
    (visual-line-mode)))

;;; Org
(add-to-list 'load-path "/home/chris/code/org-mode/lisp/")

(eval-after-load 'org
  '(if (boundp 'org-highlight-latex-and-related) ; Emacs 24.4 and later
       (setf org-highlight-latex-and-related '(latex))
     (setf org-highlight-latex-fragments-and-specials t)))

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
    (plist-put org-format-latex-options :background 'default)
    (setf org-hide-emphasis-markers t)
    (setf org-ctrl-k-protect-subtree 'error)
    (setf org-catch-invisible-edits 'show-and-error)))

;; Page breaks
(use-package page-break-lines
  :ensure page-break-lines
  :init (global-page-break-lines-mode))

;;; Paredit
;;;
;;; Unfortunately, smart-parens is not yet available via
;;; MELPA-stable. paredit will have to do!
(use-package paredit
  :ensure paredit
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (eval-after-load "cider"
      '(add-hook 'clojure-mode-hook #'enable-paredit-mode))
    (eval-after-load "eldoc"
      '(eldoc-add-command
        'paredit-backward-delete
        'paredit-close-round))))

;;; PHP
(use-package php-mode
  :ensure php-mode)

(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.php\\'" . "PHP skeleton")
     '("Short description: "
       "<?php\n\n")))

;;; PKGBUILD
(use-package pkgbuild-mode
  :ensure pkgbuild-mode
  :config
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;;; Popwin
(use-package popwin
  :ensure popwin
  :config
  (add-to-list 'display-buffer-alist
               '(popwin:display-buffer-condition popwin:display-buffer-action)))

;;; Projectile
(use-package projectile
  :ensure projectile
  :config
  (progn
    (projectile-global-mode)))

(use-package helm-projectile
  :ensure helm-projectile
  :init (helm-projectile-on)
  :config (setf projectile-switch-project-action 'projectile-vc)
  :bind ("C-c p h" . helm-projectile))

;;; Puppet
(use-package puppet-mode
  :ensure puppet-mode
  :config
  (add-hook 'puppet-mode-hook
            (lambda () (push '("=>" . ?⇒) prettify-symbols-alist))))

;;; Python
(use-package jedi
  :ensure jedi
  :config
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (setf jedi:install-imenu t)))

(add-hook 'python-mode #'subword-mode)

(use-package pyvenv
  :ensure pyvenv)

;;; R
(use-package ess
  :ensure ess)

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

;;; Rainbow mode
(use-package rainbow-mode
  :ensure rainbow-mode
  :init
  (progn
    (add-hook 'css-mode-hook #'rainbow-mode)
    (add-hook 'less-mode-hook #'rainbow-mode)))

;;; Real GUD
(use-package realgud
  :ensure realgud)

;;; Smart parens (apparently not available via MELPA-stable)
;; (use-package smartparens
;;   :ensure smartparens
;;   :config
;;   (progn
;;     (require 'smartparens-config nil t)
;;     (smartparens-global-mode)
;;     (sp-use-smartparens-bindings)
;;     (eval-after-load "web-mode" '(add-to-list 'sp-ignore-modes-list 'web-mode))))

;;; Vagrant
(use-package vagrant
  :ensure vagrant)

;;; Web mode
(use-package web-mode
  :ensure web-mode
  :init (add-to-list 'auto-mode-alist
                     '("\\.html.django\\'" . web-mode))
  :config
  (progn
    (setf web-mode-markup-indent-offset 2)
    (setf web-mode-code-indent-offset 2)))

;;; Whitespace butler
(use-package ws-butler
  :ensure ws-butler
  :config (ws-butler-global-mode))

;;; Wrap region
(use-package wrap-region
  :ensure wrap-region
  :init (wrap-region-global-mode))

;;; YAML
(use-package yaml-mode
  :ensure yaml-mode)

;;; Yasnippet
(use-package yasnippet
  :ensure yasnippet
  :config
  (progn
    (setq-default yas-prompt-functions
                  '(yas-ido-prompt yas-x-prompt yas-completing-prompt yas-no-prompt))
    (setf yas-wrap-around-region t)
    (yas-global-mode)))

(use-package emr
  :ensure t
  :bind ("M-RET" . emr-show-refactor-menu)
  :config (add-hook 'prog-mode-hook #'emr-initialize))

(use-package srefactor
  :ensure t
  :config
  (progn
    (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)))

(use-package js2-refactor
  :ensure t)

(use-package irony
  :ensure t
  :config
  (progn
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :ensure t
  :config
  (progn
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(progn
       (add-to-list 'flycheck-checkers 'irony)
       (add-to-list 'flycheck-disabled-checkers 'c/c++-clang))))

(use-package irony-eldoc
  :ensure t
  :config (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package tagedit
  :ensure t
  :init (eval-after-load "sgml-mode"
          '(progn
             (tagedit-add-paredit-like-keybindings)
             (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))))

(use-package yafolding
  :ensure t
  :config (add-hook 'prog-mode-hook #'yafolding-mode))

(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

(use-package ensime
  :ensure t
  :config (add-hook 'scala-mode-hook #'ensime-scala-mode-hook))

(use-package adoc-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.adoc" . adoc-mode)))

(use-package dockerfile-mode
  :ensure t)

(use-package sx
  :ensure t)

(use-package annotate :ensure t)

(use-package csv-mode :ensure t)

(use-package rotate :ensure t)

(use-package indent-guide
  :ensure t
  :config
  (progn
    (indent-guide-global-mode)
    (set-face-foreground 'indent-guide-face "lightgray")))

(use-package git-gutter-fringe
  :ensure t
  :config (global-git-gutter-mode))

(provide 'init)

;;; init.el ends here
