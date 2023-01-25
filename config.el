;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; (set-frame-parameter (selected-frame) 'alpha '(95 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(width  . 85))
(add-to-list 'default-frame-alist '(height . 34))

(use-package dashboard
  :ensure t
  :config      ;; tweak dashboard config before loading it
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title " ")
  ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.doom.d/logo.svg")
  (setq dashboard-center-content 't) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer-name "*dashboard*")

(beacon-mode 1)

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(setq doom-font (font-spec :family "JetBrains Mono" :weight 'regular :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(column-number-mode)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0) (hl-line-mode -1))))

(dolist (mode '(markdown-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0) (hl-line-mode -1))))

(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(setq-default visual-fill-column-center-text t)
(setq-default visual-fill-column-width 95)

(after! org
  (setq org-directory "~/org-files/"
        org-ellipsis " ▾"
        org-startup-folded 'overview
        org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")
        org-log-done 'time
        org-hide-emphasis-markers t)
  (setq org-agenda-files
        '("~/org-files/todo.org"
          "~/org-files/school.org"))

(set-face-attribute 'org-document-title nil :font "JetBrains Mono" :weight 'bold :height 1.1)
(dolist (face '((org-level-1 . 1.0)
                (org-level-2 . 1.0)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'medium :height (cdr face))))

(use-package org-roam
    :ensure t
    :init
    (setq org-roam-v2-ack t)
    :custom
    (org-roam-directory "~/org-files/")
    (org-roam-completion-everywhere t)
    (org-roam-capture-templates
     '(("d" "default" plain
        "%?"
        :if-new (file+head "${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)
       ("l" "programming language" plain
        "* Get Started\n\n- Topic: %?\n- Language: \n\n"
        :if-new (file+head "${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)
       ("b" "book" plain
        "\n#+author: Jom Dollesin\n\n"
        :if-new (file+head "${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)
       ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
        :unnarrowed t)))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert))
    :config
    (org-roam-db-autosync-enable))

(use-package prettier
  :config
  (prettier-mode t))

(use-package org-appear
  :hook (org-mode . org-appear-mode))
