(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(add-to-list 'default-frame-alist '(width  . 90))
(add-to-list 'default-frame-alist '(height . 32))

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
  (setq dashboard-items '((agenda . 5)))
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

(setq doom-theme 'doom-palenight)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(setq doom-font (font-spec :family "JetBrains Mono" :weight 'regular :size 14)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14)
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
        org-ellipsis " ???"
        org-startup-folded 'content
        org-superstar-headline-bullets-list '("???" "???" "???" "???" "???" "???" "???")
        org-log-done 'time
        org-hide-emphasis-markers t)
  (setq org-agenda-files
        '("~/org-files/todo.org"
          "~/org-files/books.org"
          "~/org-files/school.org"))

(set-face-attribute 'org-document-title nil :font "Agave Nerd Font" :weight 'medium :height 1.4)
(dolist (face '((org-level-1 . 1.3)
                (org-level-2 . 1.3)
                (org-level-3 . 1.3)
                (org-level-4 . 1.3)
                (org-level-5 . 1.3)
                (org-level-6 . 1.3)
                (org-level-7 . 1.3)
                (org-level-8 . 1.3)))
  (set-face-attribute (car face) nil :font "Agave Nerd Font" :weight 'medium :height (cdr face))))

(use-package org-roam
    :ensure t
    :init
    (setq org-roam-v2-ack t)
    :custom
    (org-roam-directory "~/org-files/notes/")
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
           ("C-c n i" . org-roam-node-insert)
           :map org-mode-map
           ("C-M-i" . completion-at-point)
           :map org-roam-dailies-map
           ("Y" . org-roam-dailies-capture-yesterday)
           ("T" . org-roam-dailies-capture-tomorrow))
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (require 'org-roam-dailies)
    (org-roam-db-autosync-enable))

(use-package prettier
  :config
  (prettier-mode t))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(defun jd/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun jd/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.3) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.3) org-code)
                                     (org-verbatim (:height 1.3) org-verbatim)
                                     (org-block (:height 1.3) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (jd/org-present-prepare-slide))

(defun jd/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun jd/org-present-prev ()
  (interactive)
  (org-present-prev)
  (jd/org-present-prepare-slide))

(defun jd/org-present-next ()
  (interactive)
  (org-present-next)
  (jd/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . jd/org-present-next)
         ("C-c C-k" . jd/org-present-prev))
  :hook ((org-present-mode . jd/org-present-hook)
         (org-present-mode-quit . jd/org-present-quit-hook)))
