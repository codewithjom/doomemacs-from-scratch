;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix
      (expand-file-name "tmp/auto-saves/sessions/"
                        user-emacs-directory)
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "tmp/auto-saves/"
                                 user-emacs-directory) t)))

(set-default-coding-systems 'utf-8)

(blink-cursor-mode 1)
(setq-default tab-width 2)

;; (set-frame-parameter (selected-frame) 'alpha '(95 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-font (font-spec :family "Agave Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15)
      doom-big-font (font-spec :family "Agave Nerd Font" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)

(dolist (mode '(org-mode-hook
                vterm-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "\nKEYBINDINGS:\
\nFind file               (SPC .)     \
Open buffer list    (SPC b i)\
\nFind recent files       (SPC f r)   \
List of keybindings (SPC h b b)")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.doom.d/doom-emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 10)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer-name "*dashboard*")

(map! :leader
      (:prefix ("b". "buffer")))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq doom-theme 'doom-one)
(map! :leader)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)

(setq-default fill-column 80)

(after! org
  (setq org-directory "~/study-notes/"
        org-agenda-files '("~/study-notes/agenda/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t)

(defun jd/org-colors-doom-one ()
  (interactive)
  (dolist
      (face
       '((org-level-1 . 1.3)
         (org-level-2 . 1.2)
         (org-level-3 . 1.1)
         (org-level-4 . 1.0)
         (org-level-5 . 1.05)
         (org-level-6 . 1.05)
         (org-level-7 . 1.05)
         (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font doom-variable-pitch-font :weight 'bold :height (cdr face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0))

(defun jd/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(setq org-image-actual-width nil)

(jd/org-mode-setup)
(jd/org-colors-doom-one))

(defun jd/org-present-prepare-slide ()
  (org-overview)
  (org-fold-show-entry)
  (org-fold-show-children))

(defun jd/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
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
