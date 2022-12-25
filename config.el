;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(setq visible-bell t)
(blink-cursor-mode 1)
;; (global-hl-line-mode t)

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

(setq display-line-numbers-type t)

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)

(after! org
  (setq org-directory "~/Documents/org"
        org-agenda-files '("~/Documents/org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦))
        org-log-done 'time
        org-hide-emphasis-markers t))

;; (defun jd/org-colors ()
;;   (interactive)
;;   (dolist
;;       (face
;;        '((org-level-1 1.7 "#268bd2" ultra-bold)
;;          (org-level-2 1.6 "#d33682" extra-bold)
;;          (org-level-3 1.5 "#859900" bold)
;;          (org-level-4 1.4 "#b58900" semi-bold)
;;          (org-level-5 1.3 "#cb4b16" normal)
;;          (org-level-6 1.3 "#6c71c4" normal)
;;          (org-level-7 1.3 "#2aa198" normal)
;;          (org-level-8 1.3 "#657b83" normal)))
;;     (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weigth (nth 3 face) :height (nth 1 face) :foreground (nth 2 face))
;;     (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf")))
;;
;; (jd/org-colors)
