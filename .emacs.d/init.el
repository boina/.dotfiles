(setq inhibit-startup-message t)  ; Inhibit startup-message
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)       ; Set up the visible bell

(load-theme 'wombat)

;; Function copied from https://github.com/daviwil/
(defun set-font-faces ()
    (message "Setting faces.")
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 115))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (set-font-faces))))
  (set-font-faces))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'norerror 'nomessage)

;;Revert buffers when the file changes in the disk
(global-auto-revert-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(setq set-mark-command-repeat-pop t)

;;Avy configuration
(use-package avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-;") 'avy-goto-char-2)
(global-set-key (kbd "C-.") 'avy-goto-word-1)

;; Change windows with shift key
;(windmove-default-keybindings)


(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;(use-package company-box
;;  :ensure t
;;  :hook (company-mode . company-box-mode))

;;Dired configuration
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-lgh --group-directories-first"))
  :config (setq dired-kill-when-opening-new-dired-buffer 1)
  :hook (diredfl-mode))

(setq delete-by-moving-to-trash 1)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes 1
	doom-modeline-height 30))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vertico
  :ensure t
    :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :after vertico
  :ensure t
;;  :custom
;;  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package savehist
  :config
  (setq history-length 25)
  :init
  (savehist-mode))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  ;;  :custom
  ;;  (consult-project-root-function #'dw/get-project-root)
  ;;  (completion-in-region-function #'consult-completion-in-region)
  :config
  (consult-customize))


;;(use-package ivy
;;  :diminish
;;  :bind (("C-s" . swiper)
;;         :map ivy-minibuffer-map
;;         ("TAB" . ivy-alt-done)
;;         ("C-l" . ivy-alt-done)
;;         ("C-j" . ivy-next-line)
;;         ("C-k" . ivy-previous-line)
;;         :map ivy-switch-buffer-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-l" . ivy-done)
;;         ("C-d" . ivy-switch-buffer-kill)
;;         :map ivy-reverse-i-search-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-d" . ivy-reverse-i-search-kill))
;;  :config
;;  (ivy-mode 1))
;;
;;(use-package ivy-rich
;;  :ensure t
;;  :after ivy
;;  :init
;;  (ivy-rich-mode 1))
;

;;(use-package counsel
;;  :ensure t
;;  :custom
;;  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;  :config
;;  (counsel-mode 1)
;;  (setq ivy-initial-inputs-alist nil))
;;
;;(use-package ivy-prescient
;;  :after counsel
;;  :custom
;;  (ivy-prescient-enable-filtering nil)
;;  :config
;;  (prescient-persist-mode 1)
;;  (ivy-prescient-mode 1))
;;

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)

(global-set-key (kbd "TAB") 'minibuffer-complete)

(use-package expand-region
  :ensure t
  :bind ("C-]"  . er/expand-region))

(use-package ess
  :mode ("\\.R\\'" . R-mode))

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
 
(use-package smartparens
   :hook (ess-mode  . smartparens-mode)
         (r-mode    . smartparens-mode)
         (prog-mode . smartparens-mode)
         (Emacs-Lisp-mode . smartparens-mode)
   :config (setq show-paren-delay 0)
           (show-paren-mode 1))
 
;;Configuration of text mode
(defun text-mode-setup ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (company-mode -1))
  ;;(flyspell-buffer))

(add-hook 'text-mode-hook 'text-mode-setup)


(defun org-mode-setup ()
  (org-indent-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-agenda-files
	'("~/OrgFiles/Tasks.org"
	  ;;"~/OrgFiles/Habits.org"
	  "~/OrgFiles/TrainingForClimbing.org"
  	  "~/OrgFiles/Workout.org"
          "~/Labo/PD_Malhotra_CRG/Projects/Mucin sorting/Mucin tagging/MucinFluorescentTagging.org"))
  (setq org-agenda-custom-commands
	'(("d" "What's up for today?"
	   ((alltodo)
 	     ;;"GHD+ACTIVE+PRIORITY=\"A\""
	     ;;((org-agenda-files '("~/org/goals.org"))
	     ;;(org-agenda-overriding-header "Primary goals this month"))
	   (agenda "" ((org-agenda-span 1)
			(org-agenda-overriding-header "Today")))))
	  ("w" "And the week?"
	   ((tags-todo "labo")
	     ;;((org-agenda-files '("~/org/goals.org"))
	     ;; (org-agenda-overriding-header "Secondary goals this month")))
	    (agenda)))))
  (setq org-capture-templates
	'(("t" "Tasks")
	  ("tt" "Task" entry (file+olp "~/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	  ("m" "Metrics Capture")
	  ("mp" "Blood pressure" table-line (file+headline "~/OrgFiles/Metrics.org" "Pressure")
	   "| %U | %^{High} | %^{Low} | %^{Heart rate} | %^{Notes} |" :kill-buffer t)
	  ("mr" "Running" table-line (file+headline "~/OrgFiles/Metrics.org" "Running")
	   "| %U | %^{Distance} | %^{Time} | %^{Pace} | %^{Notes} |" :kill-buffer t)
	  ("n" "Notes")
	  ("np" "Como estuvo el día?" table-line (file "~/OrgFiles/ElDia.org")
	   "| %U | %^{General} | %^{Notes} |" :kill-buffer t))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)


;;org Roam
(use-package org-roam
  :ensure t
  ;;:hook
  ;;(after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/OrgFiles/RoamNotes/")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(("d" "default" plain
      #'org-roam-capture--get-point
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n"
      :unnarrowed t)
     ("ll" "link note" plain
      #'org-roam-capture--get-point
      "* %^{Link}"
      :file-name "Inbox"
      :olp ("Links")
      :unnarrowed t
      :immediate-finish)
     ("lt" "link task" entry
      #'org-roam-capture--get-point
      "* TODO %^{Link}"
      :file-name "Inbox"
      :olp ("Tasks")
      :unnarrowed t
      :immediate-finish)))
  (org-roam-dailies-directory "Journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "Journal/%<%Y-%m-%d>"
       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
      ("t" "Task" entry
       #'org-roam-capture--get-point
       "* TODO %?\n  %U\n  %a\n  %i"
       :file-name "Journal/%<%Y-%m-%d>"
       :olp ("Tasks")
       :empty-lines 1
       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
      ("j" "journal" entry
       #'org-roam-capture--get-point
       "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
       :file-name "Journal/%<%Y-%m-%d>"
       :olp ("Log")
       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
      ("l" "log entry" entry
       #'org-roam-capture--get-point
       "* %<%I:%M %p> - %?"
       :file-name "Journal/%<%Y-%m-%d>"
       :olp ("Log")
       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
      ("m" "meeting" entry
       #'org-roam-capture--get-point
       "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
       :file-name "Journal/%<%Y-%m-%d>"
       :olp ("Log")
       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  :bind (:map org-roam-mode-map
          (("C-c n l"   . org-roam)
           ("C-c n f"   . org-roam-find-file)
           ("C-c n d"   . org-roam-dailies-find-date)
           ("C-c n c"   . org-roam-dailies-capture-today)
           ("C-c n C r" . org-roam-dailies-capture-tomorrow)
           ("C-c n t"   . org-roam-dailies-find-today)
           ("C-c n y"   . org-roam-dailies-find-yesterday)
           ("C-c n r"   . org-roam-dailies-find-tomorrow)
           ("C-c n g"   . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :config
  (org-roam-setup))



;;(setq org-refile-targets
;;      '(("Archive.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
;;(advice-add 'org-refile :after 'org-save-all-org-buffers)
;;
;;
;;      ("j" "Journal Entries")
;;      ("jj" "Journal" entry
;;           (file+olp+datetree "~/OrgFiles/Journal.org")
;;           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
;;           ;; ,(dw/read-file-as-strreing "~/Notes/Templates/Daily.org")
;;           :clock-in :clock-resume
;;           :empty-lines 1)
;;      ("jm" "Meeting" entry
;;           (file+olp+datetree "~/OrgFiles/Journal.org")
;;           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
;;           :clock-in :clock-resume
;;           :empty-lines 1)
;;
;;      ("w" "Workflows")
;;      ("we" "Checking Email" entry (file+olp+datetree "~/OrgFiles/Journal.org")
;;           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
;;
;;
;;
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((R          . t)
;;   (shell      . t)
;;   (emacs-lisp . t)
;;   (python     . t)
;;   ))
;;
;;(setq org-babel-python-command "python3")
;;     
;;(setq org-confirm-babel-evaluate nil)
;;
;;(require 'org-tempo)
;;
;;(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;;(add-to-list 'org-structure-template-alist '("r" . "src R"))
;;(add-to-list 'org-structure-template-alist '("py" . "src python"))
;;(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;
;;

;;(require 'emms-setup)
;;(emms-all)
;;(emms-default-players)
;;(setq emms-source-file-default-directory "~/Música/")
;;

;;(use-package ox-reveal)
;;(use-package ox-timeline);;


;;;;Python elpy
;;(use-package elpy
;;  :ensure t
;;  :defer t
;;  :init
;;  (advice-add 'python-mode :before 'elpy-enable))


;;Call custom file with bibliography packages and configuraion
;;(setq bibliography-file (concat user-emacs-directory "/bibliography.el"))
;;(load-file bibliography-file)

(put 'dired-find-alternate-file 'disabled nil)
