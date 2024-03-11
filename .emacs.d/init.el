(setq inhibit-startup-message t)  ; Inhibit startup-message
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)       ; Set up the visible bell

;(load-theme 'modus-vivendi)

(load-theme 'wombat)

;; Function copied from https://github.com/daviwil/
(defun set-font-faces ()
  (message "Setting faces.")
    (set-face-attribute 'default nil :font "Fira Code Retina" :height 110))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (set-font-faces))))
  (set-font-faces))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'norerror 'nomessage)

;;Revert buffers when the file changes in the disk
(global-auto-revert-mode 1)

;; Don't ask me what to do when opening symbolic links. Follow the link.
(setq vc-follow-symlinks 1)

;; Custom key bindings
(global-set-key (kbd "C-+") 'indent-region)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c e") 'mu4e)

;; Custom Functions
(defun jw/switch-to-buffer-other-window-keep-point (buffer)
  "Split window and open buffer but keep point."
  (interactive "b")
  (save-excursion (switch-to-buffer-other-window buffer)
		  (other-window 1)))

(global-set-key (kbd "C-x 4 n") 'jw/switch-to-buffer-other-window-keep-point)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(column-number-mode)
(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		pdf-view-mode-hook
		dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;Higlight current line with pulsar
(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-cyan)
  (setq pulsar-highlight-face 'pulsar-green)
  (pulsar-global-mode 1))

(global-set-key (kbd "C-c p") 'pulsar-pulse-line)
(global-set-key (kbd "C-c P") 'pulsar-highlight-line)


(setq set-mark-command-repeat-pop t)

;;Watch youtube videos in emacs with yeetube and mpv
(use-package yeetube
  :init (define-prefix-command 'my/yeetube-map)
  :config
  (setf yeetube-display-thumbnails nil) 
  :bind (("C-c y" . 'my/yeetube-map)
         :map my/yeetube-map
	 ("s" . 'yeetube-search)
	 ("b" . 'yeetube-play-saved-video)
	 ("d" . 'yeetube-download-videos)
	 ("p" . 'yeetube-mpv-toggle-pause)
	 ("v" . 'yeetube-mpv-toggle-video)
	 ("V" . 'yeetube-mpv-toggle-no-video-flag)
	 ("k" . 'yeetube-remove-saved-video)))


;;Avy configuration
(use-package avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-,") 'avy-goto-char-2)
(global-set-key (kbd "C-.") 'avy-goto-word-1)

;;Use pdf-tools for reading pdfs in emacs
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;(use-package company-box
;;  :ensure t
;;  :hook (company-mode . company-box-mode))


(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes 1
	doom-modeline-height 40))


;;Function to allow exteranl programs to acces emacs gpg files
(defun lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))


;;ChatGPT GPTel Chatgpt client for emacs
(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key (gptel-api-key-from-auth-source)))

;;Find synonyms
(use-package synonymous)

;;Email
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 20
  :config

  (require 'mu4e-org)

  (add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1)))

  ;;Avoid weird line breaks in text emails.
  (setq mu4e-compose-format-flowed t)

  (setq shr-color-visible-luminance-min 60)
  
  (setq message-kill-buffer-on-exit t) 
  (setq mu4e-view-show-images t)

  (setq message-send-mail-function 'smtpmail-send-it)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-context-policy 'pick-first)
  
  ;; Call the oauth2ms program to fetch the authentication token
  (defun fetch-access-token ()
    (with-temp-buffer
      (call-process "oauth2ms" nil t nil "--encode-xoauth2")
      (buffer-string)))

  ;; Functions to send emails with smpt and msoauth method (what a horrible thing to do!!!)
  ;; Add new authentication method for xoauth2
  (cl-defmethod smtpmail-try-auth-method
    (process (_mech (eql xoauth2)) user password)
    (let* ((access-token (fetch-access-token)))
      (smtpmail-command-or-throw
       process
       (concat "AUTH XOAUTH2 " access-token)
       235)))

  ;; Register the method
  (with-eval-after-load 'smtpmail
    (add-to-list 'smtpmail-auth-supported 'xoauth2))

  
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.Mail")
    
  (setq mu4e-contexts
	(list
	 ;;CRG - Workflows
	 (make-mu4e-context
	  :name "CRG"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "¨/CRG" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "jose.wojnacki@crg.eu")
		  (user-full-name . "José Wojnacki")
                  ;;(smtpmail-default-smtp-server . "smtp.office365.com")
		  (smtpmail-smtp-server  . "smtp.office365.com")
		  (smtpmail-smtp-service .  587)
		  (smtpmail-stream-type  . starttls)
		  (smtpmail-starttls-credentials . (("smtp.office365.com" 587 nil nil)))
		  ;;(smtpmail-debug-info . t)
 		  (mu4e-drafts-folder . "/CRG/Drafts")
		  (mu4e-sent-folder . "/CRG/Sent")
		  (mu4e-trash-folder .  "/CRG/Deleted Items")))

	 ;; Posteo account
	 (make-mu4e-context
	  :name "Posteo"
	  :match-func
	  (lambda (msg)
	    (when msg
	      (string-prefix-p "/Posteo" (mu4e-message-field msg :maildir))))
	  :vars '((user-mail-address . "jose.wo@posteo.net")
		  (user-full-name . "José Wojnacki")
		  (smtpmail-smtp-server . "posteo.de")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type . tls)
		  (mu4e-drafts-folder . "/Posteo/Drafts")
		  (mu4e-sent-folder . "/Posteo/Sent")
		  (mu4e-trash-folder .  "/Posteo/Trash")))))

  (setq mu4e-maildir-shortcuts
	'(("/Posteo/Inbox"  . ?p)
	  ("/Posteo/Sent"   . ?s)
	  ("/CRG/Inbox"  . ?c)
	  ("/CRG/Sent Items" . ?S)))

  (setq mu4e-bookmarks
	'(( :name "Unread messages"
	    :query "(maildir:/CRG/Inbox or maildir:/Posteo/Inbox) AND flag:unread AND NOT from:linkedin AND NOT from:glassdoor"
	    :key ?u)
	  ( :name  "Posteo messages"
	    :query "maildir:/Posteo/Inbox and NOT from:glassdoor"
	    :key ?p)
	  ( :name "CRG messages"
	    :query "maildir:/CRG/Inbox and NOT from:LinkedIn AND NOT from:glassdoor"
	    :key ?c)
	  ( :name "Today's messages"
	    :query "(maildir:/CRG/Inbox or maildir:/Posteo/Inbox) and date:today..now AND NOT from:linkedin AND NOT from:glassdoor"
	    :key ?t)
	  ( :name "CRG Last 7 days"
	    :query "maildir:/CRG/Inbox and date:7d..now AND NOT from:linkedin AND NOT from:glassdoor"
	    :hide-unread t
	    :key ?w)
	  ( :name "Jobs"
	    :query "(maildir:/CRG/Inbox or maildir:/Posteo/Inbox) AND from:linkedin OR from:glassdoor"
	    :key ?l)))

  (defun jw/capture-mail-follow-up (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ef"))

  (defun jw/capture-mail-read-later (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "er"))

  (defun jw/capture-mail-personalAgenda (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ep"))

  (defun jw/capture-mail-workAgenda (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ew"))  

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
	       '("follow up" . jw/capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions
	       '("follow up" . jw/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
	       '("read later" . jw/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions
	       '("read later" . jw/capture-mail-read-later) t)
  (add-to-list 'mu4e-headers-actions
	       '("personalAgenda" . jw/capture-mail-personalAgenda) t)
  (add-to-list 'mu4e-view-actions
	       '("personalAgenda" . jw/capture-mail-personalAgenda) t)
  (add-to-list 'mu4e-headers-actions
	       '("workAgenda" . jw/capture-mail-workAgenda) t)
  (add-to-list 'mu4e-view-actions
	       '("workAgenda" . jw/capture-mail-workAgenda) t)


  (mu4e t))


;;Write emails in html
(use-package org-mime
  :ensure t)



;;Dired configuration
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config (setq dired-kill-when-opening-new-dired-buffer 1
		dired-dwim-target 1
		delete-by-moving-to-trash 1
		dired-listing-switches "-lgh --group-directories-first"
		dired-omit-files "\\.[^.].*")
  :hook (diredfl-mode))

(setq find-name-arg "-iname")


(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ;; As resuming is expected to be one of the most commonly used
   ;; commands, this binding is one of the easiest to press.
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ;; This binding mirrors, e.g. "C-x t RET".
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))


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

(use-package vertico-quick
  :after vertico
  :ensure nil)

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

(require 'move-text)
(move-text-default-bindings)

(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
	(indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)


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

;;Global key bindings to wrap around selected text with parenthesis, brackets or curly brackets.
(global-set-key (kbd "C-x w [") 'sp-wrap-square)
(global-set-key (kbd "C-x w {") 'sp-wrap-curly)
(global-set-key (kbd "C-x w 8") 'sp-wrap-round)
(global-set-key (kbd "C-x w w") 'sp-rewrap-sexp)
(global-set-key (kbd "C-x w u") 'sp-unwrap-sexp)

(use-package expand-region
  :ensure t
  :bind ("C-M-+"  . er/expand-region))

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

;;Color-code parenthesis acording to their depth
(use-package rainbow-delimiters
  :ensure t)



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
	org-special-ctrl-a/e t
	org-agenda-files
	'("~/OrgFiles/Tasks.org"
	  "~/OrgFiles/Agenda.org"
	  "~/OrgFiles/Agendo.org"
	  "~/OrgFiles/Mail.org"
	  ;;"~/OrgFiles/Habits.org"
	  ;;"~/OrgFiles/TrainingForClimbing.org"
  	  "~/OrgFiles/Workout.org"
	  "~/OrgFiles/TimeBlock.org"))
  (setq calendar-holidays
	'(
	  ;; State holidays
	  (holiday-fixed 1 1 "Fiesta Nacional: New Year")
	  (holiday-fixed 1 6 "Fiesta Nacional: Dia de Reyes/Epifania")
	  (holiday-fixed 5 1 "Fiesta Nacional: International Labor Day")
	  (holiday-fixed 5 24 "Fiesta Nacional: Segunda Pascua")
	  (holiday-fixed 6 24 "Fiesta Nacional: San Juan")
	  (holiday-fixed 8 15 "Fiesta Nacional: Asuncion de la Virgen")
	  (holiday-fixed 10 12 "Fiesta Nacional: Dia de la Hispanidad")
	  (holiday-fixed 11 1 "Fiesta Nacional: Todos los Santos")
	  (holiday-fixed 12 6 "Fiesta Nacional: Dia de la Constitucion")
	  (holiday-fixed 12 8 "Fiesta Nacional: Inmaculada Concepcion")
	  (holiday-fixed 12 25 "Fiesta Nacional: Catholic Christmas")
	  ;; floated holidays       
	  (holiday-easter-etc  -2 "Fiesta Nacional: Viernes Santo")
	  (holiday-easter-etc  0 "Domingo de Ramos")
	  (holiday-easter-etc  1 "Fiesta Local: Lunes de Pascua Florida")
	  (holiday-easter-etc 50 "Fiesta Local: Lunes de Pascua granada:Lunes de pentecostes")
	  ;; Cataluyna holidays
	  (holiday-fixed 9 11 "Fiesta Local: Diada de Catalunya")
	  (holiday-fixed 9 24 "Fiesta Local: La Merced Barcelona")
	  (holiday-fixed 12 26 "Fiesta Local: San Esteban")))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "FOLLOW(f)" "|" "CANCELLED(c)" "DONE(d)")))
  (setq org-agenda-custom-commands
	'(("d" "Today's agenda."
	   ((tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "Important tasks\n")))
 	    (agenda  "" ((org-agenda-span 1) (org-agenda-overriding-header "\nToday's agenda\n")
 			 (org-agenda-time-grid   '((daily today require-timed)
						   (800 1000 1200 1400 1600 1800 2000 2200)
						   "......"
						   "----------------"))
			 (org-deadline-warning-days 0)
			 (org-deadline-past-days 0)
			 (org-scheduled-past-days 0)
			 (org-agenda-day-face-function (lambda (date) '(org-agenda-date bold)))
			 (org-agenda-format-date "%A %-e de %B del %Y")))
	    (tags-todo "+PRIORITY=\"B\"" ((org-agenda-overriding-header "\nOther tasks\n")))))
	  ("w" "And the week?"
	   ((tags-todo "+PRIORITY=\"A\"|+PRIORITY=\"B\"" ((org-agenda-overriding-header "Tasks")))
	    (agenda  "" ((org-agenda-overriding-header "\nAgenda for the next ten days\n")
			 (org-agenda-span 10)
			 (org-agenda-time-grid nil)
			 (org-deadline-warning-days 0)
			 (org-deadline-past-days 0)
			 (org-agenda-include-diary t)))))))
  (setq org-capture-templates
	'(("t" "Tasks")
	  ("tp" "Task" entry (file+olp "~/OrgFiles/Tasks.org" "Personal")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	  ("tw" "Task" entry (file+olp "~/OrgFiles/Tasks.org" "Work")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)	  
	  ("m" "Metrics Capture")
	  ("mp" "Blood pressure" table-line (file+headline "~/OrgFiles/Metrics.org" "Pressure")
	   "| %U | %^{High} | %^{Low} | %^{Heart rate} | %^{Notes} |" :kill-buffer t)
	  ("mr" "Running" table-line (file+headline "~/OrgFiles/Metrics.org" "Running")
	   "| %U | %^{Distance} | %^{Time} | %^{Pace} | %^{Notes} |" :kill-buffer t)
	  ("n" "Notes")
	  ("np" "Como estuvo el día?" table-line (file "~/OrgFiles/ElDia.org")
	   "| %U | %^{General} | %^{Notes} |" :kill-buffer t)
	  ("e" "Email Workflow")
	  ("ef" "Follow Up" entry (file+olp "~/OrgFiles/Mail.org" "Follow Up")
           "* TODO Follow up with %:fromname on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+3d\"))\n\n%i" :immediate-finish t)
	  ("er" "Read Later" entry (file+olp "~/OrgFiles/Mail.org" "Read Later")
           "* TODO Read %a from %:fromname\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+5d\"))\n%i" :immediate-finish t)
	  ("ep" "Agenda Personal" entry (file+olp "~/OrgFiles/Agenda.org" "Personal")
           "* %?%:subject %(org-set-tags \"personal\")\n%^t\n\n%i\ne-mail: %a")
	  ("ew" "Agenda Work" entry (file+olp "~/OrgFiles/Agenda.org" "Work")
           "* %?%:subject %(org-set-tags \"work\")\n%^t\n\n%i\ne-mail: %a"))))


(setq org-timeblock-span 7)


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
  :custom
  (org-roam-directory "~/OrgFiles/RoamNotes/")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  :bind (("C-c n l"   . org-roam-buffer-toggle)
         ("C-c n f"   . org-roam-node-find)
	 ("C-c n i"   . org-roam-node-insert)
         ("C-c n d"   . org-roam-dailies-find-date)
         ("C-c n c"   . org-roam-dailies-capture-today)
         ("C-c n C r" . org-roam-dailies-capture-tomorrow)
         ("C-c n t"   . org-roam-dailies-find-today)
         ("C-c n y"   . org-roam-dailies-find-yesterday)
         ("C-c n r"   . org-roam-dailies-find-tomorrow)
         ("C-c n g"   . org-roam-graph))
  :config
  (org-roam-setup))

;;Call custom file with bibliography packages and configuraion
(setq bibliography-file (concat user-emacs-directory "bibliography.el"))
(load-file bibliography-file)

;(use-package pubmed
;  :ensure t
;  :requires (pubmed-unpaywall pubmed-springer pubmed-dissemin pubmed-scihub)
;  :commands (pubmed-search pubmed-advanced-search pubmed-save-as)
; :config (setq  pubmed-api-key "38d4a4ce6747c1739bde2131977303e68208"
;		 pubmed-unpaywall-email "jose.wo@posteo.net"
    ;		 pubmed-springer-api-key "de01e8e321cb8812995b54d180ed2cec"
;		 pubmed-scihub-url "https://sci-hub.st/"
;		 pubmed-fulltext-functions '(pubmed-pmc pubmed-unpaywall pubmed-scihub pubmed-dissemin pubmed-openaccessbutton)))



;;:hook
;;;  (after-init . org-roam-mode)

;;;  (org-roam-capture-templates
;;;   '(("d" "default" plain
;;;      #'org-roam-capture--get-point
;;;      "%?"
;;;      :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;;      :head "#+title: ${title}\n"
;;;      :unnarrowed t)
;;;     ("ll" "link note" plain
;;;      #'org-roam-capture--get-point
;;;      "* %^{Link}"
;;;      :file-name "Inbox"
;;;      :olp ("Links")
;;;      :unnarrowed t
;;;      :immediate-finish)
;;;     ("lt" "link task" entry
;;;      #'org-roam-capture--get-point
;;;      "* TODO %^{Link}"
;;;      :file-name "Inbox"
;;;      :olp ("Tasks")
;;;      :unnarrowed t
;;;      :immediate-finish)))
;;;  (org-roam-dailies-directory "Journal/")
;;;  (org-roam-dailies-capture-templates
;;;   '(("d" "default" entry
;;;      #'org-roam-capture--get-point
;;;      "* %?"
;;;      :file-name "Journal/%<%Y-%m-%d>"
;;;       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
;;;      ("t" "Task" entry
;;;       #'org-roam-capture--get-point
;;;       "* TODO %?\n  %U\n  %a\n  %i"
;;;       :file-name "Journal/%<%Y-%m-%d>"
;;;       :olp ("Tasks")
;;;       :empty-lines 1
;;;       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
;;;      ("j" "journal" entry
;;;       #'org-roam-capture--get-point
;;;       "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
;;;       :file-name "Journal/%<%Y-%m-%d>"
;;;       :olp ("Log")
;;;       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
;;;      ("l" "log entry" entry
;;;       #'org-roam-capture--get-point
;;;       "* %<%I:%M %p> - %?"
;;;       :file-name "Journal/%<%Y-%m-%d>"
;;;       :olp ("Log")
;;;       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
;;;      ("m" "meeting" entry
;;;       #'org-roam-capture--get-point
;;;       "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
;;;       :file-name "Journal/%<%Y-%m-%d>"
;;;       :olp ("Log")
;;;       :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
;;;  
;;;  :map org-mode-map
;;;  (("C-c n i" . org-roam-insert))
;;;  (("C-c n I" . org-roam-insert-immediate)))
;;;
;;:config
;;(org-roam-setup))



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


(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
