;; Bibliographic references
;;(require 'ivy-bibtex)
(require 'org-ref)
(require 'bibtex-completion)

(setq ebib-bibtex-dialect 'biblatex)

(setq bibtex-completion-bibliography
      '("~/Labo/Papers_Database/JW_BibliographyVM.bib"))
(setq bibtex-completion-library-path '("~/Labo/Papers_Database/JW_PapersDB_VM/"))
(setq bibtex-completion-pdf-field "file")
;;(setq ivy-re-builders-alist
;;      '((ivy-bibtex . ivy--regex-ignore-order)
;;        (t . ivy--regex-plus)))
;;

(setq bibtex-completion-additional-search-fields '(abstract))

(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")


;;Open pdfs with umpdf
;;(setq bibtex-completion-pdf-open-function
;;  (lambda (fpath)
;;    (call-process "mupdf" nil 0 nil fpath)))

;; Ebib - Biblatex manager
(use-package ebib
  :config
  (setq ebib-file-name-mod-function 'my-ebib-file-name-transform)
  (setq ebib-filename-separator ";")
  (setq ebib-preload-bib-files '("~/Labo/Papers_Database/JW_BibliographyVM.bib"))
  (setq ebib-index-columns (quote
			    (("Year" 5 t)
			     ("Author/Editor" 40 nil)
			     ("Title" 80 t)
			     ("journaltitle" 30 nil)
			     ("Entry Key" 20 t))))
  (setq ebib-index-default-sort '("Year" . descend))  ; alternatively `ascend'
  (setq ebib-file-search-dirs '("~/Labo/Papers_Database/JW_PapersDB_VM/"))
  (setq ebib-file-associations '(("pdf" . "evince")))
  (defun my-ebib-file-name-transform (file direction)
    "Modify FILE for JabRef."
    (if direction
	;; Modify for storing FILE in the file field.
	(concat ":" file ":PDF")
      ;; Undo modifications so FILE can be passed to an external viewer.
      (let ((data (split-string file ":")))
	(if (= (length data) 1)
            (car data)
          (cl-second data))))))


;;Ebibi biblio support
(use-package biblio)

(require 'ebib-biblio)
(define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import)

;(use-package ebib-biblio
;  :after (ebib biblio)
;  :bind (:map biblio-selection-mode-map
;              ("e" . ebib-biblio-selection-import)))
