;;-------------------------
;; straight.el bootstraping
;;-------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; use use-package along with straight
(straight-use-package 'use-package)
;; make `use-package` to automatically install all of your packages 
;; without the need for adding `:straight t`.
(setq straight-use-package-by-default t)

(setf org-html-preamble t)
(setf org-html-postamble nil)

;; htmlize package for syntax highlighting for the code blocks

(use-package htmlize)
;;-------
;; SITE
;;-------
(require 'ox-publish)
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\"href=\"css/main.css\" type=\"text/css\" />"
      )

      ;; org-html-head "<link rel=\"stylesheet\"href=\"https://cdn.simplecss.org/simple.min.css\" />"
;; href=\"https://cdn.simplecss.org/simple.min.css\" />")
(setq org-publish-project-alist
      (list
       (list "org-site:main"
	     :recursive t
	     :base-directory "./content"
	     :publishing-function 'org-html-publish-to-html
	     :publishing-directory "./public"
	     :with-author t
	     :with-creator t
	     :with-toc t
	     :section-numbers t
	     :time-stamp-file t
	     :auto-sitemap t                ; Generate sitemap.org automagically...
	     :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
	     :sitemap-title "Index"         ; ... with title 'Sitemap'.
	     ;; :sitemap-style list
	     :author "Zakaria Kebairia"
	     :auto-preamble t
	     :auto-preamble t
	     :email "4.kebairia@gmail.com"
	     )
       (list "org-static"
	     :recursive t
	     :base-directory "./content/"
	     :publishing-function 'org-publish-attachment
	     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|woff\\|otf"
	     :publishing-directory "./public/"
	     )
       ;; (list "org"
       ;; 	     :components ("org-site:main" "org-static"))
       ))
;; Generate the site output
(org-publish-all t)
(message "Build completed")
