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

;; (use-package htmlize)
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

       (list "blog-index"
	     :recursive t
	     :base-directory "./content"
	     :publishing-function 'org-html-publish-to-html
	     :publishing-directory "./public"
	     :auto-sitemap t                ; Generate sitemap.org automagically...
	     :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
	     :sitemap-title "Posts"         ; ... with title 'Sitemap'.
	     ;; :sitemap-style list
	     :auto-preamble nil
	     :auto-postamble nil
	     )
       (list "blog:main"
	     :author "Zakaria Kebairia"
	     :email "4.kebairia@gmail.com"
	     :recursive t
	     :base-directory "./content"
	     :publishing-function 'org-html-publish-to-html
	     :publishing-directory "./public"
	     :with-author t
	     :with-creator t
	     :with-toc nil
	     :with-date t
	     :with-tags t
	     :section-numbers t
	     :time-stamp-file t
	     :auto-preamble nil
	     :auto-postamble nil
	     )
       (list "blog-static"
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
;; (setq org-html-postamble-format "\copy Zakaria Kebairia")
;; (setq org-export-html-postamble-format 
;;       '(("en" "<p class=\"postamble\">Last Updated %d. Created by %c</p>")))

(setq org-html-preamble "<p class=\"preamble\">
 %d - %a</p> ")
;; (setq org-html-postamble "<hr> <p class=\"postamble\">Last Updated %C. Created by  %a %d</p>")
(setq org-html-postamble "
<hr>
<p> 
<a href=\"https://www.kebairia.github.io/index.html\">Index</a>
&nbsp
<a href=\"mailto:4.kebairia@gmail.com\">email</a>
&nbsp
<a href=\"https://www.github.com/kebairia\">GitHub</a>
&nbsp
<a href=\"https://www.linkedin.com/in/zakaria-kebairia/\">LinkedIn</a>
&nbsp
<br>
&\copy Zakaria Kebairia
</p>"
)


(org-publish-all t)
(message "Build completed")
