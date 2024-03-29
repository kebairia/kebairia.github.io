(setq project-switch-commands t)

(setq package-enable-at-startup nil)
;;-------------------------
;; straight.el bootstraping
;;-------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use use-package along with straight
(straight-use-package 'use-package)
;; make `use-package` to automatically install all of your packages 
;; without the need for adding `:straight t`.
(setq straight-use-package-by-default t)


;; Install dependencies
(use-package htmlize)

;; Load the publishing system
(require 'ox-publish)
;; ####################################
(defun zk/org-publish-org-sitemap-format-entry (entry style project)
  (cond ((not (directory-name-p entry))
         (let* ((date (org-publish-find-date entry project)))

           (format "%s - [[file:%s][%s]]"
                   (format-time-string "%Y-%m-%d" date ) entry
                   (org-publish-find-title entry project))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\"href=\"css/main.css\" type=\"text/css\" />"
      )

(setq org-publish-project-alist
      (list

       (list "blog-index"
	           :recursive t
	           :base-directory "./content/"
	           :publishing-function 'org-html-publish-to-html
	           :publishing-directory "./public"
	           :auto-sitemap  t 
	           :sitemap-filename "index.org"  
	           :sitemap-title "Posts"         
             :sitemap-sort-files   'anti-chronologically
             :sitemap-format-entry 'zk/org-publish-org-sitemap-format-entry
	           :auto-preamble nil
	           :auto-postamble nil
	           )
       (list "blog-main"
	           :author "Zakaria Kebairia"
	           :email "4.kebairia@gmail.com"
	           :recursive t
	           :base-directory "./content"
	           :publishing-function 'org-html-publish-to-html
	           :publishing-directory "./public/"
	           :with-author t
	           :with-creator t
	           :with-toc nil
	           :with-date t
	           :with-tags t
	           :with-latex t
	           :html-head-extra 
	           "<header> <h4> <a href=\"https://kebairia.github.io\" title=\"Home\" class=\"home\">
	          <img src=\"img/home.svg\" width=\"50\" alt=\"Home\" />
	          </a>

	          <a href=\"files/feed.rss\" title=\"RSS Feed\" type=\"application/rss+xml\" class=\"rss\">
	          <img src=\"img/rss.svg\" alt=\"RSS icon\" />

	          </a>
            <a href=\"files/cv.pdf\" title=\"My Resume\" type=\"application/pdf\" class=\"resume\">
            <img src=\"img/cv.svg\" width=\"40\"  alt=\"My CV icon\" /> </a>

            </h4>
	          </header>"
	           :section-numbers t
	           :time-stamp-file t
	           :auto-preamble nil
	           :auto-postamble nil
	           )
       (list "blog-static"
	           :recursive t
	           :base-directory "./content/"
	           :publishing-function 'org-publish-attachment
	           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|woff\\|otf\\|svg\\|rss\\|xml"
	           :publishing-directory "./public/"
	           )

       ;; (list "org"
       ;;       :components ("blog-index""blog-main" "blog-static"))
       ))

(setq org-html-postamble "
<p class=\"postamble\"> 

<a href=\"http://creativecommons.org/licenses/by-sa/4.0/\" class=\"crc\">
<img src=\"img/crc.png\" alt=\"Creative Commons License\" title=\"Creative Commons License\" />

<a href=\"https://www.linkedin.com/in/zakaria-kebairia/\" title=\"LinkedIn account\" class=\"social\">
<img src=\"img/social/linkedin-icon-logo.svg\" width=\"45\" alt=\"LinkedIn Account\" />
</a>

<a href=\"https://twitter.com/z_kebairia\" title=\"Twitter Account\" class=\"social\">
<img src=\"img/social/twitter-logo.svg\" width=\"40\" alt=\"Twitter Account\" /> </a>

<a href=\"https://www.youtube.com/channel/UC7OqXJDFQI8_WFC6WnsWCrg\" title=\"Youtube Account\" class=\"social\">
<img src=\"img/social/youtube-black-logo.svg\" width=\"40\" alt=\"Youtube Account\" /> </a>

<a href=\"https://www.github.com/kebairia\" title=\"GitHub Account\" class=\"social\">
<img src=\"img/social/github.svg\" width=\"43\" alt=\"GitHub Account\" /> </a>

<br>

<p class=\"credit\">
Copyright &\copy  2023 Zakaria Kebairia
<br>
Content licensed <a href=\"http://creativecommons.org/licenses/by-sa/4.0/\">CC-BY-SA 4.0</a> unless otherwise noted.

</p>
"
)

;; Generate the site output
(org-publish-all t)

(message "Build complete!")
