;; Disable automatic package initialization
(setq package-enable-at-startup nil)

;; -------------------------
;; Bootstrap straight.el
;; -------------------------
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use `use-package` with `straight.el`
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Install dependencies
(use-package htmlize)

;; Load org-publish
(require 'ox-publish)

;; -------------------------
;; Sitemap Formatting
;; -------------------------
(defun zk/org-publish-org-sitemap-format-entry (entry style project)
  "Custom format for org-publish sitemap entries."
  (cond ((not (directory-name-p entry))
         (let ((date (org-publish-find-date entry project)))
           (format "%s - [[file:%s][%s]]"
                   (format-time-string "%Y-%m-%d" date) entry
                   (org-publish-find-title entry project))))
        ((eq style 'tree)
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

;; -------------------------
;; HTML Export Settings
;; -------------------------
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"css/main.css\" type=\"text/css\" />")

;; -------------------------
;; Org-Publish Configuration
;; -------------------------
(setq org-publish-project-alist
      (list
       ;; Index page
       (list "blog-index"
             :recursive t
             :base-directory "./content/"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./public"
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title "Posts"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-format-entry 'zk/org-publish-org-sitemap-format-entry
             :auto-preamble nil
             :auto-postamble nil)

       ;; Main blog pages
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
             "<header>
                <h4>
                  <a href=\"https://zakariakebairia.com\" class=\"home\">
                    <img src=\"img/home.svg\" width=\"50\" alt=\"Home\" />
                  </a>
                  <a href=\"files/feed.rss\" class=\"rss\">
                    <img src=\"img/rss.svg\" alt=\"RSS Feed\" />
                  </a>
                  <a href=\"files/cv.pdf\" class=\"resume\">
                    <img src=\"img/cv.svg\" width=\"40\" alt=\"My CV\" />
                  </a>
                </h4>
              </header>"
             :section-numbers t
             :time-stamp-file t
             :auto-preamble nil
             :auto-postamble nil)

       ;; Static assets (CSS, JS, images, etc.)
       (list "blog-static"
             :recursive t
             :base-directory "./content/"
             :publishing-function 'org-publish-attachment
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|woff\\|otf\\|svg\\|rss\\|xml"
             :publishing-directory "./public/")))

;; -------------------------
;; Footer with Dynamic Year
;; -------------------------
(setq org-html-postamble
      (format "
<p class=\"postamble\">
  <a href=\"http://creativecommons.org/licenses/by-sa/4.0/\" class=\"crc\">
    <img src=\"img/crc.png\" alt=\"Creative Commons License\" />
  </a>

  <a href=\"https://www.linkedin.com/in/zakaria-kebairia/\" class=\"social\">
    <img src=\"img/social/linkedin-icon-logo.svg\" width=\"45\" alt=\"LinkedIn\" />
  </a>

  <a href=\"https://twitter.com/z_kebairia\" class=\"social\">
    <img src=\"img/social/twitter-logo.svg\" width=\"40\" alt=\"Twitter\" />
  </a>

  <a href= \"https://www.youtube.com/@zakariakebairia\" class=\"social\">
    <img src=\"img/social/youtube-black-logo.svg\" width=\"40\" alt=\"YouTube\" />
  </a>

  <a href=\"https://www.github.com/zakariakebairia\" class=\"social\">
    <img src=\"img/social/github.svg\" width=\"43\" alt=\"GitHub\" />
  </a>

  <br>

  <p class=\"credit\">
    Copyright &copy; %s Zakaria Kebairia
    <br>
    Content licensed under <a href=\"http://creativecommons.org/licenses/by-sa/4.0/\">CC-BY-SA 4.0</a> unless otherwise noted.
  </p>
</p>
" (format-time-string "%Y"))) 
;; -------------------------
;; Build the Site
;; -------------------------
(org-publish-all t)
(message "Build complete!")

