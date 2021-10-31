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

;; htmlize package for syntax highlighting for the code blocks

(use-package htmlize)
;;-------
;; SITE
;;-------
(require 'ox-publish)
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\"
href=\"https://cdn.simplecss.org/simple.min.css\" />")
(setq org-publish-project-alist
      (list
       (list "org-site:main"
	     :recursive t
	     :base-directory "./content"
	     :publishing-function 'org-html-publish-to-html
	     :publishing-directory "./public"
	     :with-author nil
	     :with-creator t
	     :with-toc t
	     :section-numbers nil
	     :time-stamp-file nil)))
;; Generate the site output
(org-publish-all t)
(message "Build completed")
