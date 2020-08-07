(require 'ox-publish)

(message "default-directory %s" default-directory)

(setq org-export-html-postamble nil)
(setq org-publish-project-alist
      `(("docs"
         :base-directory ,(or (file-name-directory buffer-file-name) "doc-src")
         :publishing-directory ,(concat
                                 (or (file-name-directory buffer-file-name) "./")
                                 "../doc/")
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :with-creator nil
         :org-export-html-postamble nil
         )
        ("css"
         :base-directory ,(concat
                           (or (file-name-directory buffer-file-name) "./")
                           "../site-resourcs/css")

         :base-extension "css"
         :publishing-directory ,(concat
                                 (or (file-name-directory buffer-file-name) "./")
                                 "../doc/css/")

         :publishing-function org-publish-attachment
         :recursive t)
        ("all" :components ("docs" "css"))))
