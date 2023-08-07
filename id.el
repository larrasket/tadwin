(require 'ox-tufte)
(require 'org)
(require 'ox-publish)
(require 'org-macs)
(require 'cl-lib)
(require 'format-spec)
(require 'ox)
(require 'ox-publish)
(require 'table)

(defun org-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
         (dot (when (> (length html-ext) 0) "."))
         (link-org-files-as-html-maybe
            (lambda (raw-path info)
    	    ;; Treat links to `file.org' as links to `file.html', if
    	    ;; needed.  See `org-html-link-org-files-as-html'.
                    (save-match-data
                     (cond
                        ((and (plist-get info :html-link-org-files-as-html)
                              (let ((case-fold-search t))
                                (string-match "\\(.+\\)\\.org\\(?:\\.gpg\\)?$" raw-path)))
                         (concat (match-string 1 raw-path) dot html-ext))
                        (t raw-path)))))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
    	 ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
            (cond
               ((member type '("http" "https" "ftp" "mailto" "news"))
                (url-encode-url (concat type ":" raw-path)))
               ((string= "file" type)
    	    ;; During publishing, turn absolute file names belonging
    	    ;; to base directory into relative file names.  Otherwise,
    	    ;; append "file" protocol to absolute file name.
                (setq raw-path
                  (org-export-file-uri
                       (org-publish-file-relative-name raw-path info)))
    	    ;; Possibly append `:html-link-home' to relative file
    	    ;; name.
                (let ((home (and (plist-get info :html-link-home)
                             (org-trim (plist-get info :html-link-home)))))
                    (when (and home
                           (plist-get info :html-link-use-abs-url)
                           (file-name-absolute-p raw-path))
                         		(setq raw-path (concat (file-name-as-directory home) raw-path))))
    	    ;; Maybe turn ".org" into ".html".
                (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
    	    ;; Add search option, if any.  A search option can be
    	    ;; relative to a custom-id, a headline title, a name or
    	    ;; a target.
                (let ((option (org-element-property :search-option link)))
                    (if (not option) raw-path
                         		(let ((path (org-element-property :path link)))
                              (concat raw-path
                                    "#"
                                    (org-publish-resolve-external-link option path t))))))
               (t raw-path)))
         (attributes-plist
            (org-combine-plists
    	   ;; Extract attributes from parent's paragraph.  HACK: Only
    	   ;; do this for the first link in parent (inner image link
    	   ;; for inline images).  This is needed as long as
    	   ;; attributes cannot be set on a per link basis.
               (let* ((parent (org-export-get-parent-element link))
                      (link (let ((container (org-export-get-parent link)))
                             (if (and (eq 'link (org-element-type container))
                                      (org-html-inline-image-p link info))
                                 container
                                 link))))
                   (and (eq link (org-element-map parent 'link #'identity info t))
                    (org-export-read-attribute :attr_html parent)))
    	   ;; Also add attributes from link itself.  Currently, those
    	   ;; need to be added programmatically before `org-html-link'
    	   ;; is invoked, for example, by backends building upon HTML
    	   ;; export.
               (org-export-read-attribute :attr_html link)))
         (attributes
            (let ((attr (org-html--make-attribute-string attributes-plist)))
                (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
       (org-export-inline-image-p
          link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
         	(if (not destination) desc
             (format "<a href=\"#%s\"%s>%s</a>"
                 (org-export-get-reference destination info)
                 attributes
                 desc))))
                ;; Links pointing to a headline: Find destination and build
                ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                          (org-export-resolve-fuzzy-link link info)
                          (org-export-resolve-id-link link info))))
         	(pcase (org-element-type destination)
    	  ;; ID link points to an external file.
             (`plain-text
                (let ((fragment (concat "" path))
        		 ;; Treat links to ".org" files as ".html", if needed.
                      (path (funcall link-org-files-as-html-maybe
                                     				destination info)))
                    (format "<a href=\"%s#%s\"%s>%s</a>"
                        path fragment attributes (or desc destination))))
    	  ;; Fuzzy link points nowhere.
             (`nil
                (format "<i>%s</i>"
                    (or desc
                            (org-export-data
                                   			(org-element-property :raw-link link) info))))
    	  ;; Link points to a headline.
             (`headline
                (let ((href (org-html--reference destination info))
        		 ;; What description to use?
                      (desc
        		  ;; Case 1: Headline is numbered and LINK has no
        		  ;; description.  Display section number.
                           (if (and (org-export-numbered-headline-p destination info)
                                  (not desc))
                               (mapconcat #'number-to-string
                                  (org-export-get-headline-number
                                           destination info) ".")
                 ;; Case 2: Either the headline is un-numbered or
                 ;; LINK has a custom description.  Display LINK's
                 ;; description or headline's title.
                               (or desc
                                       			(org-export-data
                                              (org-element-property :title destination) info)))))
                    (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
    	  ;; Fuzzy link points to a target or an element.
           (_
                    (if (and destination
                             (memq (plist-get info :with-latex) '(mathjax t))
                             (eq 'latex-environment (org-element-type destination))
                             (eq 'math (org-latex--environment-type destination)))
                        ;; Caption and labels are introduced within LaTeX
                ;; environment.  Use "ref" or "eqref" macro, depending on user
                        ;; preference to refer to those in the document.
                        (format (plist-get info :html-equation-reference-format)
                                (org-html--reference destination info))
                      (let* ((ref (org-html--reference destination info))
                             (org-html-standalone-image-predicate
                              #'org-html--has-caption-p)
                             (counter-predicate
                              (if (eq 'latex-environment (org-element-type destination))
                                  #'org-html--math-environment-p
                                #'org-html--has-caption-p))
                             (number
                              (cond
                                   (desc nil)
                                   ((org-html-standalone-image-p destination info)
                                    (org-export-get-ordinal
                                           			(org-element-map destination 'link #'identity info t)
                                           			info '(link) 'org-html-standalone-image-p))
                                   (t (org-export-get-ordinal
                                       destination info nil counter-predicate))))
                             (desc
                              (cond (desc)
                                  ((not number) "No description for this link")
                                  ((numberp number) (number-to-string number))
                                  (t (mapconcat #'number-to-string number ".")))))
                        (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc)))))))
              ;; Coderef: replace link with the reference name or the
              ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
         	(format "<a href=\"#%s\" %s%s>%s</a>"
                 		fragment
                 		(format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
                       			fragment fragment)
                 		attributes
                 		(format (org-export-get-coderef-format path desc)
                       			(org-export-resolve-coderef path info)))))
                            ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
          (org-html-encode-plain-text path)
          attributes
          desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
         	(format "<a href=\"%s\"%s>%s</a>" path attributes path)))
              ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))










(defun org-html--reference (datum info &optional named-only)
  (let* ((type (org-element-type datum))
         (user-label
          (org-element-property
           (pcase type
             ((or `headline `inlinetask)
              :CUSTOM_ID)
             ((or `radio-target `target)
              :value)
             (_ :name))
           datum))
         (user-label (or user-label
                         (when-let ((path (org-element-property :ID datum)))
                           (concat "" path)))))
    (cond
     ((and user-label
           (or (plist-get info :html-prefer-user-labels)
               (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
           (not (memq type '(headline inlinetask radio-target target)))
           (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))




(setq org-export-global-macros
      '(("comments" . "(eval (salih/print-text-nodes))")
        ("b" . "(eval (format \"#+begin_export html\n%s\n#+end_export\" (salih/print-back-links)))")
        ("t" . "(eval (concat \"This section was labeled under\"))")
        ("s" . "(eval (concat \"Part of a series on\"))")))


(defun org-export--annotate-info (backend info &optional subtreep visible-only ext-plist)
  (let ((parsed-keywords
         (delq nil
               (mapcar (lambda (o) (and (eq (nth 4 o) 'parse) (nth 1 o)))
                       (append (org-export-get-all-options backend)
                               org-export-options-alist))))
        tree modified-tick)
    (run-hook-with-args 'org-export-before-processing-hook
                        (org-export-backend-name backend))
    (org-export-expand-include-keyword)
    (org-export--delete-comment-trees)
    (org-macro-initialize-templates org-export-global-macros)
    (org-macro-replace-all org-macro-templates parsed-keywords)


    (org-export-expand-include-keyword)
    (org-export--delete-comment-trees)
    (org-macro-initialize-templates org-export-global-macros)
    (org-macro-replace-all org-macro-templates parsed-keywords)
    ;; Refresh buffer properties and radio targets after previous
    ;; potentially invasive changes.
    (org-set-regexps-and-options)
    (org-update-radio-target-regexp)
    (setq modified-tick (buffer-chars-modified-tick))
    (when org-export-use-babel
      (org-babel-exp-process-buffer)
      (org-macro-replace-all '(("results" . "$1")) parsed-keywords)
      (unless (eq modified-tick (buffer-chars-modified-tick))
        (org-set-regexps-and-options)
        (org-update-radio-target-regexp))
      (setq modified-tick (buffer-chars-modified-tick)))
    (goto-char (point-min))
    (save-excursion
      (run-hook-with-args 'org-export-before-parsing-hook
                          (org-export-backend-name backend)))
    (unless (eq modified-tick (buffer-chars-modified-tick))
      (org-set-regexps-and-options)
      (org-update-radio-target-regexp))
    (setq modified-tick (buffer-chars-modified-tick))
    (setq info
          (org-combine-plists
           info (org-export-get-environment backend subtreep ext-plist)))
    (org-cite-store-bibliography info)
    (org-cite-store-export-processor info)
    (dolist (entry (append (org-export-get-all-options backend)
                           org-export-options-alist))
      (pcase entry
        (`(,p ,_ ,_ ,_ parse)
         (let ((value (plist-get info p)))
           (plist-put info
                      p
                      (org-export--remove-uninterpreted-data value info))))
        (_ nil)))
    (setq info (org-export-install-filters info))
    (let ((backend-name (org-export-backend-name backend)))
      (dolist (filter (plist-get info :filter-options))
        (let ((result (funcall filter info backend-name)))
          (when result (setq info result)))))
    (setq tree (org-element-parse-buffer nil visible-only 'defer))
    (org-export--prune-tree tree info)
    (org-export--remove-uninterpreted-data tree info)
    (setq tree
          (org-export-filter-apply-functions
           (plist-get info :filter-parse-tree) tree info))
    (setq info (org-export--collect-tree-properties tree info))
    (org-cite-process-citations info)
    (org-cite-process-bibliography info)
    info))





(setq org-id-link-to-org-use-id t)
