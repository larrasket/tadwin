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






(defun org-export--annotate-info (backend info &optional subtreep visible-only ext-plist)
  "Annotate the INFO plist according to the BACKEND.

This is run in the context of the current buffer.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't process the
contents of hidden elements.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings."
  (let ((parsed-keywords
         (delq nil
               (mapcar (lambda (o) (and (eq (nth 4 o) 'parse) (nth 1 o)))
                       (append (org-export-get-all-options backend)
                               org-export-options-alist))))
        tree modified-tick)

    ;; Run first hook with current back-end's name as argument.
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
    ;;  Possibly execute Babel code.  Re-run a macro expansion
    ;;  specifically for {{{results}}} since inline source blocks
    ;;  may have generated some more.  Refresh buffer properties
    ;;  and radio targets another time.
    (when org-export-use-babel
      (org-babel-exp-process-buffer)
      (org-macro-replace-all '(("results" . "$1")) parsed-keywords)
      (unless (eq modified-tick (buffer-chars-modified-tick))
        (org-set-regexps-and-options)
        (org-update-radio-target-regexp))
      (setq modified-tick (buffer-chars-modified-tick)))
    ;; Run last hook with current back-end's name as argument.
    ;; Update buffer properties and radio targets one last time
    ;; before parsing.
    (goto-char (point-min))
    (save-excursion
      (run-hook-with-args 'org-export-before-parsing-hook
                          (org-export-backend-name backend)))
    (unless (eq modified-tick (buffer-chars-modified-tick))
      (org-set-regexps-and-options)
      (org-update-radio-target-regexp))
    (setq modified-tick (buffer-chars-modified-tick))
    ;; Update communication channel with environment.
    (setq info
          (org-combine-plists
           info (org-export-get-environment backend subtreep ext-plist)))
    ;; Pre-process citations environment, i.e. install
    ;; bibliography list, and citation processor in INFO.
    (org-cite-store-bibliography info)
    (org-cite-store-export-processor info)
    ;; De-activate uninterpreted data from parsed keywords.
    (dolist (entry (append (org-export-get-all-options backend)
                           org-export-options-alist))
      (pcase entry
        (`(,p ,_ ,_ ,_ parse)
         (let ((value (plist-get info p)))
           (plist-put info
                      p
                      (org-export--remove-uninterpreted-data value info))))
        (_ nil)))
    ;; Install user's and developer's filters.
    (setq info (org-export-install-filters info))
    ;; Call options filters and update export options.  We do not
    ;; use `org-export-filter-apply-functions' here since the
    ;; arity of such filters is different.
    (let ((backend-name (org-export-backend-name backend)))
      (dolist (filter (plist-get info :filter-options))
        (let ((result (funcall filter info backend-name)))
          (when result (setq info result)))))
    ;; Parse buffer.
    (setq tree (org-element-parse-buffer nil visible-only))
    ;; Prune tree from non-exported elements and transform
    ;; uninterpreted elements or objects in both parse tree and
    ;; communication channel.
    (org-export--prune-tree tree info)
    (org-export--remove-uninterpreted-data tree info)
    ;; Call parse tree filters.
    (setq tree
          (org-export-filter-apply-functions
           (plist-get info :filter-parse-tree) tree info))
    ;; Now tree is complete, compute its properties and add them
    ;; to communication channel.  This is responsible for setting
    ;; :parse-tree to TREE.
    (setq info (org-export--collect-tree-properties tree info))
    ;; Process citations and bibliography.  Replace each citation
    ;; and "print_bibliography" keyword in the parse tree with
    ;; the output of the selected citation export processor.
    (org-cite-process-citations info)
    (org-cite-process-bibliography info)
    info))

(setq org-id-link-to-org-use-id t)




(defun org-export-as
    (backend &optional subtreep visible-only body-only ext-plist)
  "Transcode current Org buffer into BACKEND code.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Return code as a string."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (org-export-barf-if-invalid-backend backend)
  (org-fold-core-ignore-modifications
    (save-excursion
      (save-restriction
        ;; Narrow buffer to an appropriate region or subtree for
        ;; parsing.  If parsing subtree, be sure to remove main
        ;; headline, planning data and property drawer.
        (cond ((org-region-active-p)
	       (narrow-to-region (region-beginning) (region-end)))
	      (subtreep
	       (org-narrow-to-subtree)
	       (goto-char (point-min))
	       (org-end-of-meta-data)
               ;; Make the region include top heading in the subtree.
               ;; This way, we will be able to retrieve its export
               ;; options when calling
               ;; `org-export--get-subtree-options'.
               (when (bolp) (backward-char))
	       (narrow-to-region (point) (point-max))))
        ;; Initialize communication channel with original buffer
        ;; attributes, unavailable in its copy.
        (let* ((org-export-current-backend (org-export-backend-name backend))
	       (info (org-combine-plists
		      (org-export--get-export-attributes
		       backend subtreep visible-only body-only)
		      (org-export--get-buffer-attributes)))
	       (parsed-keywords
	        (delq nil
		      (mapcar (lambda (o) (and (eq (nth 4 o) 'parse) (nth 1 o)))
			      (append (org-export-get-all-options backend)
				      org-export-options-alist))))
	       tree modified-tick)
	  ;; Update communication channel and get parse tree.  Buffer
	  ;; isn't parsed directly.  Instead, all buffer modifications
	  ;; and consequent parsing are undertaken in a temporary copy.
	  (org-export-with-buffer-copy
           (font-lock-mode -1)
	   ;; Run first hook with current back-end's name as argument.
	   (run-hook-with-args 'org-export-before-processing-hook
			       (org-export-backend-name backend))
	   (org-export-expand-include-keyword)
	   (org-export--delete-comment-trees)
	   (org-macro-initialize-templates org-export-global-macros)
	   (org-macro-replace-all org-macro-templates parsed-keywords)

           ;; duplicate
           	   (org-export-expand-include-keyword)
	   (org-export--delete-comment-trees)
	   (org-macro-initialize-templates org-export-global-macros)
	   (org-macro-replace-all org-macro-templates parsed-keywords)

	   ;; Refresh buffer properties and radio targets after previous
	   ;; potentially invasive changes.
	   (org-set-regexps-and-options)
	   (org-update-radio-target-regexp)
           (setq modified-tick (buffer-chars-modified-tick))
	   ;;  Possibly execute Babel code.  Re-run a macro expansion
	   ;;  specifically for {{{results}}} since inline source blocks
	   ;;  may have generated some more.  Refresh buffer properties
	   ;;  and radio targets another time.
	   (when org-export-use-babel
	     (org-babel-exp-process-buffer)
	     (org-macro-replace-all '(("results" . "$1")) parsed-keywords)
             (unless (eq modified-tick (buffer-chars-modified-tick))
	       (org-set-regexps-and-options)
	       (org-update-radio-target-regexp))
             (setq modified-tick (buffer-chars-modified-tick)))
	   ;; Run last hook with current back-end's name as argument.
	   ;; Update buffer properties and radio targets one last time
	   ;; before parsing.
	   (goto-char (point-min))
	   (save-excursion
	     (run-hook-with-args 'org-export-before-parsing-hook
			         (org-export-backend-name backend)))
           (unless (eq modified-tick (buffer-chars-modified-tick))
	     (org-set-regexps-and-options)
	     (org-update-radio-target-regexp))
           (setq modified-tick (buffer-chars-modified-tick))
	   ;; Update communication channel with environment.
	   (setq info
	         (org-combine-plists
		  info (org-export-get-environment backend subtreep ext-plist)))
           ;; Pre-process citations environment, i.e. install
	   ;; bibliography list, and citation processor in INFO.
	   (org-cite-store-bibliography info)
           (org-cite-store-export-processor info)
	   ;; De-activate uninterpreted data from parsed keywords.
	   (dolist (entry (append (org-export-get-all-options backend)
				  org-export-options-alist))
	     (pcase entry
	       (`(,p ,_ ,_ ,_ parse)
	        (let ((value (plist-get info p)))
		  (plist-put info
			     p
			     (org-export--remove-uninterpreted-data value info))))
	       (_ nil)))
	   ;; Install user's and developer's filters.
	   (setq info (org-export-install-filters info))
	   ;; Call options filters and update export options.  We do not
	   ;; use `org-export-filter-apply-functions' here since the
	   ;; arity of such filters is different.
	   (let ((backend-name (org-export-backend-name backend)))
	     (dolist (filter (plist-get info :filter-options))
	       (let ((result (funcall filter info backend-name)))
	         (when result (setq info result)))))
	   ;; Parse buffer.
	   (setq tree (org-element-parse-buffer nil visible-only))
	   ;; Prune tree from non-exported elements and transform
	   ;; uninterpreted elements or objects in both parse tree and
	   ;; communication channel.
	   (org-export--prune-tree tree info)
	   (org-export--remove-uninterpreted-data tree info)
	   ;; Call parse tree filters.
	   (setq tree
	         (org-export-filter-apply-functions
		  (plist-get info :filter-parse-tree) tree info))
	   ;; Now tree is complete, compute its properties and add them
	   ;; to communication channel.
	   (setq info (org-export--collect-tree-properties tree info))
           ;; Process citations and bibliography.  Replace each citation
	   ;; and "print_bibliography" keyword in the parse tree with
	   ;; the output of the selected citation export processor.
           (org-cite-process-citations info)
           (org-cite-process-bibliography info)
	   ;; Eventually transcode TREE.  Wrap the resulting string into
	   ;; a template.
	   (let* ((body (org-element-normalize-string
		         (or (org-export-data tree info) "")))
		  (inner-template (cdr (assq 'inner-template
					     (plist-get info :translate-alist))))
		  (full-body (org-export-filter-apply-functions
			      (plist-get info :filter-body)
			      (if (not (functionp inner-template)) body
			        (funcall inner-template body info))
			      info))
		  (template (cdr (assq 'template
				       (plist-get info :translate-alist))))
                  (output
                   (if (or (not (functionp template)) body-only) full-body
	             (funcall template full-body info))))
             ;; Call citation export finalizer.
             (setq output (org-cite-finalize-export output info))
	     ;; Remove all text properties since they cannot be
	     ;; retrieved from an external process.  Finally call
	     ;; final-output filter and return result.
	     (org-no-properties
	      (org-export-filter-apply-functions
	       (plist-get info :filter-final-output)
	       output info)))))))))
