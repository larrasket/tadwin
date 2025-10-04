#!/usr/bin/env /Users/l/.emacs.d/bin/doomscript
(provide 'tadwin)
(require 'doom-start)
(load "~/blog/id.el")
(load "~/blog/formats.el")

(setq ess-ask-for-ess-directory nil)


(defun salih/recents (d)
  (if d
      (s-replace-all '(("class=\"outline" . "class=\"recents")
                       ("<h2 id=\"posts\">Posts</h2>" .
                        "<h2> <a href=\"stack.html\">Top of My Stack</a> </h2> "))
                     (salih/org-string-to-html
                      (salih/org-get-subtree-as-org-string
                       "~/blog/content/stack.org" "Posts")))
    (s-replace-all '(("class=\"outline" . "class=\"footrecents")
                     ("<h2 id=\"posts\">Posts</h2>" .
                      "<h2> <a href=\"stack.html\">Top of My Stack</a> </h2>"))
                 (salih/org-string-to-html
                  (salih/org-get-subtree-as-org-string
                   "~/blog/content/stack.org" "Posts")))))


(advice-add 'org-html-stable-ids--get-reference :override
            (lambda (orig-fun datum info)
              (if org-html-stable-ids
                  (let ((cache (plist-get info :internal-references))
                        (id (org-html-stable-ids--extract-id datum))
                        (RID (org-element-property :ID datum)))
                    (if (org-roam-node-from-id RID) RID
                      (or (car (rassq datum cache))
                          (progn (while (assoc id cache)
                                   (setq id (org-html--update-string-with-underscore id)))
                                 (when id (push (cons id datum)
                                                cache)
                                       (plist-put info :internal-references cache)
                                       id)))))

                (funcall orig-fun datum info))))


(setq header (with-temp-buffer
               (insert-file-contents "assets/head.html")
               (buffer-string)))
(setq preamble (with-temp-buffer
                 (insert-file-contents "assets/preamble.html")
                 (buffer-string)))

(setq postamalbe (with-temp-buffer
                   (insert-file-contents "assets/postamable.html")
                   (buffer-string)))

(setq org-export-time-stamp-file nil)
(setq org-id-locations-file "~/roam/.orgids")
(defvar salih/org-export-replace-links-counter 0)

(setq org-html-mathjax-options '((path "https://fastly.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
                                 (scale 1.0)
                                 (align "center")
                                 (font "mathjax-modern")
                                 (overflow "overflow")
                                 (tags "ams")
                                 (indent "0em")
                                 (multlinewidth "85%")
                                 (tagindent ".8em")
                                 (tagside "right")))

(defun salih/org-string-to-html (org-string)
  "Export an Org-mode string to HTML with proper heading levels."
  (let* ((org-export-with-toc nil)
         (org-export-with-section-numbers nil)
         (top-level
          (if (string-match "^\\(\\*+\\)\\s-+" org-string)
              (length (match-string 1 org-string))
            1)))
    (let ((org-html-toplevel-hlevel top-level))
      (with-temp-buffer
        (insert org-string)
        (org-export-string-as (buffer-string) 'html t)))))





(defun salih/get-date (file &optional node)
  (if node
      (cdr (org-id-decode node))
    (let ((date (org-publish-find-property file :date nil)))
    ;; DATE is a secondary string.  If it contains
    ;; a time-stamp, convert it to internal format.
    ;; Otherwise, use FILE modification time.
     (cond ((let ((ts (and (consp date) (assq 'timestamp date))))
              (and ts
                   (let ((value (org-element-interpret-data ts)))
                     (and (org-string-nw-p value)
                          (org-time-string-to-time value))))))
           ((file-exists-p file)
            (file-attribute-modification-time (file-attributes file)))))))

(defun salih/time-less-p (v1 v2 &optional reversed)
  (if reversed
      (time-less-p  timestamp1 timestamp2)
    (time-less-p  timestamp2 timestamp1)))


(defun salih/compare-timestamps (node1 node2)
  "Comparison function to sort structs based on timestamp slot."
  (let* ((node-1-file (org-roam-node-file node1))
         (node-2-file (org-roam-node-file node2))
         (use-id-1 (or
                    (cl-search "/sh/" node-1-file)
                    (cl-search "stack.org" node-1-file)))
         (use-id-2 (or
                    (cl-search "/sh/" node-2-file)
                    (cl-search "stack.org" node-2-file)))
         (timestamp1 (salih/get-date node-1-file (when use-id-1 (org-roam-node-id node1))))
         (timestamp2 (salih/get-date node-2-file (when use-id-2 (org-roam-node-id node2)))))
    (time-less-p  timestamp2 timestamp1)))
    ;; (cond ((and use-id-1 (not use-id-2)) t)
    ;;       ((and use-id-2 (not use-id-1)) nil)
    ;;       ((and (not use-id-1) (not use-id-2)) (time-less-p  timestamp2 timestamp1))
    ;;       (t (time-less-p  timestamp2 timestamp1)))





(defun salih/get-back-nodes (id &optional no-sort with-sh)
  (let ((backlinks (org-roam-backlinks-get (org-roam-node-from-id id) :unique t))
        nodes '())
    (while backlinks
      (let ((shp (or
                  (cl-search "/sh/" (org-roam-node-file
                                     (org-roam-backlink-source-node (car
                                                                     backlinks))))
                  (cl-search "stack.org" (org-roam-node-file
                                          (org-roam-backlink-source-node (car backlinks)))))))
        (if (or (and with-sh shp) (and (not with-sh) (not shp)))
            (push (org-roam-backlink-source-node (car backlinks)) nodes)))
      (setq backlinks  (cdr backlinks)))
    (if no-sort (reverse (sort nodes #'salih/compare-timestamps))
      (sort nodes #'salih/compare-timestamps))))



(defun salih/mkentity (node counter tag &optional astr)
  (unless astr
    (setq astr "**"))
  (let* ((id (org-roam-node-id node))
         (entry (org-roam-node-file (org-roam-node-from-id id)))
         (lang (if node (salih/get-node-property node "LANG")))
         (subtitle (if lang (salih/get-node-property node "SUBTITLE")))
         (use-id (or
                  (cl-search "/sh/" entry)
                  (cl-search "stack.org" entry)))
         (preview nil))
    (if (and (cl-search "blog" (org-roam-node-file node)) (not (cl-search "/t/" (org-roam-node-file node))))
        (if tag
            (format "%s %s[[id:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n"
                    astr
                    (if lang
                       "*Arabic* "
                     "")
                    id
                    (if subtitle
                        subtitle
                      (org-roam-node-title node))
                    (format-time-string "%a %d %b %Y" (salih/get-date entry (when use-id id))))

          (format "%s %s. %s [[id:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n"
                  astr
                  counter
                  (if lang
                       "*Arabic* "
                     "")
                  id
                  (if subtitle
                        subtitle
                      (org-roam-node-title node))
                  (format-time-string "%a %d %b %Y" (salih/get-date entry (when use-id id)))))

      "")))




(defun salih/compare-org-id (node1 node2)
  (let ((timestamp1 (cdr (org-id-decode  (org-roam-node-id node1))))
        (timestamp2 (cdr (org-id-decode  (org-roam-node-id node2)))))
    (time-less-p  timestamp1 timestamp2)))

(defun salih/print-text-nodes (&optional first)
  (let* ((nodes (sort (salih/get-back-nodes (if first
                                                (org-roam-node-id
                                                 (org-roam-node-from-title-or-alias
                                                  "Commentary"))
                                              (org-entry-get nil "ID" t)))
                      #'salih/compare-org-id))
         (strings '()))
    (if first
        (push (salih/mkinclude (car (last nodes)) nil) strings)
      (while nodes (push (salih/mkinclude (car nodes) nil)
                         strings)
             (setq nodes (cdr nodes))))
    (mapconcat 'identity strings "")))

(defun salih/print-text-nodes-anth ()
  (let* ((nodes (sort (salih/get-back-nodes "3xedczc0i2k0")
                      #'salih/compare-org-id))
         (strings '()))
    (while nodes (push (salih/mkinclude (car nodes) t)
                       strings)
           (setq nodes (cdr nodes)))
    (mapconcat 'identity strings "")))


(defun salih/mkinclude (node remove-title &optional astr)
  (unless astr
    (setq astr "**"))
  (let* ((id (org-roam-node-id node))
         (entry (org-roam-node-file (org-roam-node-from-id id)))
         (cite (file-name-sans-extension (file-name-nondirectory entry))))
    (if remove-title
        (format "#+INCLUDE: \"%s::#%s\" :only-contents t\n\n\n \n/Derived from/ [cite:@%s], no. %s. Appeared: %s\n-----\n"
                entry
                id
                cite
                (substring (cl-first (s-split " " (salih/get-node-property node "NOTER_PAGE"))) 1)
                (format-time-string "%Y-%m-%d (%H:%M)" (cdr (org-id-decode  (org-roam-node-id node)))))

        (format "#+INCLUDE: \"%s::#%s\" :only-contents nil\n"
            entry
            id))))


(defun salih/get-node-property (node property)
  (cdr (assoc property (org-roam-node-properties node))))



(defun salih/org-get-subtree-as-org-string (file-path heading)
  "Get the content of the subtree under a given HEADING in the Org file at FILE-PATH as an Org string."
  (with-temp-buffer
    ;; Load the Org file
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))
    ;; Search for the specified heading
    (if (re-search-forward (concat "^\\*+ " (regexp-quote heading)) nil t)
        (let ((start (line-beginning-position))
              (end (progn
                     ;; Move to the next heading at the same level or higher
                     (org-end-of-subtree t t)
                     (point))))
          ;; Return the substring between start and end
          (buffer-substring-no-properties start end))
      (error "Heading '%s' not found in %s" heading file-path))))




(defun salih/length (l)
  (let ((counter 0))
    (while l
      (when (and (car l) (cl-search "blog" (org-roam-node-file (car l))))
        (setq counter (+ counter 1)))
      (setq l (cdr l)))
    counter))



(defun salih/include-recents ()
  (salih/org-string-to-html (org-export-expand-include-keyword)))



(defun salih/get-backlinks-html (&optional tag sh astr)
  (let* ((nodes (salih/get-back-nodes (org-entry-get nil "ID" t) tag sh))
         (strings '())
         (counter (salih/length nodes)))
    (while nodes (push (salih/org-string-to-html (salih/mkentity
                                                  (car nodes)
                                                  counter tag astr))
                       strings)
           (if (and (car nodes) (cl-search "blog" (org-roam-node-file (car nodes))))
               (setq counter (- counter 1)))
           (setq nodes (cdr nodes)))


    (mapconcat 'identity strings "")))

(defun salih/print-back-links (&optional tag treat-shorts-as-long)
  (concat (salih/get-backlinks-html tag nil) "\n"
          (let ((shorts
                 (salih/get-backlinks-html t t (if treat-shorts-as-long nil "***"))))
            (unless (and (equal shorts ""))
              (concat (unless treat-shorts-as-long
                        (salih/org-string-to-html "** Short Posts")) "\n" shorts)))))



(defun salih/get-preview (file)
  (if t
      nil
    (with-temp-buffer
     (insert-file-contents file)
     (goto-char (point-min))
     (when (re-search-forward "^#\\+BEGIN_PREVIEW$" nil 1)
       (goto-char (point-min))
       (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$" nil 1)))
             (end (progn (re-search-forward "^#\\+END_PREVIEW$" nil 1)
                         (match-beginning 0))))
         (goto-char beg)
         (while (search-forward "\n" end t)
           (replace-match " " nil t))
         (buffer-substring beg end))))))

(defun salih/delete-index (list)
  "Delete items containing the word 'index' from LIST."
  (cond
   ((null list) nil)
   ((atom list)
    (if (stringp list)
        (unless (string-match-p "index.org" list)
          list)
      list))
   (t
    (let ((car-result (salih/delete-index (car list)))
          (cdr-result (salih/delete-index (cdr list))))
      (if (and (null car-result) (null cdr-result))
          nil
        (cons car-result cdr-result))))))


(defun salih/org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat "#+OPTIONS: toc:nil")
  (org-list-to-subtree (salih/delete-index list)))

;; modify this one!
(defun salih/org-publish-org-sitemap-format (entry style project)
  "Custom sitemap entry formatting: add date"
  (cond ((not (directory-name-p entry))
         (let* ((preview nil)
                (title (org-publish-find-title entry project))
                (node (org-roam-node-from-title-or-alias title))
                (lang (if node (salih/get-node-property node "LANG")))
                (subtitle (if lang (salih/get-node-property node "SUBTITLE"))))
           (format "%s[[file:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n"
                   (if lang
                       "*Arabic* "
                     "")
                   entry
                   (if subtitle
                       subtitle
                     title)
                   (format-time-string "%a %d %b %Y" (org-publish-find-date entry
                                                                            project)))))

        ((eq style 'tree)
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun salih/file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;;; additional settings
(setq org-html-style-default (salih/file-contents "assets/head.html"))


(setq org-html-prefer-user-labels t)

(setq org-export-with-clocks t)


(setq org-export-with-clocks t)

(defun org-html-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((ts (org-timestamp-translate timestamp))
         (time (apply #'encode-time (org-parse-time-string ts)))
         (formatted-time (format-time-string "[%Y-%m-%d %a %H:%M]" time)))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>"
            (replace-regexp-in-string "--" "&#x2013;" formatted-time))))


;; (defun org-drawerkk (name content)
;;   (format "@<div class=\"notes\"> %s @</div>" content))

;; (setq org-html-format-drawer-function 'org-drawerkk)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

(defun org-drawerkk (name content)
  (if (s-equals? name "REVIEW_DATA")
      ""
    (format "<div class=\"notes\"> %s </div>" content)))


(setq org-html-format-drawer-function 'org-drawerkk)



(defun salih/org-html-publish-to-tufte-html (plist filename pub-dir)
  "Make sure that the file is not already published befeore really publihing
it."
  (let* ((rebuild nil)
         (html (let* ((org-inhibit-startup t)
                      (visiting (find-buffer-visiting filename))
                      (salih/rebuild nil)
                      (extension (concat "." (or (plist-get plist :html-extension)
                                                 org-html-extension
                                                 "html")))
                      (work-buffer (or visiting (find-file-noselect filename))))
                 (unwind-protect
                     (with-current-buffer work-buffer
                       (when (ignore-errors (buffer-local-value 'salih/rebuild work-buffer))
                         (setq rebuild t))
                       (progn
                         (org-export-output-file-name extension nil pub-dir)))))))
    (when (or (string-match ".*index.*" filename)
              rebuild (file-newer-than-file-p filename html))
      (org-html-publish-to-tufte-html plist filename pub-dir))))



(setq my/org-referenced-filenames
      (let* ((org-dir (expand-file-name "content/" "~/blog"))
             (all-org (directory-files-recursively org-dir "\\.org\\'"))
             (filenames '()))
        (dolist (f all-org)
          (with-temp-buffer
            ;; locally pretend tabs are 8 columns
            (setq-local tab-width 8)
            (insert-file-contents f)
            (org-element-map (org-element-parse-buffer) 'link
              (lambda (lnk)
                (when (string= (org-element-property :type lnk) "file")
                  (push (file-name-nondirectory
                         (org-element-property :path lnk))
                        filenames))))))
        (delete-dups filenames)))


(defun salih/org-publish-attachment-if-referenced (plist filename pub-dir)
  "Publish the file at FILENAME into PUB-DIR only if its basename is in `my/org-referenced-filenames`."
  (let* ((basename (file-name-nondirectory filename)))
    (if (member basename my/org-referenced-filenames)
        (progn
          (unless (file-directory-p pub-dir)
            (make-directory pub-dir t))
          (let ((dest (expand-file-name basename pub-dir)))
            (unless (file-equal-p (file-truename filename)
                                  (file-truename dest))
              (copy-file filename dest t)) dest))
      (message basename "is not in my list"))))





(defun salih/set-org-publish-project-alist ()
  "Set publishing projects for Orgweb and Worg."
  (interactive)
  (setq org-publish-project-alist
        `(("blog-notes"
           ;; Directory for source files in org format
           :language "en"
           :base-directory "./content"
           :base-extension "org"
           :html-doctype "html5"
           :html-head ,header
           :html-html5-fancy t
           ;; HTML directory
           :publishing-directory "public"
           :recursive t
           :headline-levels 8
           :with-sub-superscript nil
           :section-numbers nil
           :auto-preamble nil
           :html-preamble ,preamble
           :html-postamble ,postamalbe

           :with-drawers t
           :auto-sitemap t
           :sitemap-title nil
           :sitemap-format-entry salih/org-publish-org-sitemap-format
           :sitemap-function salih/org-publish-org-sitemap
           :sitemap-sort-files anti-chronologically
           :sitemap-filename "sitemap.org"
           :sitemap-style tree
           :with-toc nil

           :publishing-function salih/org-html-publish-to-tufte-html
           :exclude "\!.*\.org"
           :makeindex t
           :html-head-include-default-style nil)

          ("blog-static"
           :base-directory "./content"
           :base-extension "css\\|js\\|png\\|jpg\\|html\\|json\\|jpeg\\|gif\\|svg\\|pdf\\|mp3\\|woff2\\|woff"
           :publishing-directory "public"
           :recursive t
           :publishing-function org-publish-attachment)

          ;; ("media"
          ;;  :base-directory "../media"
          ;;  :base-extension   "mp4\\|png\\|jpe?g\\|gif\\|svg"
          ;;  :publishing-directory "public/media"
          ;;  :recursive t
          ;;  :publishing-function salih/org-publish-attachment-if-referenced)

          ("assets"
           :base-directory "./assets"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf\\|mp3\\|woff2\\|woff\\|html\\|md\\|ico"
           :publishing-directory "public"
           :recursive t
           :publishing-function org-publish-attachment)

          ("blog" :components ("blog-notes"
                               ;; "media"
                               "assets" "blog-static")))))




(salih/set-org-publish-project-alist)


(add-hook! 'org-mode-hook (projectile-mode -1))
(org-publish-all t)
(remove-hook! 'org-mode-hook (projectile-mode -1))

(message "Build Complete!")
(setq org-html-htmlize-output-type nil) ; Disable syntax highlighting
