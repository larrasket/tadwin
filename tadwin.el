#!/usr/bin/env doomscript
(require 'doom-start)
(load "~/blog/id.el")

(setq isso-comments
      "<section id=\"isso-thread\">
    <noscript>Javascript needs to be activated to view comments.</noscript>
</section>
<script data-isso-css-url=\"https://lr0.fly.dev/style/comments.css\" data-isso-reply-notifications-default-enabled=\"true\" data-isso-vote=\"false\" data-isso=\"//salihcomments.fly.dev/\" src=\"//salihcomments.fly.dev/js/embed.min.js\"></script>")


(use-package ox-html-stable-ids
  :config
  (org-html-stable-ids-add))


(setq org-export-global-macros
      '(("comments" . "(eval (salih/print-text-nodes))")
        ("dis" . "(eval (format \"* Comments \n #+begin_export html\n%s\n#+end_export\" isso-comments ))")
        ("b" . "(eval (format \"#+begin_export html\n%s\n#+end_export\" (salih/print-back-links)))")
        ("t" . "(eval (concat \"This section was labeled under\"))")
        ("s" . "(eval (concat \"Part of a series on\"))")))


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
  "Export an Org-mode string to HTML."
  (let ((org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (with-temp-buffer
      (insert org-string)
      (org-export-string-as (buffer-string) 'html t))))




(defun salih/get-date (file)
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
           (file-attribute-modification-time (file-attributes file))))))

(defun salih/time-less-p (v1 v2 &optional reversed)
  (if reversed
      (time-less-p  timestamp1 timestamp2)
    (time-less-p  timestamp2 timestamp1)))


(defun salih/compare-timestamps (node1 node2)
  "Comparison function to sort structs based on timestamp slot."
  (let ((timestamp1 (salih/get-date (org-roam-node-file node1)))
        (timestamp2 (salih/get-date (org-roam-node-file node2))))
    (time-less-p  timestamp2 timestamp1)))




(defun salih/get-back-nodes (id &optional no-sort with-sh)
  (let ((backlinks (org-roam-backlinks-get (org-roam-node-from-id id) :unique t))
        nodes '())
    (while backlinks
      (let ((shp (cl-search "/sh/" (org-roam-node-file
                                    (org-roam-backlink-source-node (car backlinks))))))
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
         (preview (if (salih/get-preview entry)
                      (salih/get-preview entry)
                    "")))
    (if (and (cl-search "blog" (org-roam-node-file node)) (not (cl-search "/t/" (org-roam-node-file node))))
        (if tag
            (format "%s [[id:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n%s"
                    astr
                    id
                    (org-roam-node-title node)
                    (format-time-string "%a %d %b %Y" (salih/get-date entry))
                    preview)
          (format "%s %s. [[id:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n%s"
                  astr
                  counter
                  id
                  (org-roam-node-title node)
                  (format-time-string "%a %d %b %Y" (salih/get-date entry))
                  preview))
      "")))




(defun salih/compare-org-id (node1 node2)
  (let ((timestamp1 (cdr (org-id-decode  (org-roam-node-id node1))))
        (timestamp2 (cdr (org-id-decode  (org-roam-node-id node2)))))
    (time-less-p  timestamp1 timestamp2)))

(defun salih/print-text-nodes ()
  (let* ((nodes (sort (salih/get-back-nodes (org-entry-get nil "ID" t) ) #'salih/compare-org-id))
         (strings '()))
    (while nodes (push (salih/mkinclude (car nodes))
                       strings)
           (setq nodes (cdr nodes)))
    (mapconcat 'identity strings "")))


(defun salih/mkinclude (node &optional astr)
  (unless astr
    (setq astr "**"))
  (let* ((id (org-roam-node-id node))
         (entry (org-roam-node-file (org-roam-node-from-id id))))
    (format "#+INCLUDE: \"%s::#%s\" :only-contents nil\n"
            entry
            id)))





(defun salih/get-backlinks-html (&optional tag sh astr)
  (let* ((nodes (salih/get-back-nodes (org-entry-get nil "ID" t) tag sh))
         (strings '())
         (counter (length nodes)))
    (while nodes (push (salih/org-string-to-html (salih/mkentity
                                                  (car nodes)
                                                  counter tag astr))
                       strings)
           (setq nodes (cdr nodes)
                 counter (- counter 1)))
    (mapconcat 'identity strings "")))

(defun salih/print-back-links (&optional tag)
  (concat (salih/get-backlinks-html tag nil) "\n" (let ((shorts
                                                         (salih/get-backlinks-html tag t "")))
                                                    (unless (equal shorts "")
                                                      (concat (salih/org-string-to-html "** Short Posts") "\n" shorts)))))



(defun salih/get-preview (file)
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
        (buffer-substring beg end)))))

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
         (let ((preview (if (salih/get-preview (concat "content/" entry))
                            (salih/get-preview (concat "content/" entry))
                          "(No preview)")))
           (format "[[file:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n%s"
                   entry
                   (org-publish-find-title entry project)

                   (format-time-string "%a %d %b %Y" (org-publish-find-date entry
                                                                            project))
                   preview)))
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
         (formatted-time (format-time-string "%A, %e %B %Y (%H:%M)" time)))
    (format "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">%s</span></span>"
            (replace-regexp-in-string "--" "&#x2013;" formatted-time))))


;; (defun org-drawerkk (name content)
;;   (format "@<div class=\"notes\"> %s @</div>" content))

;; (setq org-html-format-drawer-function 'org-drawerkk)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

(defun org-drawerkk (name content)
  (format "<div class=\"notes\"> %s </div>" content))

(setq org-html-format-drawer-function 'org-drawerkk)

(setq org-html-stable-ids t)

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

           :publishing-function org-html-publish-to-tufte-html
           :exclude "\!.*\.org"
           :makeindex t
           :html-head-include-default-style nil)

          ("blog-static"
           :base-directory "./content"
           :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|svg\\|pdf\\|mp3\\|woff2\\|woff"
           :publishing-directory "public"
           :recursive t
           :publishing-function org-publish-attachment)

          ("assets"
           :base-directory "./assets"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf\\|mp3\\|woff2\\|woff\\|html\\|md\\|ico"
           :publishing-directory "public"
           :recursive t
           :publishing-function org-publish-attachment)

          ("blog" :components ("blog-notes" "assets" "blog-static")))))

(salih/set-org-publish-project-alist)


(org-publish-all t)

(message "Build Complete!")
