#!/usr/bin/env doomscript
(require 'doom-start)
(require 'ox-tufte)
(require 'org)
(require 'ox-publish)



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


(defun salih/compare-timestamps (node1 node2)
  "Comparison function to sort structs based on timestamp slot."
  (let ((timestamp1 (salih/get-date (org-roam-node-file node1)))
        (timestamp2 (salih/get-date (org-roam-node-file node2))))
    (time-less-p  timestamp2 timestamp1)))



(defun salih/get-back-nodes (id)
  (let ((backlinks (org-roam-backlinks-get (org-roam-node-from-id id) :unique t))
        nodes '())
    (while backlinks
      (push (org-roam-backlink-source-node (car backlinks)) nodes)
      (setq backlinks  (cdr backlinks)))
    (sort nodes #'salih/compare-timestamps)))



(defun salih/mkentity (node counter)
  (let* ((id (org-roam-node-id node))
         (entry (org-roam-node-file (org-roam-node-from-id id)))
         (preview (if (salih/get-preview entry)
                      (salih/get-preview entry)
                    "(No preview)")))
    (format "** %s. [[id:%s][%s]]\n#+BEGIN_smth\n%s\n#+END_smth\n%s"
            counter
            id
            (car (org-publish-find-property entry :title nil))
            (format-time-string "%a %d %b %Y" (salih/get-date entry))
            preview)))


(defun salih/print-back-links ()
  (let* ((nodes (salih/get-back-nodes (org-entry-get nil "ID" t)))
         (strings '())
         (counter (length nodes)))
    (while nodes (push (salih/org-string-to-html (salih/mkentity (car nodes) counter))
                       strings)
           (setq nodes (cdr nodes)
                 counter (- counter 1)))
    (mapconcat 'identity strings "")))



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


;; modify this one! (if necessary)
(defun salih/org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat "#+OPTIONS: toc:nil")
  (org-list-to-subtree list))

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



(defun salih/set-org-publish-project-alist ()
  "Set publishing projects for Orgweb and Worg."
  (interactive)
  (setq org-publish-project-alist
        `(("blog-notes"
           ;; Directory for source files in org format
           :language "zh"
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
