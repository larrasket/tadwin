(setq grid-view (format "%s" "#+begin_export HTML
<head>
<style>

h1:not(#preamble h1):before {
    content: '■';
    font-size: 0.8em;
    margin-right: 0.5em;
    vertical-align: middle;
    color: #ff0101;
}


h1 b, h2 b, h3 b, h4 b {
background: rgba(38, 38, 38, 0.64);
padding: 0.1em 0.7em;
color: white;
font-size: 0.6em;
}



h2,
h3,
h4,
h5,
h6,
h7,
h8 {
  font-style: normal !important;
  font-family: Arial;
}


h3 {
    margin-block-end: 0;
}

h2,h3,h4,h5,h6,h7,h8 {
    font-weight: normal;
    margin-top: 0px;
}

.outline-2  {
    display: flex;
    align-items: baseline;
}


.outline-3  {
    display: flex;
    align-items: baseline;
}

@media screen and (max-width: 1089px) {

.outline-2 {
    display: grid;
    grid-template-columns: minmax(0, 3.6fr) minmax(0, 1fr);
    align-items: baseline;
}


.outline-3 {
    display: grid;
    grid-template-columns: minmax(0, 3.6fr) minmax(0, 1fr);
    align-items: baseline;
}

}

</style>
</head>
#+end_export"))


(setq arabic-view (f-read-text "~/blog/arabicview"))

(setq org-export-global-macros
      '(("comments" . "(eval (salih/print-text-nodes))")
        ("anth" . "(eval (salih/print-text-nodes-anth))")
        ("recent" . "(eval (salih/recents))")
        ("homecomments" . "(eval (salih/print-text-nodes t))")
        ("dis" . "(eval (format \"* Comments \n #+begin_export html\n%s\n#+end_export\" isso-comments ))")
        ("b" . "(eval (format \"#+begin_export html\n%s\n#+end_export\" (salih/print-back-links)))")
        ("t" . "(eval (concat \"This section was labeled under, or is related to\"))")
        ("indx" . "(eval (concat grid-view))")
        ("ar" . "(eval (concat arabic-view))")
        ("s" . "(eval (concat \"Part of a series on\"))")))
