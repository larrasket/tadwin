> Please **do note** that this software is not even a pre-alpha, it&rsquo;s just my
> personal script. It might not work for you but it will surely help inspiring you
> to build something similar, or it will work for you after some modifications.

This repository holds code for my publishing system that I use for my [website](https://lr0.fly.dev/),
written in elisp.


# Requirements

-   `tadwin.el` uses the `doomscript` executor to access emacs, thus it requires
    a [doom emacs](https://github.com/doomemacs/doomemacs) installation, however this can be modified easily to use a
    vanilla emacs instead.
-   [`org-roam`](https://github.com/org-roam/org-roam) is required by default to link the nodes (website org files) with
    each others, however this is can be dismissed too, but you will have to depend
    on hierarchical approach to organize your files, which often breaks.
    -   You can use `org-ids` file to get around this you don&rsquo;t use org-roam.


# Get Started

Clone the repository and edit [assets/](assets/) to match with your taste. Go through the
<tadwin.el> file quickly to make sure it&rsquo;s adjusted to what you wish. Make
sure to redfine `org-id-locations-file` in <tadwin.el> to your ids file location.

Afterwards, create `content/` directory, this directory will hold the org files
for your site. Start with creating a `posts` directory under `content` and add
some org files, do not forget to add date.

Now, under `content` you should instantiate an `index.org` file that will
represent the index of your website. Let&rsquo;s say you want to make it consists of
the posts directory you just created, then add:

    #+TITLE: My website
    #+OPTIONS: toc:nil
    
    * Recent Posts
    #+INCLUDE: sitemap.org::*posts :lines "-26" :only-contents t

Where 26 is the number of the posts you want to include $\times 5 + 1$ (each post with date
and preview requires 5 lines, and 1 line for org-offset) In this case it
includes the first 5 posts.


# Publishing

Publish the site by executing `tadwin.el`:

    chmod +x tadwin.el
    ./tadwin.el

This assumes that `doomscript` is in your `$PATH`. It will create a `public/`
directory with your static website. You can publish it later using whatever
service you wish, like [fly.io](https://fly.io/) or [GitHub Pages](https://pages.github.com/).


# Tip for org-roam

If you wish sometimes to include the backlinks for a node that you have, you can
add something like:

    #+BEGIN_SRC emacs-lisp :exports results :results value html
    (salih/print-back-links)
    #+END_SRC

Which will be evaluated when you export the site to a list of headings for your backlinks.

