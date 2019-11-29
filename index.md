---
title: wei2912's articles
---

# [wei2912](https://github.com/wei2912)'s articles

## Summary Pieces

These articles are *easier*-to-read summary pieces on a few select topics,
intended to let people dip their toes into unfamiliar technical topics.

### [Why AES is Secure](/posts/crypto/why-aes-is-secure.html)
(January 1, 2017) An explanation of how AES, a (very) commonly used modern
crytosystem, was designed to be resistant to many different types of
cryptographic attacks.

### [Introduction to Haskell](/posts/haskell/intro-to-haskell.html)
(November 27, 2014) A brief introduction to Haskell, for programmers wondering
what functional programming languages have to offer over imperative languages.

---

## Design of Blog

The design of this blog was intended to be minimalist, while being very
readable and following responsive design principles. The general look and feel
of the blog is inspired by [Mike Bostock](https://bost.ocks.org/mike)'s
website. I used these software to build the website:

* [Hakyll](https://jaspervdj.be/hakyll), a static site generator written in
Haskell which allows for great flexibility in the building of pages,
* [KaTeX](https://katex.org) for [LaTeX](https://www.latex-project.org/) math
equations
* [Travis CI](https://travis-ci.org) which builds the source at
[blog-src](https://github.com/wei2912/blog-src) and deploys to
[blog](https://github.com/wei2912/blog), which is pulled regularly by my
server

My website is hosted on [DigitalOcean](https://www.digitalocean.com) which
also hosts other software, all at a very reasonable rate of USD$6.00/month.
[Cloudflare](https://www.cloudflare.com) serves DNS requests and all of the
content on this website. I run the following services on my server:

* [Docker](https://www.docker.com) for containers
* [nginx](https://nginx.org) for website hosting and reverse proxy
* [LetsEncrypt](https://letsencrypt.org) which provides the free (!) SSL
certificates used on this website
* [Matomo](https://matomo.org) which provides free web analytics that respects
user privacy, along with [MariaDB](https://mariadb.org) hosting the database
and [PHP](https://www.php.net/) which Matomo is unfortunately built on
* [Commento](https://commento.io) which provides the commenting platform on
the posts, along with [PostgreSQL](https://www.postgresql.org) hosting the
database

In addition to the above, I use these external services:

* [Cloudflare](https://www.cloudflare.com) serves DNS requests as well as all
of the content for [weien.io](https://weien.io)
* [Mailgun](https://www.mailgun.com/) for notification emails
* [Askimet](https://akismet.com) for comments spam protection
* [Google Search Console](https://search.google.com/search-console) to make
sure my website doesn't disappear from Google

---

