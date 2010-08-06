---
title: Paper Trail
---

**Paper Trail** is a single-user publishing tool for [book reviews][books].
It's written in [Ruby on Rails][rails], and the [source code][source] is
available on GitHub, licensed under the GPL.

Some extremely rudimentary documentation is included in the `README` file, and
I noted its intentions and shortcomings in an [introductory article][intro]
(further mentions of it [have appeared since][more]). If you find any bugs,
please let me know via the [GitHub issue tracker][bugs].

URLify
------

[URLify][urlify] is a library extracted from Paper Trail for converting
diacritical marks to unaccented equivalents, to assist with ASCII-safe URI
creation. It also includes a utility method to remove subtitles. URLify is a
Ruby gem, so to install it just run this (depending on your system
configuration, you may have to `sudo`):

    gem install urlify

URLify is released under the BSD license.

  [books]:  http://books.extralogical.net
  [rails]:  http://rubyonrails.org
  [source]: http://github.com/ionfish/papertrail
  [intro]:  $root/2008/11/bookkeeping
  [more]:   $root/taxon/paper-trail
  [bugs]:   http://github.com/ionfish/papertrail/issues
  [urlify]: http://github.com/ionfish/urlify
