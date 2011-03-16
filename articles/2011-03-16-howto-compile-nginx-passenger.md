---
title: How to compile your own Nginx and Passenger
description: Tutorial explaining how to compile Nginx and Passenger to serve Ruby web applications.
---

Every few months I upgrade my server's [Nginx] and [Passenger] installations,
and whenever I do, it takes me a minute to remember how it all goes. This
article explains how to compile them from scratch, and also how to upgrade
either or both programs. My server runs Ubuntu 10.04, but it should be
straightforward to modify these instructions to work on any Linux distribution
or POSIX-compliant operating system.

[Nginx]:     http://nginx.org
[Passenger]: http://www.modrails.com


Preamble
--------

Before we get going, a brief note on how Nginx and Passenger fit together.
Nginx is a web server, much like [Apache]. Passenger is an application server
for web applications written in Ruby. Passenger provides an Nginx module, and
the only way to add a module to Nginx is to compile it with that module, so
even if you're only upgrading or installing Passenger, you still need to
(re)compile Nginx.

This article is a guide to installing these two programs from scratch, but it's
also a guide to upgrading one or both. If you get lost at any point---if
something happens which is outside the scope of this guide---then I strongly
recommend taking a look at the Passenger [users guide], which is admirably
clear and comprehensive.

The commands that follow need to be run as root. I'm the only admin on my
system, so I just become root; if you prefer you can just prefix commands with
`sudo`. Commands will be prefixed with the `$` character; anything else is
output provided for expository purposes and should not be typed into your
terminal.

    $ su -

[Apache]:      http://httpd.apache.org/
[users guide]: http://www.modrails.com/documentation/Users%20guide%20Nginx.html


Installing Ruby
---------------

Passenger is a Ruby web server, so the first dependency you'll need to install
is [Ruby]. It should be available from your package manager; on Ubuntu you can
use `apt-get`.

    $ apt-get install ruby

I prefer to compile my own, but I shan't cover that here. From here on in I'll
assume that you have a working, Passenger-compatible Ruby installation and
[Rubygems]. You'll also need `wget` or `curl` to download files.

[Ruby]:     http://www.ruby-lang.org
[Rubygems]: http://rubygems.org/


Installing other dependencies
-----------------------------

Both Passenger and Nginx, as one would expect, have various dependencies. The
list below is exhaustive for the configuration I outline, but if you compile
more modules into Nginx then you may have to install additional prerequisites.

    $ apt-get install build-essential libpcre3-dev libssl-dev zlib1g

Nginx's HTTP rewrite module requires the [PCRE] library sources, so it can
parse regular expressions in `location` directives. It also needs the [OpenSSL]
header files for SSL support, and [zlib] so that responses can be compressed.

[PCRE]:    http://www.pcre.org/
[OpenSSL]: http://openssl.org/
[zlib]:    http://zlib.net/


Installing the Passenger gem
----------------------------

Downloading and installing the Passenger library is extremely simple: just run

    $ gem install passenger

This is a good time to note down just where the Passenger gem is installed. The
authors provide a handy command to let the user know just that.

    $ passenger-config --root
    /usr/local/lib/ruby/gems/1.9.1/gems/passenger-3.0.5
    $ PASSENGER_NGINX_DIR=`passenger-config --root`/ext/nginx

Compiling Nginx
---------------

At this point you'll need to download the Nginx source files to your server. I
keep mine in `/usr/local/src`, but you can do this anywhere. As of the time of
writing, the latest stable version of Nginx is 0.8.54, but of course that may
well have changed by the time you read this, and so the URL to pass to `wget`
will have changed too.

    $ cd /usr/local/src
    $ wget http://nginx.org/download/nginx-0.8.54.tar.gz
    $ tar -xzvf nginx-0.8.54.tar.gz
    $ NGINX_SRC_DIR=/usr/local/src/nginx-0.8.54/
    $ cd $NGINX_SRC_DIR

We're now at the point where some decisions need to be made. These include
where you want to install the `nginx` binary, where your Nginx config file is
going to live, where it should write log files, and most importantly in this
context, which modules you want to compile with it. I'm going to describe a
fairly standard, minimal setup with just TLS and Gzip support, as well as
compiling in the Passenger module. The [install options] page on the Nginx wiki
is very comprehensive if you want to deviate from the ones provided here.

    $ ./configure \
      --prefix=/usr/local \
      --sbin-path=/usr/local/sbin \
      --conf-path=/etc/nginx/nginx.conf \
      --error-log-path=/var/log/nginx/error.log \
      --http-log-path=/var/log/nginx/access.log \
      --with-http_ssl_module \
      --with-http_gzip_static_module \
      --add-module=$PASSENGER_NGINX_DIR
    $ make
    $ make install


[install options]: http://wiki.nginx.org/InstallOptions


Configuring Nginx and Passenger
-------------------------------

I'm only going to cover the basics of setting up and configuring Nginx and
Passenger---there are plenty of other articles out there which provide
comprehensive coverage of this topic. For that reason, exposition here will be
relatively brief and high level; you can get the details elsewhere should you
need them.

Managing a web server process by hand is, of course, not something anyone wants
to do---that's what [initialisation scripts] are for. Just copy the source of
that script to `/etc/init.d/nginx` (depending on the details of your setup, you
might need to tweak it a little), then make it writable, and add it to the
default run levels so it'll start automatically on boot. For details, consult
the Linode Library articles on [installing Nginx]. They have their own init
script which you might prefer to the one linked above.

    $ chmod +x /etc/init.d/nginx
    $ /usr/sbin/update-rc.d -f nginx defaults

The next step is to set up your `nginx.conf` file. The best way to start is to
use the configuration that comes with the Nginx source as a basis for your own.
Again, Linode have some excellent articles on [configuring Nginx].

    $ cd $NGINX_SRC_DIR
    $ cp -R conf /etc/nginx
    $ mdkir /etc/nginx/conf.d

Then edit `/etc/nginx/nginx.conf` and add the following line before the end of
the `http` block.

    include /etc/nginx/conf.d/*.conf;

That will pull in any configuration files you place in the `/etc/nginx/conf.d/`
directory, including the one you're about to add. Open up
`/etc/nginx/conf.d/passenger.conf` in a text editor and add the following two
lines:

    passenger_root /usr/local/lib/ruby/gems/1.9.1/gems/passenger-3.0.5;
    passenger_ruby /usr/local/bin/ruby;

The value of the `passenger_root` variable should be the output of running
`passenger-config --root`, while the second should be the path to your `ruby`
binary (you can get it by running `which ruby`). Keeping this information in a
separate configuration file to your main Nginx config just makes it simpler to
update when you upgrade Passenger and need to change that the value of
`passenger_root` to point to the new location.

All that's needed now is to let Nginx know about the [Rack] application you
want to run. Again, there are plenty of other articles which explain this
process, particularly the relevant sections of the Passenger users guide ([§3]
for Ruby on Rails applications, [§4] for generic Rack applications).
Essentially, all that's needed is to tell Nginx what the path to your Rack
application is, and that it should use Passenger. The minimal configuration
below should suffice---just put it (suitably modified to reflect the details
of your site) somewhere Nginx will find it.

    server {
        listen 80;
        
        server_name myrackapp.net;
        root /var/www/myrackapp.net/public;
        
        passenger_enabled on;
        rails_env production;
    }

Most of this is common to all Nginx `server` directives---the ones we're
interested in are `passenger_enabled` and `rails_env`. The first makes Nginx
enable Passenger for that server, while the latter specifies the environment
the relevant Rack application should be run in. Of course, there are many more
options available than just these two---for details, consult [§5] of the guide.
Now you just need to start up the server.

    $ /etc/init.d/nginx start

[initialisation scripts]: http://code.google.com/p/nginx-init-ubuntu
[installing Nginx]:       http://library.linode.com/web-servers/nginx/installation/
[configuring Nginx]:      http://library.linode.com/web-servers/nginx/configuration/
[Rack]:                   http://rack.rubyforge.org/
[§3]:                     http://www.modrails.com/documentation/Users%20guide%20Nginx.html#deploying_a_ror_app
[§4]:                     http://www.modrails.com/documentation/Users%20guide%20Nginx.html#deploying_a_rack_app
[§5]:                     http://www.modrails.com/documentation/Users%20guide%20Nginx.html#_configuring_phusion_passenger


Recompiling Nginx
-----------------

If you want to install a new version of Nginx, or a new version of Passenger,
you'll have to recompile Nginx. If it's the latter, install the new Passenger
gem as described above, and then compile Nginx. There are two gotchas to this
process, and one handy trick: if you ask nicely, an `nginx` binary will tell
you exactly what configure arguments were passed when compiling it.

    $ cd $NGINX_SRC_DIR
    $ nginx -V
    nginx version: nginx/0.8.54
    built by gcc 4.4.3 (Ubuntu 4.4.3-4ubuntu5)
    TLS SNI support enabled
    configure arguments: --prefix=/usr/local --sbin-path=/usr/local/sbin --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --add-module=/usr/local/lib/ruby/gems/1.9.1/gems/passenger-3.0.5/ext/nginx --with-http_ssl_module --with-http_gzip_static_module

This is incredibly handy: you don't need to write down or reconstruct what
options you chose you compiled Nginx, as the program remembers them all for
you. Now, if you've just installed a new version of Passenger, you can't just
copy and paste the arguments verbatim---you have to change the value of the
`add-module` flag to point to the location of the new Passenger gem.

Run `./configure` with the appropriate flags and then `make` as above. Since
you're recompiling, presumably your existing `nginx` program is running at this
point, so you need to stop it before installing the new binary. If you're
reinstalling Passenger, you must update your `passenger.conf` file with the new
path to the library. Then stop `nginx`, install the new binary, and restart it.

    $ /etc/init.d/nginx stop
    $ make install
    $ /etc/init.d/nginx start
