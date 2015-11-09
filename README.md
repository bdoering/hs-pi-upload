hs-pi-upload
============

A simple HTTP server implemented in Haskell, which offers a convenient
upload form for large and/or many files. The server can be compiled
and run on a Raspberry Pi.

The upload form is useful e.g. to collect photos and videos from
friends and family after a trip or wedding. Further details are given
in a
[blog post](https://doering.io/posts/2015-10-23-happstack-upload-on-raspberry-pi.html).

The server and frontend are based on:

* [Happstack Lite](http://www.happstack.com/page/view-page-slug/9/happstack-lite)
* [Resumable.js](http://www.resumablejs.com/)
