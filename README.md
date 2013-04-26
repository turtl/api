tag.it
======
Tag.it is a client-side encrypted note/bookmark storage app.

The idea is that you can use tag.it to track and organize notes, bookmarks,
private/personal data, etc. All data is encrypted in the browser before sending
it to the server, so even the people running the servers have no access to any
of the data.

The only public data in the database are the IDs of the records and the links
between the records, and some metadata (like sort order). Your entire user
record (including username) and all project/note data is completely encrypted.
The only way to access any of your information is to use your username/password
combination on login.

Tag.it currently uses AES (symmetric-key) encryption to work its magic.

Tag.it is alpha
---------------
While tag.it uses STATE OF THE ART ENTERPRISE SECURITY SOLUTIONS I admit that
my knowledge of encryption is that of a child who wanders into the middle of a
scientist's disseration on why relativity is only half-correct. This means that
while I've implemented encryption, I may have done so stupidly. *Please use
tag.it at your own risk (for the time being).*

\- andrew

Open-source
-----------
Tag.it is open source because we believe it's important to have transparency.
You wouldn't know what we're doing with your info unless you were able to look
at the code, so here it is.

Dependencies
------------
You are more than welcome to run your own tag.it server/app (although do your
own branding...OR ELSE).

Tag.it's backend is written in Common Lisp, and requires:

- [Wookie](https://github.com/orthecreedence/wookie) - An async webserver for
Common Lisp
- [cl-rethinkdb](https://github.com/orthecreedence/cl-rethinkdb) - A RethinkDB
driver for Common Lisp.

All other deps should be loadable via [Quicklisp](http://www.quicklisp.org/beta/).

Tag.it uses [RethinkDB](http://www.rethinkdb.com/) as its data store. It was
chosen because it scales nicely and does well with fluid/linked data.

Tag.it's front-end is built on top of [Mootools](http://mootools.net/) and
[Composer.js](http://lyonbros.github.io/composer.js/).

License
-------
Tag.it's code is licensed MIT. You can wrap it up in ribbons, you can slip it in
your sock.

HOWEVER, as mentioned, the brand is NOT open source. If you run your own version
of the tag.it app, you must re-brand. Also note that tag.it might in the future
have premium/payed features that will not be open-sourced.

