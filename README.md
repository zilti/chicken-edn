chicken-edn
===========

An [EDN](https://github.com/edn-format/edn) reader and writer for chicken scheme.

In this release this implementation can only read EDN strings and files. EDN Tags and EDN-writing will follow soon.

Data type conversions
---------------------

 * DONE All kinds of numbers get converted to Scheme numbers, precision suffixes (N and M) get ignored.
 * DONE Keywords :keyword get converted to chicken scheme keywords keyword:.
 * DONE Maps get converted to a-lists (and vice versa). There'll be a global setting to use srfi-69 hashtables instead.
 * DONE Vectors are srfi-4 vectors.
 * DONE EDN Sets get converted to lists.
 * TODO The #inst tag requires [egg rfc3339](http://wiki.call-cc.org/eggref/4/rfc3339). You'll have to manually register a handler, but a convenience function is provided.
 * TODO #uuid is read as a String.

API
---

 * (edn-read-file filename) Reads the contents of the file and returns a list where each entry is a top-level form of the .edn file.
 * (edn-read-string string) Reads a string containing one EDN-top-level-form and returns the scheme equivalent of it.

Other tasks
-----------

 * Clean up code
 * Tag handling
 * Chicken egg
 * EDN writing

Releases
--------

 * **0.2.1**: Can read EDN-files and -strings. EDN tags are not working yet.

Roadmap
-------

 * **0.3**: EDN tags work.
 * **0.3.1**: Code cleanup with performance improvements.
 * **0.4**: Can write scheme data structures (lists,vectors,hashmaps) to EDN.
 * **0.4.1**: Code cleanup with performance improvements.
 * **0.5**: API to write lists and vectors as sets, alists as maps, and tags.
 * **0.5.1**: Code cleanup.

About
-----
Written by Daniel Ziltener. EDN written by Rick Hickey. The EDN specification is available at [https://github.com/edn-format/edn](https://github.com/edn-format/edn).
