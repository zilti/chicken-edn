chicken-edn
===========

An [EDN](https://github.com/edn-format/edn) reader and writer for chicken scheme.

Data type conversions
---------------------

 * DONE All kinds of numbers get converted to Scheme numbers, precision suffixes (N and M) get ignored.
 * DONE Keywords :keyword get converted to chicken scheme keywords keyword:.
 * TODO Maps get converted to a-lists (and vice versa). There'll be a global setting to use srfi-69 hashtables instead.
 * TODO Vectors are srfi-4 vectors.
 * TODO EDN Sets get converted to lists.
 * TODO The #inst tag requires [egg rfc3339](http://wiki.call-cc.org/eggref/4/rfc3339). You'll have to manually register a handler, but a convenience function is provided.
 * TODO #uuid is read as a String.

API
---
 *TODO*

Other tasks
-----------

 * Clean up code
 * Tag handling
 * Add testing code
 * Chicken egg

About
-----
Written by Daniel Ziltener. EDN written by Rick Hickey. The EDN specification is available at [https://github.com/edn-format/edn](https://github.com/edn-format/edn).
