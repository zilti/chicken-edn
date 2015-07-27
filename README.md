chicken-edn
===========

An [EDN](https://github.com/edn-format/edn) reader and writer for chicken scheme.

In this release this implementation can only read EDN strings and files. EDN-writing will follow soon.

Data type conversions
---------------------

 * All kinds of numbers get converted to Scheme numbers, precision suffixes (N and M) get ignored.
 * Keywords :keyword get converted to chicken scheme keywords keyword:.
 * Maps get converted to SRFI 69 hashtables.
 * Vectors are srfi-4 vectors.
 * true = #t, false = #f, nil = #f.
 * Reader tag functionality is supported, but no handlers are provided, and it's currently untested.

Missing reader functionality
----------------------------
Should you notice missing functionality of the reader, plesase use [the issues page](https://github.com/zilti/chicken-edn/issues) to report
it and, if possible, provide a minimal test case.

API
---

* Transforming EDN into Chicken: `(call-with-port <port> read-edn)`
* Transforming Chicken into EDN: `(call-with-port <port> (write-edn <datastructure>)`

Releases
--------

 * **0.4**: Complete rewrite. Only relies on R7RS, and SRFI 1, 4, 69 and 88. Uses ports. Reads and writes EDN.
 * **0.3**: EDN tags, including special forms, work. #inst and #uuid both get read as strings. Add nil. Add number prefixes. Add no-space-required to #_ tag.
 * **0.2.1**: Can read EDN-files and -strings. EDN tags are not working yet.
 * **0.2**: Can read EDN-strings with one top-level data structure.

Roadmap
-------


About
-----
Written by Daniel Ziltener. EDN written by Rich Hickey. The EDN specification is available at [https://github.com/edn-format/edn](https://github.com/edn-format/edn).
