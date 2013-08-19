chicken-edn
===========

An [EDN](https://github.com/edn-format/edn) reader and writer for chicken scheme.

In this release this implementation can only read EDN strings and files. EDN Tags and EDN-writing will follow soon.

Data type conversions
---------------------

 * All kinds of numbers get converted to Scheme numbers, precision suffixes (N and M) get ignored.
 * Keywords :keyword get converted to chicken scheme keywords keyword:.
 * Maps get converted to a-lists (and vice versa). There'll be a global setting to use srfi-69 hashtables instead.
 * Vectors are srfi-4 vectors.
 * EDN Sets get converted to lists.
 * TODO The #inst tag requires [egg rfc3339](http://wiki.call-cc.org/eggref/4/rfc3339). You'll have to manually register a handler, but a convenience function is provided. (By default it is read as a String)
 * #uuid is read as a String.

Missing reader functionality
----------------------------
The reader doesn't yet fully support those EDN features. Denoted in parens is the target release when they'll be fixed.

 * DONE Built-in tags #_ #inst and #uuid *(0.3)*
 * DONE Add user-specifiable tags *(0.3)*
 * TODO `nil` is not handled yet and will be recognized as a symbol. *(0.3)*
 * TODO Numbers have to start with a digit. *(0.3)*
 * TODO The discard tag #_ has to be separated by whitespace from the element to be discarded. *(0.3)*

API
---

 * `(edn-read-file filename)` Reads the contents of the file and returns a list where each entry is a top-level form of the .edn file.
 * `(edn-read-string string)` Reads a string containing one EDN-top-level-form and returns the scheme equivalent of it.
 * `(edn-register-handler tag fn)` Register a tag (string, including the #) to a one-argument-function which will get a scheme data structure.
 * `(edn-deregister-handler tag)` Removes the handler associated with tag.

Other tasks
-----------

 * EDN writing

Releases
--------

 * **0.2.1**: Can read EDN-files and -strings. EDN tags are not working yet.
 * **0.2**: Can read EDN-strings with one top-level data structure.

Roadmap
-------

 * **0.3**: EDN tags, including special forms, work. #inst and #uuid both get read as strings. Add nil. Add number prefixes. Add no-space-required to #_ tag.
 * **0.3.1**: Code cleanup with performance improvements.
 * **0.4**: Can write scheme data structures (lists,vectors,hashmaps) to EDN.
 * **0.4.1**: Code cleanup with performance improvements.
 * **0.5**: API to write lists and vectors as sets, alists as maps, and tags.
 * **0.5.1**: Code cleanup.

About
-----
Written by Daniel Ziltener. EDN written by Rich Hickey. The EDN specification is available at [https://github.com/edn-format/edn](https://github.com/edn-format/edn).
