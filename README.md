# cl-patterns

A library for writing SuperCollider-esque patterns in Lisp.

IDEAS
=====

* `tsubseq` function for getting a subsequence based on start and end times of events.
* automatically convert between different ways to represent the same thing in events
  * for example, if you set an event's `amp` but then try to access its `db`, it calculates the `db` based on the `amp` value.
  * the same should be true for when you set the values - setting `db` instead allows you to get the `amp`
  * interally, an Event only keeps track of `amp` and just converts to whatever type you request, or converts whatever type you give it to `amp`.
* more metadata in patterns and streams so that it's easier to write functions that process streams/patterns/etc

TODO
====

* use `prove` to write tests for the patterns
