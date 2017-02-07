# cl-patterns

A library for writing SuperCollider-esque patterns in Lisp. Aims to emulate most of the patterns that are part of SuperCollider, but make them more robust, expressive, consistent, reflective, and lispy.

* robust - strongly prefer coercing values into something "correct" rather than failing or giving an error.
* expressive - make writing music as easy and "natural" as possible, so that patterns can be built in real-time, in performance settings, without having to think so hard about how to bend the library to your will. i feel this is a weakness of SuperCollider.
* consistent - edge cases minimized, arguments for the various functions in an intuitive order. pretty self-explanatory.
* reflective - store more data about the stream state and more metadata about the patterns. make it easier for a pattern to access the values of another pattern, for patterns to affect other patterns, etc.
* lispy - prefer lisp idioms rather than direct translations of the SuperCollider way of doing things (while still keeping things relatively similar so it's not a huge adjustment for SC users to make).

In addition to emulating most of SuperCollider's patterns system, another goal is to further extend it with more tools and more ways to write patterns/sequences, for example "drum sequence" notation like `k - - - k - - - k - - - k - - -` for a four-to-the-floor beat. The idea is that Lisp's macros should make it possible to more expressively write music with code.

## Intro

Download cl-patterns and put it in your quicklisp local-projects directory, then load it:

```common-lisp
(ql:quickload :cl-patterns)
(in-package :cl-patterns)
```

Create a pattern like so:

```common-lisp
(defparameter pat (pbind :foo (pseq '(1 2 3))
                         :bar (prand '(9 8 7) 5)))
```

Since patterns are basically "templates", you need to turn them into `pstream` objects in order to actually get output from them:

```common-lisp
(defparameter pstream (as-pstream pat))
```

Then, you can get results from the pstream one at a time with `next`, or many at a time with `next-n`:

```common-lisp
(defparameter list (next-n pstream 7))
```

You can play an event using the `play` function:

```common-lisp
(play (car list))
```

Or you can play the pattern itself, which will automatically convert it to a pstream for you "under the hood":

```common-lisp
(play pat)
```

If you want to actually hear sound output, you'll need to either use SuperCollider or Incudine for that, as `cl-patterns` doesn't create sound on its own:

```common-lisp
(ql:quickload :cl-patterns+supercollider)

(load #P"/path/to/cl-patterns/supercollider-example.lisp") ;; code to start scsynth and a few example synthdefs

(play (pbind :instrument :kik :freq (pseq '(100 200 400 800) 1)))
```

In the future, you might be able to do something like this to use Incudine as the output:

```common-lisp
(ql:quickload :cl-patterns+incudine)

(load #P"/path/to/cl-patterns/incudine-example.lisp")

(play (pbind :id 1 :freq (pseq '(100 200 400 800) 1)))
```

...But right now Incudine support isn't implemented.

If you have access to the `bordeaux-threads` library for threading, you can also fork a pattern:

```common-lisp
(fork (pbind :instrument :kik :freq (pseq '(100 200 400 800) 4)))
```

That way your REPL won't be tied up as it's playing. For now, the only way to stop a forked pattern is using SLIME's thread manager (`C-c C-x t` or `M-x slime-list-threads`) or by using `(bt:destroy-thread THREAD)` where THREAD is the object returned by the `fork` function.

## Features

This library isn't just a copy of SuperCollider's patterns - I wanted to improve upon them as well. Here are a few of the features of this library that are implemented right now:

* It's possible to "inject" an event's values into another from inside a pattern. For example:
```common-lisp
> (next-n (pbind :foo (pseq '(1 2 3 4))
               :inject (pseq (list (event) (event :bar 1 :baz 2) (event :qux 3))))
        3)
((EVENT :FOO 1)
 (EVENT :FOO 2 :BAR 1 :BAZ 2)
 (EVENT :FOO 3 :QUX 3))
```

* Event parameters that are different representations of the same concept are automatically converted between each other. For example, if you set the `amp` of an event and then try to get its `db`, the amp is automatically converted to db for you.
```common-lisp
> (db (event :amp 0.5))
-6.0205994
> (amp (event :db -3))
0.70794576
```

## Tour

* README.md - this file. self-expanatory, i'd hope.
* cl-patterns.asd - cl-patterns system definition with no backends.
* package.lisp - the package definition file.
* LICENSE - the GPLv3 license.
* SC.md - a list of pattern classes in SuperCollider and their cl-patterns implementation status.


* event.lisp - code to represent and deal with events (the event class, play functionality, `*event-output-function*`, etc)
* patterns.lisp - the current version of the patterns. includes the `pattern` superclass as well as `pbind` and `pseq`, `pk`, etc.
* clock.lisp - the scheduling functionality to make sure that each event is played at the proper time.
* pat-utilities.lisp - random utilities that don't fit anywhere else. also some notes for myself in case i forget to use `alexandria`.


* tests.lisp - test suite using `prove`.


* ideas.lisp - right now just a file to keep unsorted work-in-progress ideas in.
* misc.lisp - where i put my code when i'm just messing around with stuff. might be useful as example code, but could also have outdated stuff in it.


* cl-patterns+supercollider.asd - cl-patterns system definition with SuperCollider backend.
* supercollider.lisp - code to interface `cl-patterns` with the [cl-collider](https://github.com/defaultxr/cl-collider) library.
* cl-collider-extensions.lisp - a few additions to the cl-collider library for ease of use and cl-pattern interfacing.
* supercollider-example.lisp - example code to get started with the `cl-collider` library.


* incudine.lisp - code to interface `cl-patterns` with [incudine](https://github.com/titola/incudine) - WIP.

## Ideas/TODO

* implement `*latency*`

* `tsubseq` function for getting a subsequence based on start and end times of events.

* `tsubseq*` function. same as tsubseq* but it also includes for synths that would've already been playing at the start time specified.
  * i.e. `(tsubseq* (pbind :dur 2 :foo (pseq '(1 2 3))) 1 4)` returns `(list (event :dur 1 :foo 1) (event :dur 2 :foo 2))`

* do "static" things to "dynamic" patterns - i.e. `(pshift (pseq '(1 2 3)) 1)` results in `'(3 1 2 3 1 2 3 ...)` or the like. would work with event patterns too obviously and should "fail" gracefully by still giving output even if the source pattern is infinite-length (maybe just only operate on the first 16 beats, events, or output values by default for infinite patterns).

* more metadata in patterns and streams so that it's easier to write functions that process streams/patterns/etc
  * automatically record output from pstreams so it can be referenced later - store `*max-pattern-yield-length*` values from each pattern.
    * make a `current` function that will get the last value that was output from a pstream.

* make it possible to easily create lfos for the synth's parameters
  * can embed a synth definition (`sc:defsynth`) as the value, in which case the synth is triggered at the start of each pattern (or maybe for each event?)
  * can embed a `sc:proxy`, in which case the pattern just takes values from the output of the proxy.
  * can embed an Env, in which case a env-playing synth is mapped to the triggered synth's parameter.
  * maybe make it possible to change whether to retrigger for each event or just have the synth/env play for the duration of the pattern. perhaps retrigger if the synth/env is the result of an embedded pbind, but play for the duration if it's just a lone env/synthdef.

* make it possible to send out values at a specific key at a different rate
  * i.e.: `(pbind :dur 1 :foo (pseq '(1 2 3)) :bar (pbind :dur 1/2 :val (pseq '(9 8 7))))` results in `:foo` being set to 1, then 2, then 3 on every beat, while `:bar` is set to 9, then 8, then 7 on every half beat. effectively, the :bar sub-pattern is independent from the main pbind, it's just launched at the same time and ends at the same time.

* make macros to quickly write out patterns with symbols, i.e. `k---s---k---s---` for a kick/snare/kick/snare pattern or the like - see `ds` in `misc.lisp`

* add more tests to `tests.lisp`

* make patterns able to trigger other patterns. i.e. something like this:
```common-lisp
(progn
  (fork (pbind :name :bar :pefollow :foo :timing-offset 0.25))
  (fork (pbind :name :foo :dur (pseq '(0.5 0.5 0.5 0.5 1 1)))))
```
...then the `:bar` pattern's events will play 0.25 beats after each of `:foo`'s events play, because it's set to `:pefollow` that pattern.
  * similarly, a `:pfollow` key could be used to automatically start the pattern for each event of the source pattern. the default event would be the event from the source pattern that triggered the subpattern to play.

* `:cleanup` key for pbinds. this can either contain a function or a list of functions. when the pattern ends or is stopped, the function or functions will be called.
  * not sure if it should be called if the pattern is swapped out while playing, i.e. through pdef redefintion or the like.

* a generalized way to inject keys into an event from inside a pbind...?
  * or maybe just do something like `(pbind :inject (pcycles [32 - - [64 - -]]))` and pcycles would return keys for `:freq` and `:dur`. i.e. a syntax similar to TidalCycles?

* patterns from SuperCollider - see SC.md

* convert more patterns to `listpattern`s.

* `pclockdm` - clock divider/multiplier pattern. could be used, for example, for a pattern that's set to `:pfollow` another pattern, to make it trigger twice as often, half as often, etc. for half as often, patterns would have to have their own `gensym`s or IDs so that it could be kept track of whether or not to trigger the sub-pattern for each event. this ID would probably have to be associated with the pattern itself, not the pstream. could maybe be like the `number` slot but for the number of times the pattern is played, not the number of events in the pstream.

* events with arrays/lists as values should be automatically multichannel-expanded as the last step before being played, and those lists/events should be handled properly by the pattern system prior to that.

* `pmetropolis` - intellijel metropolis-inspired pattern class (maybe a mini-language for compactly representing durstutters, etc).
  * i.e., could be something like this:
  ```common-lisp
  (pmetropolis
   (pbind :instrument :acid
    :midinote (pseq '(60 59 58 57 56 55 54 53) :inf))
   5s 2h+ 2r 2o 0 3 2h- 1)
   ```
   this pattern would stutter 60 for 5 pulses, hold 59 for 2 pulses with a slide into 58 (`+` means slide), rest for 2 pulses (instead of playing 58), play 57 for 1 pulse and then rest for a pulse, skip 56 entirely (0 pulses), play 55 once and then rest 2 pulses (default step mode is "once"), skip 54 entirely (`-` means skip), play 53 for one pulse, and then loop.
  * maybe don't make it a macro so the step pattern could be a pseq, prand, etc?

* `pgatestorm` - erogenous tones gatestorm-inspired pattern class with a mini-language for writing trigger-based patterns.
