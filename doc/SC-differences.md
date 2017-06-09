# Differences between cl-patterns and SuperCollider patterns

This document is a WIP.

## New features

* event values representing the same things are automatically converted for you
  * i.e. amp/db, dur/delta, sustain/legato, etc
* pbind has special keys which alter the functionality of it
* all pstreams (pattern streams) keep a history of previous values that can be referred back to at any time using `pstream-nth`
* pwrand - weights are automatically normalized (FIX)
* pdurstutter - works on event streams as well as number streams.
