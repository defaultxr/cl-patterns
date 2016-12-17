# SuperCollider Patterns

This is a listing of all the pattern classes from SuperCollider that I'd like to make equivalents of for cl-patterns. I basically just copied a list of pattern classes from SuperCollider's help files and sorted them a bit. This list is subject to change as cl-patterns is developed, since some of these will not be applicable to the way cl-patterns does things - I'll try to note those cases here, so this document can evolve into a "differences between SuperCollider and cl-patterns" type of thing.

* list patterns
  * Pseq - done as pseq
  * Prand - done as prand
  * Pxrand - done as pxrand
  * Pshuf
  * Pwrand
  * Pdfsm
  * Pfpar
  * Pfsm
  * Ppar
  * Pslide
  * Ptuple
  * Pwalk
  * Pser
  * Pindex
  * Place
  * Ppatlace
  * Pswitch
* filter patterns
  * Pavaroh
  * Pbus
  * Pclump
  * Pclutch
  * Pconst
  * Pdelay
  * Pdelta
  * Pdiff
  * Pdrop
  * PfadeIn
  * Pfin - see also: pbind's :pfin key.
  * PfinQuant
  * Pfindur - see also: pbind's :pfindur key.
  * Pfx
  * Pinterp
  * Plag
  * Plambda
  * Pn - done as pn
  * PpatRewrite
  * Pplayer
  * Pprotect - see also: pbind's :cleanup key.
  * Prewrite
  * Prorate
  * Pscratch
  * Pseed
  * Pset
  * Psetpre
  * Pstretch
  * Pstutter - done as pr
  * PdurStutter
  * Psym
  * Psync - see also: pbind's :psync key
  * Ptrace
  * Pwrap
* math / randomness patterns
  * Pbinop
  * Pnaryop
  * Punop
  * Paccum
  * Pbeta
  * Pbrown
  * Pexprand
  * Pgbrown
  * Pcauchy
  * Pgauss
  * Pseries
  * Pgeom
  * Phprand
  * Plprand
  * Pmeanrand
  * Ppoisson
  * Pprob
  * Pwhite
* timing patterns
  * Pseg
  * Ptempo
  * Ptime
  * Pstep
  * Pstep2add
  * Pstep3add
  * PstepNfunc
* other / unsorted patterns
  * Pbind - done as pbind
  * Pbindf
  * Pmono
  * Pdef - WIP as pdef. see also: pbind's :name/:pdef keys.
  * Pchain - see also: pbind's :inject key.
  * Pdict
  * Penvir
  * Peventmod
  * Pif
  * Pkey - done as pk
  * Plazy - done as plazy
  * Plet
  * Ppatmod
  * Pproto - maybe just use a pbind key like :init instead?
  * Pvoss
  * Pfunc - done as pfunc
  * Pfuncn
  * Prout
