# SuperCollider Patterns

This is a listing of all the pattern classes from SuperCollider that I'd like to make equivalents of for cl-patterns. I basically just copied a list of pattern classes from SuperCollider's help files and sorted them a bit. This list is subject to change as cl-patterns is developed, since some of these will not be applicable to the way cl-patterns does things - I'll try to note those cases here, so this document can evolve into a "differences between SuperCollider and cl-patterns" type of thing.

* list patterns
  * Pseq - done as pseq
  * Pser - done as pser
  * Pshuf - done as pshuf
  * Prand - done as prand
  * Pxrand - done as pxrand
  * Pwxrand
  * Pwrand
  * Pdfsm
  * Pfpar
  * Pfsm
  * Ppar
  * Pslide - done as pslide
  * Ptuple
  * Pwalk
  * Pindex
  * Place
  * Ppatlace - done as ppatlace.
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
  * Pn - done as pn
  * PpatRewrite
  * Pplayer
  * Pprotect - see also: pbind's :cleanup key.
  * Prewrite
  * Prorate
  * Pscratch - done as pscratch
  * Pseed
  * Pset
  * Psetpre
  * Pstretch
  * Pstutter - see also: pr
  * PdurStutter
  * Psym
  * Psync - see also: pbind's :psync key
  * Ptrace - done as ptrace
  * Pwrap
* math / randomness patterns
  * Punop - done as punop
  * Pbinop - done as pbinop
  * Pnaryop - done as pnaryop
  * Paccum
  * Pbeta
  * Pbrown - done as pbrown
  * Pexprand
  * Pgbrown
  * Pcauchy
  * Pgauss
  * Pseries - done as pseries
  * Pgeom - done as pgeom
  * Phprand
  * Plprand
  * Pmeanrand
  * Ppoisson
  * Pprob
  * Pwhite - done as pwhite
* timing patterns
  * Pseg
  * Penv
  * Ptempo - see also: (pk :tempo)
  * Ptime
  * Pstep
  * Pstep2add
  * Pstep3add
  * PstepNfunc
* ugen emulation patterns (maybe defer these to Pseg or Penv or the like?)
  * PSinOsc
  * POsc
* other / unsorted patterns
  * Pbind - done as pbind
  * Pbindf
  * Pmono - done as pmono
  * PmonoArtic - see instead: pmono.
  * Pdef - WIP as pdef. see also: pbind's :name/:pdef keys.
  * Pchain - see also: pbind's :inject key.
  * Pdict
  * Penvir
  * Peventmod
  * Pif - done as pif
  * Pkey - done as pk
  * Plazy - done as plazy
  * Plambda
  * Plet
  * Pget
  * Ppatmod
  * Pproto - maybe just use a pbind key like :init instead?
  * Pvoss
  * Pfunc - done as pfunc
  * Pfuncn
  * Prout
  * Pbjorklund
