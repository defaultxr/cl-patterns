# SuperCollider Patterns

This is a listing of some of the pattern classes from SuperCollider and a few of the quarks I had installed. I'm planning on making equivalents of many of these for cl-patterns, but as I go through this list I might skip some of the more esoteric or less useful ones, since I didn't really go through them too in-depth when I copied the list from SC.

This list basically is a quick overview of the implementation status of various pattern classes from SuperCollider, so if you're familiar with SC then hopefully this will be helpful. For more in-depth information, I recommend reading the "SC-differences.md" file instead, which will give more details about the ways cl-patterns differs from SuperCollider.

* list patterns
  * `Pseq` - done as `pseq`.
  * `Pser` - done as `pser`.
  * `Pshuf` - done as `pshuf`.
  * `Prand` - done as `prand`.
  * `Pxrand` - done as `pxrand`.
  * `Pwxrand`
  * `Pwrand`
  * `Pdfsm`
  * `Pfpar`
  * `Pfsm`
  * `Ppar`
  * `Pslide` - done as `pslide`.
  * `Ptuple`
  * `Pwalk`
  * `Pindex`
  * `Place`
  * `Ppatlace` - done as `ppatlace`.
  * `Pswitch`
* filter patterns
  * `Pavaroh`
  * `Pbus`
  * `Pclump`
  * `Pclutch`
  * `Pconst`
  * `Pdelay`
  * `Pdelta`
  * `Pdiff`
  * `Pdrop`
  * `PfadeIn`
  * `Pfin` - done as `pfin`. see also: `pbind`'s `:pfin` key.
  * `PfinQuant`
  * `Pfindur` - done as `pfindur`. see also: `pbind`'s `:pfindur` key.
  * `Pfx`
  * `Pinterp`
  * `Plag`
  * `Pn` - done as `pn`.
  * `PpatRewrite`
  * `Pplayer`
  * `Pprotect` - not implemented yet; in the future you will also be able to use `pbind`'s `:cleanup` key. (FIX)
  * `Prewrite`
  * `Prorate`
  * `Pscratch` - done as `pscratch`.
  * `Pseed`
  * `Pset`
  * `Psetpre`
  * `Pstretch`
  * `Pstutter` - done as `pstutter`. see also: `pr`
  * `PdurStutter` - done as `pdurstutter`. see also: `pbind`'s `:pdurstutter` key.
  * `Psym`
  * `Psync` - see also: `pbind`'s `:psync` key.
  * `Ptrace` - done as `ptrace`.
  * `Pwrap` - see also: `(pnary #'wrap)`
* math / randomness patterns
  * `Punop` - done as `punop`.
  * `Pbinop` - done as `pbinop`.
  * `Pnaryop` - done as `pnaryop`.
  * `Paccum`
  * `Pbeta`
  * `Pbrown` - done as `pbrown`.
  * `Pexprand`
  * `Pgbrown`
  * `Pcauchy`
  * `Pgauss`
  * `Pseries` - done as `pseries`.
  * `Pgeom` - done as `pgeom`.
  * `Phprand`
  * `Plprand`
  * `Pmeanrand`
  * `Ppoisson`
  * `Pprob`
  * `Pwhite` - done as `pwhite`.
* timing patterns
  * `Pseg`
  * `Penv`
  * `Ptempo` - see also: `(pk :tempo)`
  * `Ptime` - done as `pbeats`. `ptime` in cl-patterns returns seconds, not beats.
  * `Pstep`
* ugen emulation patterns (maybe defer these to Pseg or Penv or the like?)
  * `PSinOsc`
  * `POsc`
* other / unsorted patterns
  * `Pbind` - done as `pbind`.
  * `Pbindf`
  * `Pmono` - done as `pmono`.
  * `PmonoArtic` - `pmono` covers this functionality.
  * `Pdef` - done as `pdef`. see also: `pbind`'s `:name`/`:pdef` keys.
  * `Pchain` - see also: `pbind`'s `:inject` key.
  * `Pdict`
  * `Penvir`
  * `Peventmod`
  * `Pif` - done as `pif`.
  * `Pkey` - done as `pk`.
  * `Plazy` - done as `plazy`.
  * `Plambda`
  * `Plet`
  * `Pget`
  * `Ppatmod`
  * `Pproto` - maybe just use a `pbind` key like `:init` instead?
  * `Pvoss`
  * `Pfunc` - done as `pfunc`.
  * `Pfuncn`
  * `Prout`
  * `Pbjorklund`
