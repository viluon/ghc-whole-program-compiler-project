here are the notes

- Interpreter.hs
- peek into M -> Base.hs
- top-down skim
  - Atom
  - HeapObject
- jump to Syntax.hs for a bit, some of the (cross-project?) HLS references don't work
- trying the CLI now, `stack install` and `ext-stg-interpreter -d ghc-rts-base.fullpak` in `data/`
- recall STG: Core in ANF (functions are only applied to bindings), type stuff erased (no Λ, no coercions)
- Rts
- M
- search for "thunk"
- ext-stg-interpreter-notes



questions:
- how do weak pointers & stable pointers refer to the heap? Do stable pointers kinda work like HOAS, serving as a stable reference to a "heap object" that's not actually on the heap (so won't be moved or collected)?
  - primops to dereference weak/stable pointers
  - no coercions/conversions
  - look at corresponding primops
  - they're not actually "pointers," not indices to the heap
  - off-topic:
    - static automatic memory management, specialised GC
    - (PhD thesis)
    - synthesised garbage collector into the IR
    - example of non-tracing GC
    - tracing GC has to update the key-value store of the stable pointer
  - heap model is an int map
  - no key is reused by the GC!
  - monotonically increasing bump-allocator style
- what are marks? (`markExecuted`, `markFFI`, ...) (probs for statistics?)
  - yep, stats
  - project intended for easy extension (though not via plugins)
- is there support (or future plans for) parallelism?
  - nope
- shouldn't `readMVar#` be non-blocking (line ~850 in `Base.hs`) -- oh, I guess not, it doesn't produce a Maybe, right?
- where can I find all the primops?
  - `Stg.Interpreter.PrimOp`
- so `Syntax.hs` contains all the details of the language. What does a `Rhs'` correspond to?
  - closures (thunks, partial applications, ...)
  - constructors
  - LetNoEscape -- the binder doesn't outlive the enclosing closure
- what role do various void things play in all this? (`VoidRep` in `PrimRep` in `Syntax.hs`, `Void` in `Atom`)
  - kind parameter for type representation
  - type reps form the type system of STG
  - void is more like a unit, used as the IO token
- what are update flags?
- how are thunks represented?
  - closures + remaining args

builtinStgEval is a good place to do debug stuff (or evalExpr)

debugger: don't overengineer commands, see `Internal.hs`

ask again about trace events (traceEvent# primop, tracking what compilation phase did something come from) -- added manually by

GHC devs to the GHC source code

`libHSbase-4.14.0.0.cbits.so`
- for foreign functions
- data representation is different on Haskell vs C side
- Haskell side managed by GC
- foreign part is unmanaged
- extracting bit data from eg. `Int64` requires unboxing
- GHC generates C code during module compilation which contains FFI
- these C code bits are called stubs
- stubs compiled to object code for linking with the compiled Haskell program
- GHC-WPC collects stubs into a shared library
- Cabal packages can contain C code as well
- there's cbits (for Cabal stuff), stubs, and pure Haskell code


### moving on to dynamic analysis now
The key thing we wanted to get at the start was a laziness diff: which arguments were forced during the evaluation of a function?

how would we go about implementing that:
- special stack frame pushed on entry? how/when would we remove it?
- we could mark the "switch" to a different closure, we see the mutation of ssCurrentClosure after all. Would require adding a special stack though, right? When would we pop?


pending questions about interpreter impl:
- what's the `[Atom]` "return value" of the `M`onadic evals? (oh how I wish for type-aware search)
  - it's an unboxed tuple

new StackContinuation, the enclosure of a function's evaluation
builtinStgEval - push
evalStackContinuation - pop

questions:
  what are blackholes? (Csaba said original GHC meaning is messy, here they detect evaluation cycles. How come we can see them in the trace?)
  what are ApStacks?
  how do strictness annotations and bang patterns translate to STG?
  how are typeclass dictionaries translated?
  how do we track locations (or just function names) back to source?

figure & filter out typeclass dictionaries
application: get functions which eval all arguments
need to get precise location info for STG definitions
find "accidentally lazy" functions
filter out incidentally lazy and annotated strict

split codebases into purely library code and programs

leave other features for later (thunk depth, types, ...)

pipeline:
- GitHub/Hackage/Stackage search & fetch
- dedupe
- GHC-WPC -> fullpak
- interpret fullpaks -> traces
- analyse traces (some kind of backend for queries, at least to load them into some fRiendly format)

check out [R Markdown](https://rmarkdown.rstudio.com/) for analysis.

virtual machine @ [cloud.fit](https://cloud.fit.cvut.cz/)

R studio in haskell-laziness-analysis VM, runs in the browser


### pipeline progress
Stackage is probably the best way to go as it is ~stable and curated. They seem
to have [guidelines and tools](https://github.com/commercialhaskell/curator/) in
place to help with the build process and they compile a nightly snapshot daily
(an LTS snapshot on a weekly basis).

`janus` on IRC suggested reusing the Nixpkgs derivations for Haskell packages,
which is an alternate route. There's a [10 MB
derivation](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/hackage-packages.nix)
of Hackage packages.

Csaba seems more fond of the Stack tooling, told me about `stack unpack`, which
may come in handy (already did together with `stack init` for installing
specific versions of Alex and Happy. Very cool!). Got GHC-WPC and the external
interpreter working on the VM, hopefully VPN will be alright too (the machine
was probably offline yesterday).

VPN works! Some technical trouble building the sample programs with the
"vanilla" GHC-WPC. Agda couldn't build because of issues with the locale (how is
Debian not UTF-8 by default??), now the Stack root seems to be a mess. `texmath`
and `skylighting` won't build for a confusing reason that [may be worked
around](https://stackoverflow.com/questions/54156236/how-to-resolve-however-the-given-installed-package-instance-does-not-exist-in)
by just rebuilding everything again...

next steps:
- get the interpreter fork working
- (optional) automate building of samples
  - ask Csaba about the Stack configs for the samples (did he hand-code them?)
- collect traces
- get analysis (R studio?) working
- ask Filip about further steps


questions:
- new "static origin" change in upstream interpreter (only for call graph uses?)

### Csaba


2013: Hungarian startup Prezi
      - based on Flash
      - server side with Python & Haxe & C++
      - CTO found Csaba online, interested in functional programming
        - worked with OCaml
      - research phase, looking for something to replace Flash
      - robust new design
      - convinced CTO to use Haskell over OCaml
      - other Haskellers joined, Csaba invited them
      - goal was to generate as much code as possible
      - generate the toolkit for application developers
      - senior team wasn't familiar with QuickCheck, formal methods, etc
        - good engineers but old tech
      - used Agda to prove the correctness of collaborative editing
      - generated code from a formally verified specification
      - 3 people, then 4 + 1 newbie
      - generated C++ and JS SDKs, property-based tests
        - test toolkit could simulate multiple users simultaneously, in a
          deterministic way
      - used Haskell to generate toolkits for app devs
      - in 2 years, the whole thing was rewritten in Scala
        - Haskell maintenance was horrible
        - tooling was just shit
        - unpredictable performance
        - space leaks
      - the engineers were Haskell fans but they saw that it's unmaintainable
      - GHC community doesn't focus on the backend
        - adjusted for researchers, not engineers
        - people without industrial experience

- I like Haskell the concept
- some fanboys (me included) try to write practical software
- popularising and evangelising the technology
- hard to convince regular programmers without maintenance tooling
  - and that's not changing
- the RTS is a nightmare to work with
- even the GHC build system is horrible
  - not a regular Haskell experience
  - can't use arbitrary libraries in the compiler
  - not a Stack project, not a regular Cabal project
  - special build system
- I asked Stephen Diehl whether this (in general) happens
  - "definitely:" projects developed initially as MVPs, then rewritten using
    maintainable technology
- plan is to develop custom tooling, a new RTS
- generate as many things as possible
- 10 years ago I wanted to have the same thing, in that time nothing changed


progress on the pipeline:

So it looks like Agda needs its standard library to build anything of note. The
sample reppo doesn't include the test suite and looking at GH it seems to be
Haskell-driven. Oh boy... Fullpaks are now in `/mnt/analysis/paks/` along with a
symlink to the required cbits and I added a `--cbits-path` option to the
interpreter so that I can run it in directories without the shared library
present.

The simple app errors out with
```
ext-stg-interpreter: user error (dlsym: ./libHSbase-4.14.0.0.cbits.so: undefined symbol: print_int64)
```
we're probs missing an update to the `.so`? Pandoc also errors out, this time with
```
 * stgErrorM: "unknown variable: Binder {binderName = \"$fMonoidDList\", binderId = BinderId rGpA, binderType = SingleValue LiftedRep, binderTypeSig = \"forall a. Monoid (DList a)\", binderScope = HaskellExported, binderDetails = DFunId, binderInfo = \"\", binderDefLoc = UnhelpfulSpan \"<no location info>\", binderUnitId = UnitId {getUnitId = \"dlist-0.8.0.7-BJXMnWuS97J57VWbmW4T1I\"}, binderModule = ModuleName {getModuleName = \"Data.DList\"}, binderTopLevel = True, binderUniqueName = \"dlist-0.8.0.7-BJXMnWuS97J57VWbmW4T1I_Data.DList.$fMonoidDList\", binderUNameHash = 2193830745877576581}"
ext-stg-interpreter: uninitialized ssCurrentThreadId
CallStack (from HasCallStack):
  error, called at lib/Stg/Interpreter/Base.hs:423:29 in external-stg-interpreter-0.1.0.1-IxRgKwySTA8JAJJ5P3XVHY:Stg.Interpreter.Base
```
Agda seems to work (at least it terminates cleanly with `--help`), but isn't
very useful as mentioned above. In theory we could install actual Agda and then
build something with the local one, interpreted. Might be worth a try.

That's all there is for `stack.yaml.all`, other samples aren't "enabled." The
repo also contains `unordered-containers` (which are unsupported due to unboxed
sums), so Csaba probs keeps around things that don't work yet.

I set up the RStudio Server Pro which is now called RStudio Workbench and a user
account `viluon:viluon`. I ran Alex on the `Tokens.x` example (samples for the
sample programs are now in `/mnt/analysis/samples-for-samples/`) but the call
graph and tracing outputs are placed to the directory of the fullpak
(`/mnt/analysis/paks/`). Onto analysis we go.

RStudio Workbench installs packages in the home directory of the user. We gotta move `/home/viluon/` to somewhere in `/mnt/analysis/`. It also compiles everything sequentially when installing packages... When will people learn? (there's a `Ncpus` parameter tho)

Working in R(Studio) to get some insights into the Alex trace. So far nothing
specific, I'm new to the language so I'm just trying to figure out how to do
basic things. What I'm writing is pretty general: splitting argument diffs by
arity and parsing the diffs into new columns. Subsequent exploration will
hopefully be much faster.

We will definitely need a way to tie the insights back to the source code
though. **I should talk to Csaba about this**.


I can't seem to find a solution to what I want to do in R. Maybe I'm looking too
close.

Konrad gave me helpful warnings and information:
- beware NA semantics
- beware implicit conversion to factors
- work with tibbles, R is not good for structures
  - lots of pain to work with sth. like a list of tibbles, etc
- just add `max(arity)` columns beforehand (with `NA`'s), fill in later

```haskell
oof = join . (((.) . flip) .) . (.)
```

So parsing a column into multiple new columns in R is a great pain. I think I've
come up with a way to do it:
```r
data = diffs
for (i in 1:max(diffs$arity)) {
  colName <- paste0("arg", i)
  data    <- bind_cols(data, tibble(!! colName := rep(NA, length(diffs[[1]]))))
  data    <- apply(data, 1, function (row) {
    parsedDiff <- read_delim( I(substr(row[["arg diff"]], 1, -1))
                            , delim          = ","
                            , col_names      = FALSE
                            , lazy           = FALSE
                            , show_col_types = FALSE
                            , progress       = FALSE
                            )
    if (length(parsedDiff >= i)) {
      row[[colName]] <- parsedDiff[[i]]
    }
    row
  })
}
```
but it takes forever, so screw that. I'm modifying the output instead, which is
probably going to be much easier.

I use
```bash
awk -F'\t' -v OFS='\t' 'NF=37' alex-dyn-trace.tsv > alex-dyn-trace-valid.tsv
```
to fill in the missing

... it was a stupid idea to try in the first place
```r
maxArity <- max(diffs$arity)
for (ar in 1:maxArity) {
  frame <- diffs %>%
    filter(arity == ar)
  NAsThatShouldBeSet    <- 0
  valuesThatShouldBeNAs <- 0
  for (a in 1:30) {
    colName <- paste0("arg", a)
    if (a > ar) {
      valuesThatShouldBeNAs <- valuesThatShouldBeNAs +
        frame %>%
        select(colName) %>%
        filter(is.na(frame[[colName]])) %>%
        count()
    } else {
      NAsThatShouldBeSet <- NAsThatShouldBeSet +
        frame %>%
        select(colName) %>%
        filter(!is.na(frame[[colName]])) %>%
        count()
    }
  }
  show(paste("arity", ar))
  show(valuesThatShouldBeNAs)
  show(NAsThatShouldBeSet)
}
```

(Thursday, 19th Aug)

So far I haven't come across evidence of "rarely lazy" functions in the Alex
dataset. I'm now going to look for those specifically.

goals:
- get the Rmd into Git
  - [done](https://github.com/viluon/laziness-analysis)
- revise todos above
  - [x] get the interpreter fork working
  - [ ] (optional) automate building of samples
    - [ ] ask Csaba about the Stack configs for the samples (did he hand-code them?)
  - [ ] collect traces
  - [x] get analysis (R studio?) working
  - [ ] ask Filip about further steps


### Monday, 30th August

I'm back from "vacation." More than a week ago, on Friday, I chatted with Csaba
on Discord about the current progress. He suggested coming up with smaller
examples that I could investigate and understand fully before moving on to
larger projects (like Alex):

> my advice is to elaborate your analysis on the smallest artificial programs,
that you fully understand both in haskell and stg level. Based on the in-depth
understanding of this simple program do the analysis manually on paper (or
similar) step by step. Without this deep understanding it is impossible to
interpret the output. This will help you to ask the right questions...

(slightly edited for readability)

> You're right, I can see some general trends in the trace (such as roughly 80%
> of calls preserve laziness of at least one argument) but I should scale it up
> from smaller programs.

> ...and to select the interesting and relevant direction. The large programs
can give hints what to look for but you have to elaborate the analysis in the
smallest scale.


### Tuesday, 31st August

I'd like to understand the example presented in the [`nothunks` blog
post](https://well-typed.com/blog/2020/09/nothunks/) in terms of my dynamic
analysis. I obtained a trace from the program (with the sample input `aabbb`,
iirc), but I don't know what questions to ask. I'd like to understand the STG
source (*why?*, because Csaba told you to?), it'd be best if I could spot the
problem outlined in the blog post in the trace itself.

The gist of the problem is that a long-lived data structure contains a field
that's not as strict as it could be. The field has type `!(Map Char Stats)`
where `Stats = (Int, Int)`, meaning one cannot make it stricter in the struct
definition itself -- thunk build-up happens in the values of the map (it's a
strict map, but the pair is lazy).

However, this might be a poor example to delve into, because it's not about the
*use* of laziness, but rather its annoying pervasiveness, which leads to
performance problems. I'm writing a summary of my goals below, because I
struggle in finding direction in my work.

What I think I should find out:
- how do people use laziness to their benefit in Haskell programs?
  - this includes laziness speeding things up (avoiding unnecessary computation)
    and simplifying code
- what data structures leverage laziness and how widespread are they?

What I want to find out:
- the above, plus
- how well does GHC do at avoiding unnecessary laziness?
  - how closely do demand signatures follow the real world?
- how do people struggle with laziness in Haskell programs?
  - this includes laziness slowing things down (adding unnecessary computation)
    and complicating code, including by requiring strictness annotations

A cheap approximation of laziness speeding things up is the number (and scale?)
of thunks which are never evaluated.
pros:
- simple
- actually tells you something
cons:
- what it tells you might be the amount of work for the GC added by a stupid
  compiler, not necessarily work saved by language semantics
- ... there surely are more downsides, right?

Another approximation (or a couple of them) comes from the dynamic trace.
pros:
- pretty detailed, every closure entry is right there
- laziness diffs seem to work?
- can identify functions which sometimes force their arguments
  - truly lazy functions should be recognisable from demand signatures
cons:
- unclear as to what it really tells you -- it certainly cannot spot memory
  leaks and is **potentially a poor proxy for the amount of computation** a
  definition is responsible for (this is a hypothesis in need of an experiment)
  - possible improvement: define a notion of "thunk size/scale" or at least
    depth and log that in the trace. Beware cycles and sharing.

We also have the demand signatures themselves, and we would certainly like to
know how accurate are they. The demand analysis is conservative, so we should
see cases where it assumes little but something pretty standard is happening at
runtime.

We also have **a sadness**: typeclasses. We don't really know how do those work
(although it should be enough to ask Csaba, he ought to know). The issue seems
to be that the trace contains references to generic functions (like `++`) but we
have no clue as to the actual instance involved and whether it is known at
compile time. Is this a source location tracking problem or just our
inexperience?


a general problem is that we don't really look at data or the structure thereof.
There are things like [finger
trees](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html) which rely on
laziness heavily. How do we identify patterns where laziness is useful and
helpful (or even crucial)? The key in these patterns may be a sprinkle of
laziness here and there, rather than a load of thunks all over the place. And
like with all libraries, the fact that very little code defines (and very few
people author) these patterns doesn't really say anything about their spread
throughout the ecosystem. (Few packages use unboxed sums, but they happen to be
heavily depended upon, so unboxed sums transitively make it into a vast majority
of Hackage code.) Data structure libraries are likely optimised to use the exact
amount of laziness necessary for the task (or close to it). Rough "20% of all
calls are strict" statements aren't insightful. Is laziness-by-default the right
way to go? Would it be difficult to recreate these patterns in strict languages?
Would the performance gains in a strict setting outweigh the benefits of lazy
data structures altogether? There are far too many unknowns and too much nuance
in the knowns to give an honest answer.

In theory, a detailed trace could reveal instances of data structures
accumulating thunks only to have them forced anyway later in the program and of
these events perhaps happening periodically. It could reveal misuse of inferior
types -- like the default `String` or even `[]` -- or overly lazy structures in
an otherwise or eventually strict program. To do so, we would need something
like garbage collection, a graph traversal that would locate hidden delayed
computations. Logging their pointers could provide an approximation of something
like this, at least a hint at the lifetimes of various closures.


### Wednesday, 22nd September, meeting with Filip

- look at [the R paper](https://dl.acm.org/doi/pdf/10.1145/3360579)
- minimal cases just to check it's giving the right results
- big cases to verify it works at scale (supports a useful subset of programs)
- a notion of distance between the creation site and evaluation site of a thunk
  - in R, this was stack distance
    - do the same here, although it may turn out misleading
  - interesting to see which ones are never evaluated
  - interesting to look at bottom values (error calls, difference between
    creation and evaluation site v. important)
- libraries vs programs -- we can differentiate between the two (Cabal has
  different types of targets)
- can we get the type of a thunk when it's forced?
  - if not, at least the heap object type is useful

next steps:
- section 7, what's Haskell-relevant?
  - what do thunks yield (in terms of heap objects)?
  - how often are they accessed?
    - this subsumes used/unused thunks as well as accesses after evaluation, we
      could have a special indirection for that
  - how far are thunks forced from their creation?
    - R paper measures stack distance, **ask Csaba how to fix that**
  - how long do thunks live?
    - this is in terms of GC cycles, and that's cool!
  - do thunks force one another?
    - we know they do, but measuring how deep do those nested evaluations go or
      how "wide" they get may be (too) tricky
  -
- decide on "research questions" to ask and implement analysis for

### Thursday, 7th October

btw, `evalStackContinuation` is where the magic happens

plan:
- log the type of heap object a thunk evaluates to
  - best implemented when popping `ClosureExitMarker`
- number of evaluations of a thunk
  - upon evaluation, replace the thunk with a special closure that logs the number of accesses
  - this kind of mutation may be tricky to get right
  - counts could be logged on GC
- "distance" from allocation to evaluation

ok, to add to the above: adding an `Indirection` constructor seems to be too much work. How about we modify a `Closure` instead?
- however, there may be places which read but can't write (but that's an issue either way)

- other questions to consider:
  - do thunks force each other?
    - read about how exactly did they measure this, try to implement
  - where are thunks created?
    - R paper talks about promises being mostly generated in function arguments, I *think* this is unknowable in STG alone
    - however, do check `evalExpr` -- calls `declareBinding` when evaluating `StgLet`

### Tuesday, 12th October
Implemented checking of the result (unboxed) tuple in `evalStackContinuation`'s
`ClosureExitMarker` branch.

Compiled the changes on the VM, reran the Alex test and changed the analysis.
The results are below, the pointer type reports search through all the (unboxed)
tuple elements, i.e. they don't focus just on the single result calls.

```
98.66% of all calls produce a single value (2 353 201 / 2 385 101)
92.64% of all calls end in a pointer to a constructor (2 209 539 / 2 385 101)
0.44% of all calls end in a pointer to a thunk (10 462 / 2 385 101)
```

We can measure the "distance" from allocation to evaluation by counting the
number of `ClosureExitMarker`s between the two events.

this requires adding a field to the `Closure` variant of `HeapObject`, hence
`hoAllocTime`.
TODO/FIXME: can we evaluate a single thunk/closure multiple times?

### Tuesday, 19th October
Very interesting results from the histograms, I want to find out what the zero
lifetime closures mostly are (assuming it's mostly one kind?). Unfortunately I
need to leave for FIKS atm and won't have the time later.

### Monday, 25th October
Would you look at that:
50.37% of thunks are evaluated within 4 closure entries from the point of their
creation (222 699 / 442 085)

I'm adding better classification to the interpreter to answer the Tuesday
question.

### Tuesday, 26th October
14.26% of all heap objects are paps and thunks with lifetimes < 3 (403 040 / 2
827 194)

ggplot2's `geom_area()` seems quadratic, so I'm limiting its use to the first
10k rows.

Important: **only thunks and paps are ever entered!**

unsurprisingly, ggplot and plotly don't play as nice with each other as they
could.

### Tuesday, 2nd November
Wrote the report, hopefully final now.

(call with Csaba on Wednesday)

Questions:
- if so few functions return thunks, what creates thunks?
- do paps/thunks correspond well to their "genuine" GHC + RTS counterparts?

for Csaba:
- how are his and GHC's GC related?
  - different!
  - GC has several parameters, implemented simplest one
  - Csaba's is not generational
  - (primops behave the same)
- are closures really like those in the RTS?
    - simpler impl, goal was same behaviour, not to recreate GHC's design
      decisions
    - behaviour of lazy programs shouldn't depend much on STG impl
    - focus on trends
    - if the interpreter allocates whenever RTS STG allocates then it shouldn't matter
  - we track enters of paps and thunks, are paps reused as thunks? (some args
    supplied, none missing)
    - cannot happen in either GHC or the interpreter
    - thunk cannot be created with function application
- how are typeclasses handled? If I want to track invocations of a typeclass
  method, where do I look? (the `Id` points to the declaration site of the
  typeclass, not to the instance)
  - instances -> dictionaries, those are just constructors!
  - every Id in STG (and Core and C--) has a field `IdDetails`
  - Csaba saved these details to ext-stg
  - see `Syntax.hs`, also see GHC source
  - find the module in the Alex fullpak, the STG code has to get the method from
    the dictionary it was passed
- methodology
  - is # of closure entries a good notion of time? what about heap pointer time?
    - almost the same
    - this method is right, important difference between interpreter and GHC
      - "I need to check"
      - for convenience I may be doing extra allocation when the closure is saturated
      - TODO: find out similarities and differences
  - notion of distance? R paper used stack distance, that's not monotonic. Does
    it make sense to look for something better?
    - copy distance? how many times is this thunk copied to data structures
    - if I had this information, would it help me optimise the code?
    - stacks are somewhat related to scopes, view from the source code perspective
    - computation perspective: alloc / closure evaluation, dynamic setting

GHC-WPC:
call-graph building over summer, call graph per region
define regions with debugger
individual call graph for each region
still lossy! had call freq
new: log every closure entry, every callee and every caller
`main` is wrapped with RTS setup (signal handling, exceptions, unicode, error
reporting, ...)
more than 200 modules in every Haskell program

tsv 400 MB
+ zstd 4 MB

TODO: is Alex optimised??

2 level type theory, control over what's evaluated and what's generated

### Tuesday, 9th November
to do today:
- [ ] make sure Alex was compiled with optimisations
  - Csaba said the new GHC-WPC bundles optimisation flags in the archive
  - make sure to pull that!
- [ ] log the number of allocated heap objects by type
  - perhaps best to add a new entry variant with the allocation
  - maybe we can just replace the update frame thing, it's not used anyway

Since we'd like to use the new GHC-WPC, I'm merging the upstream to
viluon/dynamic-tracing (the submodule has been updated). I'll recompile it on
the VM afterwards, then move on to recompiling Alex with optimisations, setting
up the new fullpak, and tracing it again. While compiling, I can work with
RStudio to analyse a new trace of the existing fullpak (with an unknown
optimisation level).

#### Dynamic dependency issues

(2pm) GHC-WPC is compiling now, but I can't run the external-stg-interpreter
because of a dynamic linking error:
```
ext-stg-interpreter: user error (dlopen: /lib/x86_64-linux-gnu/libm.so.6: version `GLIBC_2.29' not found (required by /mnt/analysis/paks/libHSbase-4.14.0.0.cbits.so))
```

This is probably because I upgraded the interpreter separately (before starting
Hadrian for GHC-WPC) and it now requires an updated `libHSbase`... The
interpreter is being run from `/mnt/analysis/paks` and it's reading the symlink
```
libHSbase-4.14.0.0.cbits.so -> /mnt/analysis/ghc-whole-program-compiler-project/external-stg-interpreter/data/libHSbase-4.14.0.0.cbits.so*
```

which *should* point to the up-to-date `libHSbase`. So maybe the dependencies of
that library changed?
```bash
/mnt/analysis# ldd paks/libHSbase-4.14.0.0.cbits.so
paks/libHSbase-4.14.0.0.cbits.so: /lib/x86_64-linux-gnu/libm.so.6: version `GLIBC_2.29' not found (required by paks/libHSbase-4.14.0.0.cbits.so)
        linux-vdso.so.1 (0x00007ffd20d6d000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f6437919000)
        libgmp.so.10 => /lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f6437896000)
        libtinfo.so.6 => /lib/x86_64-linux-gnu/libtinfo.so.6 (0x00007f6437868000)
        libGL.so.1 => /lib/x86_64-linux-gnu/libGL.so.1 (0x00007f64377d4000)
        libX11.so.6 => /lib/x86_64-linux-gnu/libX11.so.6 (0x00007f6437693000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f6437672000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f64374af000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f6437b94000)
        libGLX.so.0 => /lib/x86_64-linux-gnu/libGLX.so.0 (0x00007f643747b000)
        libGLdispatch.so.0 => /lib/x86_64-linux-gnu/libGLdispatch.so.0 (0x00007f64373be000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f64373b9000)
        libxcb.so.1 => /lib/x86_64-linux-gnu/libxcb.so.1 (0x00007f643738f000)
        libXext.so.6 => /lib/x86_64-linux-gnu/libXext.so.6 (0x00007f643717d000)
        libXau.so.6 => /lib/x86_64-linux-gnu/libXau.so.6 (0x00007f6436f77000)
        libXdmcp.so.6 => /lib/x86_64-linux-gnu/libXdmcp.so.6 (0x00007f6436d71000)
        libbsd.so.0 => /lib/x86_64-linux-gnu/libbsd.so.0 (0x00007f6436d57000)
        librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f6436d4d000)
```

Therein lies the problem: we're missing `libm` which the cbits rely on. It
should be enough to recompile the cbits library on the Debian VM (via the
`c`ompile script).

Unfortunately, the cbits (or at least the compilation process) depend on other
things too:
```
/usr/bin/ld: cannot find -lXi
/usr/bin/ld: cannot find -lXrandr
/usr/bin/ld: cannot find -lXcursor
/usr/bin/ld: cannot find -lXinerama
collect2: error: ld returned 1 exit status
```

These require
```bash
apt-get install libxinerama-dev libxcursor-dev libxrandr-dev libxi-dev
```

And there we go! The external interpreter is again in full working order!

#### Tracing issues

... that is, until it crashes with an error.
```bash
/mnt/analysis/samples-for-samples/alex/vanilla-examples# ../alex.sh Tokens.x
ext-stg-interpreter: uninitialized ssCurrentThreadId
CallStack (from HasCallStack):
  error, called at lib/Stg/Interpreter/Base.hs:445:29 in external-stg-interpreter-0.1.0.1-IxRgKwySTA8JAJJ5P3XVHY:Stg.Interpreter.Base
```

What's worrisome is that the line number in that error message is definitely
wrong, at the current commit (`acecd3a6e3197c1e0e9d7f2d0aac879589d2d3f2`)
checked out both locally and on the VM, that line is empty. It should be
reporting line 483, where `error` is actually called. There's also no stack
trace. Oh well.

The issue probably is allocation of top-level closures (CAFs? maybe other
things?). It makes sense that it would happen before a thread is even spawned.
Unfortunately, there's no way to check whether that field has been initialized or not, and making it a `Maybe` is... uh...

#### GHC-WPC compilation issues

Meanwhile, the compilation of GHC-WPC failed with
```
Invalid option `--info=/tmp/ghc27724_0/ghc_9.p_o_info'

Usage: mkmodpak --modpakname FILENAME --stgbin FILENAME --ghcstg FILENAME
                --cmm FILENAME --asm FILENAME [--hssrc FILENAME]
                [--ghccore FILENAME]
`mkmodpak' failed in phase `create .modpak'. (Exit code: 1)
Error when running Shake build system:
  at action, called at src/Rules.hs:71:19 in main:Rules
  at need, called at src/Rules.hs:93:5 in main:Rules
* Depends on: _build/stage1/lib/package.conf.d/ghc-prim-0.6.1.conf
  at need, called at src/Rules/Register.hs:117:5 in main:Rules.Register
* Depends on: _build/stage1/libraries/ghc-prim/build/libHSghc-prim-0.6.1_p.a
  at need, called at src/Rules/Library.hs:214:5 in main:Rules.Library
* Depends on: _build/stage1/libraries/ghc-prim/build/GHC/Types.p_o
* Depends on: _build/stage1/libraries/ghc-prim/build/GHC/Types.p_o _build/stage1/libraries/ghc-prim/build/GHC/Types.p_hi
  at cmd', called at src/Builder.hs:291:23 in main:Builder
  at cmd, called at src/Builder.hs:376:8 in main:Builder
* Raised the exception:
Development.Shake.cmd, system command failed
Command line: _build/stage0/bin/ghc -Wall -hisuf p_hi -osuf p_o -hcsuf p_hc -static -prof -eventlog -hide-all-packages -no-user-package-db '-package-db _build/stage1/lib/package.conf.d' '-this-unit-id ghc-prim-0.6.1' '-package-id rts-1.0' -i -i/mnt/analysis/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/libraries/ghc-prim/build -i/mnt/analysis/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/libraries/ghc-prim/build/autogen -i/mnt/analysis/ghc-whole-program-compiler-project/ghc-wpc/libraries/ghc-prim -Iincludes -I_build/stage1/lib -I_build/stage1/libraries/ghc-prim/build -I/mnt/analysis/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/lib/x86_64-linux-ghc-8.11.0.20210923/rts-1.0/include -I_build/stage1/lib -optc-I_build/stage1/lib -optP-include -optP_build/stage1/libraries/ghc-prim/build/autogen/cabal_macros.h -outputdir _build/stage1/libraries/ghc-prim/build -Wnoncanonical-monad-instances -optc-Wno-error=inline -c libraries/ghc-prim/GHC/Types.hs -o _build/stage1/libraries/ghc-prim/build/GHC/Types.p_o -O2 -H32m -this-unit-id ghc-prim -XHaskell2010 -ghcversion-file=/mnt/analysis/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/lib/ghcversion.h -haddock -Wno-deprecated-flags -Wno-trustworthy-safe
Exit code: 1
Stderr and Stdout:
Invalid option `--info=/tmp/ghc27724_0/ghc_9.p_o_info'

Usage: mkmodpak --modpakname FILENAME --stgbin FILENAME --ghcstg FILENAME
                --cmm FILENAME --asm FILENAME [--hssrc FILENAME]
                [--ghccore FILENAME]
`mkmodpak' failed in phase `create .modpak'. (Exit code: 1)
```

Am I -- am I trying to compile GHC-WPC using GHC-WPC?
**NOPE!**

I asked Csaba, he pointed out that the required steps are documented in [the
Usage section of the
repository](https://github.com/grin-compiler/ghc-whole-program-compiler-project#usage).
I need to reinstall `mod-pak`.

#### Preliminary results
Assuming no thunk is entered (at least) twice, 92.31% of allocated thunks are
entered (442 085 / 478 899).

#### New GHC-WPC + new tracing
Aaaand we're running out of memory. There's a weird bug with the interpreter
now, it seems to be stuck in a loop.

```
write gc facts to: /mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts
Souffle.writeFiles done
loading: "/mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts/Live.csv"
freed after GC:
  ssHeap                old: 3000002     new: 2872203     diff: 127799      dead:  4.26%
  ssWeakPointers        old: 4           new: 3           diff: 1           dead: 25.00%
  ssMVars               old: 4           new: 2           diff: 2           dead: 50.00%
  ssMutVars             old: 35          new: 25          diff: 10          dead: 28.57%
  ssArrays              old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrays       old: 7           new: 0           diff: 7           dead: 100.00%
  ssSmallArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssSmallMutableArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssArrayArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrayArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableByteArrays   old: 24          new: 21          diff: 3           dead: 12.50%
  ssStableNameMap       old: 0           new: 0           diff: 0           dead:  0.00%
live threads: 1       all threads: 1
resource address counters:
  ssNextHeapAddr           3000002
  ssNextStableName         0
  ssNextWeakPointer        4
  ssNextStablePointer      1
  ssNextMutableByteArray   24
  ssNextMVar               4
  ssNextMutVar             35
  ssNextArray              0
  ssNextMutableArray       7
  ssNextSmallArray         0
  ssNextSmallMutableArray  0
  ssNextArrayArray         0
  ssNextMutableArrayArray  0
heap start:       55946
static heap size: 55945
dyn heap size:    71853
write gc facts to: /mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts
Souffle.writeFiles done
loading: "/mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts/Live.csv"
freed after GC:
  ssHeap                old: 3127800     new: 2979697     diff: 148103      dead:  4.74%
  ssWeakPointers        old: 4           new: 3           diff: 1           dead: 25.00%
  ssMVars               old: 2           new: 0           diff: 2           dead: 100.00%
  ssMutVars             old: 10          new: 0           diff: 10          dead: 100.00%
  ssArrays              old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrays       old: 7           new: 0           diff: 7           dead: 100.00%
  ssSmallArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssSmallMutableArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssArrayArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrayArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableByteArrays   old: 3           new: 0           diff: 3           dead: 100.00%
  ssStableNameMap       old: 0           new: 0           diff: 0           dead:  0.00%
live threads: 1       all threads: 1
resource address counters:
  ssNextHeapAddr           6000003
  ssNextStableName         0
  ssNextWeakPointer        4
  ssNextStablePointer      1
  ssNextMutableByteArray   24
  ssNextMVar               4
  ssNextMutVar             35
  ssNextArray              0
  ssNextMutableArray       7
  ssNextSmallArray         0
  ssNextSmallMutableArray  0
  ssNextArrayArray         0
  ssNextMutableArrayArray  0
heap start:       55946
static heap size: 55945
dyn heap size:    92157
write gc facts to: /mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts
Souffle.writeFiles done
loading: "/mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts/Live.csv"
freed after GC:
  ssHeap                old: 3148106     new: 3024959     diff: 123147      dead:  3.91%
  ssWeakPointers        old: 4           new: 3           diff: 1           dead: 25.00%
  ssMVars               old: 2           new: 0           diff: 2           dead: 100.00%
  ssMutVars             old: 10          new: 0           diff: 10          dead: 100.00%
  ssArrays              old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrays       old: 8           new: 0           diff: 8           dead: 100.00%
  ssSmallArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssSmallMutableArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssArrayArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrayArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableByteArrays   old: 8           new: 0           diff: 8           dead: 100.00%
  ssStableNameMap       old: 0           new: 0           diff: 0           dead:  0.00%
live threads: 1       all threads: 1
resource address counters:
  ssNextHeapAddr           9000006
  ssNextStableName         0
  ssNextWeakPointer        4
  ssNextStablePointer      1
  ssNextMutableByteArray   29
  ssNextMVar               4
  ssNextMutVar             35
  ssNextArray              0
  ssNextMutableArray       8
  ssNextSmallArray         0
  ssNextSmallMutableArray  0
  ssNextArrayArray         0
  ssNextMutableArrayArray  0
heap start:       55946
static heap size: 55945
dyn heap size:    67201
../alex.sh: line 5:  2731 Killed                  ext-stg-interpreter /mnt/analysis/paks/alex.fullpak --cbits-path /mnt/analysis/paks/libHSbase-4.14.0.0.cbits.so --args "$@"
```

It appears to save the GC stats thrice, before getting terminated by the OOM
killer (I suppose, it was using ~27 GB of RAM(!!) when I last checked during its
runtime). That's work for next time though.

### Tuesday, 16th November
Still not sure what's going on with `ssNextHeapAddr` reaching 3x its usual
value. Interestingly enough, the example does finish (within ~30 GB of memory),
so it's not stuck in a loop. I compiled the interpreter with profiling and I'm
running it now to see what takes up so much memory. The execution is very slow,
however, it took 5:27 without profiling but with it enabled it's been running
for over half an hour and it hasn't even gotten to the first GC yet.

Hmm, I don't see anything from last week verifying that Alex was compiled with
oxygen, but I *think* that was the case since the opt flags are set in the Stack
YAML for the samples project. We have an up-to-date GHC-WPC and a fullpak from
it, but the oxygen suggests previous results came from meaningful data.

### Tuesday, 23rd November
Talked to prof. Jan about the memory issue after merging upstream changes.

The plan:
- compile (*and install*) an older commit of the interpreter
- run the Alex example using `time`, note the memory usage
  - if suspiciously high, recompile with profiling and run with heap tracing
  - in fact, we probably want the above for reference regardless
- compile the new commit, run using `time`, note the memory usage
  - actually, invocation using `time` is now a part of `alex.sh`
- go through Csaba's commits, identify the change that led to memory blow up
- fix the regression


Installed an older commit, `0b317d0`. Note that the fullpak is still the one
from the *new* GHC-WPC, if the changes are due to the compiler, we should again
see memory blowup.

Nevermind, "uninitialized `ssCurrentThreadId`" (a bug I fixed). Let's go to
`f6e8a7b`.

Obviously, I was accidentally running it with heap profiling on...

`[16:45]` I've been running a `+RTS -h -p` interpreter for more than two hours
now. It passed the first GC:
```
write gc facts to: /mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts
Souffle.writeFiles done
loading: "/mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts/Live.csv"
freed after GC:
  ssHeap                old: 3000001     new: 2874767     diff: 125234      dead:  4.17%
  ssWeakPointers        old: 4           new: 3           diff: 1           dead: 25.00%
  ssMVars               old: 4           new: 2           diff: 2           dead: 50.00%
  ssMutVars             old: 35          new: 25          diff: 10          dead: 28.57%
  ssArrays              old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrays       old: 7           new: 0           diff: 7           dead: 100.00%
  ssSmallArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssSmallMutableArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssArrayArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrayArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableByteArrays   old: 24          new: 21          diff: 3           dead: 12.50%
  ssStableNameMap       old: 0           new: 0           diff: 0           dead:  0.00%
live threads: 1       all threads: 1
resource address counters:
  ssNextHeapAddr           3000001
  ssNextStableName         0
  ssNextWeakPointer        4
  ssNextStablePointer      1
  ssNextMutableByteArray   24
  ssNextMVar               4
  ssNextMutVar             35
  ssNextArray              0
  ssNextMutableArray       7
  ssNextSmallArray         0
  ssNextSmallMutableArray  0
  ssNextArrayArray         0
  ssNextMutableArrayArray  0
heap start:       55298
static heap size: 55297
dyn heap size:    69936
```

Keep in mind this is the old commit and it *should* (in theory) finish rather
"quickly," it certainly shouldn't do the 3x GC dance. Regardless, I need to find
out what the culprits of allocation and timing slowdowns are. If there's
low-hanging fruit I can pick to speed it up significantly, there shouldn't be
that high a cost to further experiments.

Right now, I've no clue whether it's the GC causing the slowdown (with the
program itself mostly "done") or whether it actually runs this slowly. Guessing
from the GC report during continued execution, whatever Alex used to do before
these bugs arrived isn't really related to what it's doing now after the GC
cycle is done.

YES! It didn't do the 3x dance and now we have `.prof` and `.hp` files! It only
took 2:45 hours!

... wow. The heap profile ranges over 3 out of the 10 (max RSS) GB and about 4
minutes out of the 2 hours 45 minutes the interpreter ran for.

### Wednesday, 24th November

@mpickering helped me figure out the profiling trouble. The reported time isn't
wall clock time, as it doesn't include the (huge!) profiling overhead. Also,
logging to eventlog and using `eventlog2html` is faster and feature-richer.
`../alex.sh Tokens.x +RTS -l-aug -i5` finished in 5 minutes! The eventlog should
hopefully include all the details (including full cost centre paths), but I
haven't looked at it yet.

### Tuesday, 30th November

If only stuff worked out of the box. `eventlog2html` crashes with `hGetContents:
invalid argument (invalid byte sequence)`.

My bad, it was the `-p` option which didn't do what I thought it did.

Rerunning the profile with `-hy -l-aug -i3` for extra detail.

```
write gc facts to: /mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts
Souffle.writeFiles done
loading: "/mnt/analysis/samples-for-samples/alex/vanilla-examples/.gc-datalog-facts/Live.csv"
freed after GC:
  ssHeap                old: 3000001     new: 2874767     diff: 125234      dead:  4.17%
  ssWeakPointers        old: 4           new: 3           diff: 1           dead: 25.00%
  ssMVars               old: 4           new: 2           diff: 2           dead: 50.00%
  ssMutVars             old: 35          new: 25          diff: 10          dead: 28.57%
  ssArrays              old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrays       old: 7           new: 0           diff: 7           dead: 100.00%
  ssSmallArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssSmallMutableArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssArrayArrays         old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableArrayArrays  old: 0           new: 0           diff: 0           dead:  0.00%
  ssMutableByteArrays   old: 24          new: 21          diff: 3           dead: 12.50%
  ssStableNameMap       old: 0           new: 0           diff: 0           dead:  0.00%
live threads: 1       all threads: 1
resource address counters:
  ssNextHeapAddr           3000001
  ssNextStableName         0
  ssNextWeakPointer        4
  ssNextStablePointer      1
  ssNextMutableByteArray   24
  ssNextMVar               4
  ssNextMutVar             35
  ssNextArray              0
  ssNextMutableArray       7
  ssNextSmallArray         0
  ssNextSmallMutableArray  0
  ssNextArrayArray         0
  ssNextMutableArrayArray  0
heap start:       55298
static heap size: 55297
dyn heap size:    69936
ssTotalLNECount: 200470
ssClosureCallCounter: 2827194
executed closure id count: 1303
call graph size: 2091
        Command being timed: "ext-stg-interpreter /mnt/analysis/paks/alex.fullpak --cbits-path /mnt/analysis/paks/libHSbase-4.14.0.0.cbits.so --args Tokens.x +RTS -hy -l-aug -i3"
        User time (seconds): 968.03
        System time (seconds): 27.88
        Percent of CPU this job got: 101%
        Elapsed (wall clock) time (h:mm:ss or m:ss): 16:18.42
        Average shared text size (kbytes): 0
        Average unshared data size (kbytes): 0
        Average stack size (kbytes): 0
        Average total size (kbytes): 0
        Maximum resident set size (kbytes): 10701460
        Average resident set size (kbytes): 0
        Major (requiring I/O) page faults: 0
        Minor (reclaiming a frame) page faults: 2764239
        Voluntary context switches: 973558
        Involuntary context switches: 1764
        Swaps: 0
        File system inputs: 0
        File system outputs: 1895752
        Socket messages sent: 0
        Socket messages received: 0
        Signals delivered: 0
        Page size (bytes): 4096
        Exit status: 0
```

Let's have a look.
Ok, a lot of allocations are of type `[]` (up to 1 GB live) and up to 0.3 GB
live are of type `DynTraceEntry`. The heap size is often more than double the
count of live bytes...

We should get a per-module overview, then try a simple eventlog profile of the
up-to-date version to see the delta. We could try retainer profiling as well.

Now switching to the new commit after yet-another and by-module. The module
breakdown doesn't seem very interesting, everything important happens either in
`Interpreter` or in `Interpreter.Base`, there's not enough detail to produce
useful conclusions. Ette compillere...

Ran into
```
ext-stg-interpreter: user error (dlopen: /lib/x86_64-linux-gnu/libm.so.6: version `GLIBC_2.29' not found (required by /mnt/analysis/paks/libHSbase-4.14.0.0.cbits.so))
```

again, recompiled `mod-pak` without realising that it's probably not necessary;
what I need is an up-to-date `.cbits.so`. The script is
`external-stg-interpreter/data/cbits.so-script/c`.

The script produced the right cbits, but the symlink in `paks/` was wrong (I guess I copied the file over. Hmm, actually it's probably in Git). Fixed(?).

**Eventlogs**:

- yet-another:     `-hy -l-aug -i3`, old commit (`f6e8a7b`)
- by-module:       `-hm -l-aug -i3`, old commit (`f6e8a7b`)
- 18eabb0-by-type: `-hy -l-aug -i3`, new commit (`18eabb0`)

Argh, magnificently weird. It doesn't do the 3x GC dance. No clue why.
Regardless, we wanna optimise this now, replacing the 4 GB of linked list
overhead with at most a gig of ~~vectors~~ `Seq`s.


### Wednesday, 8th December
todo: heap profile by cost centre to figure out where are all the linked lists
coming from (`-hc`, also `-hd` later for identifying constructors).

Ah, there's `-L <num>` for setting the max cost-centre stack name length in the
heap profile... Not sure if it's necessary with eventlog tho. (Doesn't look like
it is.)

### Tuesday, 14th December
We have `seq-rnf-by-cost-centre.eventlog.html`, which is `-hc -l-aug -i3`.

chat with Filip:
- run multiple libraries
  - monitor peak mem usage
- more datasets
  - one project for analysis testing
  - pilot experiment (~5 projects) (pipeline testing)
  - main corpus
    - run a larger experiment
- Makefile + GNU parallel
  - (ongoing switch to R Targets)
  - dump to CSV, import merged file to RStudio


todo: `ssCurrentClosure` wastes space by turning a bytestring (from the binder)
to a `[Char]` via show


Switched to `Data.Text` in the tracing entries, which should hopefully reduce
memory usage. Filip says we can just add more RAM to the VM and that the next
step should be collecting data for a range of examples. The easiest way to do
that seems to be running the interpreter on many examples in parallel,
collecting the traces into a single CSV and loading that in RStudio.

I just noticed that the call counts in `alex-tokensx.Rmd` are suspiciously high.
We used to have
```
98.66% of all calls produce a single value (2 353 201 / 2 385 101)
92.64% of all calls end in a pointer to a constructor (2 209 539 / 2 385 101)
0.44% of all calls end in a pointer to a thunk (10 462 / 2 385 101)
```

but now we've got
```
[1] "99.68% of all calls produce a single value (5 534 588 / 5 552 280)"
[1] "80.08% of all calls end in a pointer to a constructor (4 446 423 / 5 552 280)"
[1] "0.38% of all calls end in a pointer to a thunk (21 244 / 5 552 280)"
```

Something's probably wrong with how we select argument diffs from the dataset.
Also, the `arg1` column is all NAs. It looks like the TSV is shifted!

Yep, it was, but I don't think that explains the difference in call counts. The
last `alex-dyn-trace-valid.tsv` is from Nov 16, let's get a new one.

Ran the updated interpreter with oxygen (without profiling), this is the
`/usr/bin/time` output:
```
Command being timed: "ext-stg-interpreter /mnt/analysis/paks/alex.fullpak --cbits-path /mnt/analysis/paks/libHSbase-4.14.0.0.cbits.so --args Tokens.x"
User time (seconds): 198.96
System time (seconds): 15.69
Percent of CPU this job got: 100%
Elapsed (wall clock) time (h:mm:ss or m:ss): 3:34.26
Average shared text size (kbytes): 0
Average unshared data size (kbytes): 0
Average stack size (kbytes): 0
Average total size (kbytes): 0
Maximum resident set size (kbytes): 8438472
Average resident set size (kbytes): 0
Major (requiring I/O) page faults: 15
Minor (reclaiming a frame) page faults: 2108227
Voluntary context switches: 21555
Involuntary context switches: 1228
Swaps: 0
File system inputs: 44976
File system outputs: 1777944
Socket messages sent: 0
Socket messages received: 0
Signals delivered: 0
Page size (bytes): 4096
Exit status: 0
```

That's actually not much of an improvement in terms of memory usage considering
profiling is off :(

Regarding the weird reports, the issue was probably the weird 3x dance that led
to abnormally large traces (as the old `alex-dyn-trace-valid.tsv`'s 1.6 GB size
suggests). We're back on track with a regular 641 MB TSV.

Took a Btrfs snapshot (`btrfs subvolume snapshot -r /mnt/analysis
before-ram-increase`), Peta arranged that the VM will get extra RAM tonight.

The interpreter now logs the program name and command line arguments to the
trace (unfortunately, it's part of every row...). It also names the trace after
a base64'd hash of the arguments (but a hash from `Data.Hashable`, and a base64
of its `show`... yeah...). So we can run multiple alexes in parallel (the GC
facts will be overridden, but who cares).

**TODO**: obviously pass a proper product type or at least a map from `Main` to
the interpreter so we can add cmdline arguments easily. Then add one for the
trace filename so that Make has control over that, and we don't have to do
stupid shit with base64. It would be great if we stopped placing the trace file
to where the fullpak is, so that we stop cluttering `/mnt/analysis/paks`.

Actually, I'm afraid the GC needs to read the facts folder, it's not just an
informational dump. So we'll need to give the folder a UUID as well. (That
shouldn't affect analysis though, right? well it shouldn't unless the GC
crashes).

### Wednesday, 9th February

I'm about to quit my internship at PRL-PRG to start another internship at Intel
next month. Here are some thoughts on my time spent at the lab, what went well,
and what could have gone better.

- the lab is full of wonderful, good-humoured people
- I felt genuinely honoured to spend time with them
- the sense of camaraderie, mutual support and respect is very welcoming

takeaways:
- the way group meetings are run is genius
  - it's a great way to update others on individual progress in such a large and
    diverse group
  - you get to ask others about topics that really interest you, so whoever's
    speaking feels like at least one person is definitely listening
    - this kind of fixes the impersonal nature of online group calls
  - the notes are amazing -- missing the meeting isn't an obstacle to catching
    up with a colleague's progress
    - thanks to all contributors for writing them down!
- sadly I don't have good personal experience with the reports but I think they
  might work well
- feedback is crucial (I should have sought it early and often)
