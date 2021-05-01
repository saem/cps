# these modules are used extensively
import std/macros
import std/mersenne
import std/options # need this for counter examples

# these modules have limited use, so be selective
from std/math import floor, log10
from std/strformat import fmt
from std/strutils import join
from std/sugar import `=>` # XXX: maybe a bust because inference can't keep up
from std/algorithm import sort
from std/sequtils import toSeq, apply

# XXX: Once this is mature enough (repeatability, shrinking, and API) move out
#      of experimental.

## :Author: Saem Ghani
##
## Current Development:
## ====================
## This module implements property based testing facilities. Like most nice
## things there are two major parts of this module:
## 1. core concepts of structured data generation, operations, & property tests
## 2. the API by which the former is expressed
## It's important not to conflate the two, the current way tests are executed
## and reported upon is not relevant to the core as an example of this. Nor is
## the composition of arbitraries, predicates, and random number generators.
##
## API Evolution:
## --------------
## The API needs evolving, in a few areas:
## 1. definition of generators
## 2. expression of properties
## 
## Generators: at the very least `chain` or `fmap` is likely required along
## with a number of other combinators that allow for rapid definition of
## generators. This is likely the most important on the road to being able to
## generate AST allowing for rapidly testing languages features working in
## combinations with each other. This should allow for exacting documentation
## of a spec.
##
## Properties: something that provides some simple combinators that allow for:
## `"some property is true" given arb1 & arb2 & arb3 when somePredicate(a,b,c)`
## Where we have a nice textual description of a spec, declaration of
## assumptions, and the predicate. More complex expressions such as given a
## circumstance (textual + given), many properties predicates can be checked
## including introducing further givens specific to the branch of properties.
## To provide a cleaner API in most circumstances, an API that can take a
## subset of `typedesc`s with associated default arbitraries would remove a lot
## of noise with good defaults.
## 
## Core Evolution:
## ---------------
## Evolving the core has a number of areas, the most pressing:
## 1. establishing a strong base of default arbitraries
## 2. shrinking support
## 3. replay failed path in run
## 
## Default Arbitraries: a strong base is required to feed NimNode generation so
## valid ASTs can be generated quickly.
## 
## Shrinking: automatically generated programs will often contain a lot of
## noise, a shrinking can do much to provide small failure demonstrating
## scenarios.
## 
## Replay: when you can generate tests, test suites start taking longer very
## quickly. This is of course a good thing, it's a relfection of the lowered
## cost of rapidly exploring large areas of the input space. Being able to
## re-run only a single failing run that otherwise only shows up towards the
## end of a test battery quickly becomes important.
## 
## Heavily inspired by the excellent
## [Fast Check library](https://github.com/dubzzz/fast-check).
## 
## Concepts:
## * predicate - a function which given a value indicates true or false
## * arbitrary - generator of arbitrary value for some set of values
## * property - a condition a value must hold to, given a predicate
## * run - test a single value against a property
## 
## Future directions:
## * properties with predefined examples -- not purely random
## * before and after run hooks for properties
## * support for multiple random number generators
## * optimise arbitraries for Map/Filter/etc via variants, but allow extension
## * distribution control
## * model based checking
## * async testing
## * shrinking

type
  PTStatus* = enum
    ## the result of a single run/predicate check
    ## XXX: likely to be changed to a variant to support, true/false/error+data
    ptPreCondFail,
    ptFail,
    ptPass

  RunId* = range[1..high(int)]
    ## sequential id of the run, starts from 1
  
  PossibleRunId* = int
   ## separate from `RunId` to support 0 value, indicating non-specified
  
  Predicate*[T] = proc(s: T): PTStatus
    ## test function to see if a property holds
  
  Random* = object
    ## random number generator, allows abstraction over algorithm
    seed: uint32
    rng: MersenneTwister
  
  Arbitrary*[T] = object of RootObj
    ## arbitrary value generator for some type T
    ## XXX: eventually migrate to concepts once they're more stable, but
    ##      language stability is the big reason for making this whole property
    ##      based testing framework. :D
    mgenerate: proc(a: Arbitrary[T], mrng: var Random): Shrinkable[T]

  Shrinkable*[T] = object
    ## future support for shrinking
    value: T

  Property*[T] = object
    ## a condition that must hold for an arbitrary as specified by a predicate
    arb: Arbitrary[T]
    predicate: Predicate[T]
  
  Frequency* = int
    ## future use to allow specification of biased generation
  
  # Extra Arbitraries for various circumstances

  MapArbitrary*[T, U] = object of Arbitrary[U]
    ## mapped version of an arbitrary
    mapper: proc(t: T): U
      ## proc used to refine the arbitrary
    origArb: Arbitrary[T]
      ## store the original here so we can wrap it with `mapper`

  FilterArbitrary*[T] = object of Arbitrary[T]
    ## filtered version of an arbitrary
    predicate: proc(t: T): bool
      ## proc used to filter the values generated by arbitrary
    origArb: Arbitrary[T]
      ## store the original here so we can filter with `predicate` to a subset

#-- Run Id

const noRunId = 0.PossibleRunId

proc isUnspecified*(r: PossibleRunId): bool =
  ## used for default param handling
  result = r.uint == 0

proc newRun(): RunId = 1.RunId

proc startRun(r: var RunId): RunId {.discardable, inline.} =
  ## marks the current run as complete and returns the preivous RunId
  result = r
  inc r

proc startRun(r: var PossibleRunId) {.inline.} =
  inc r

proc runIdToFrequency(r: RunId): int =
  2 + toInt(floor(log10(r.int.float)))

#-- Shrinkable

# These seem redundant with Arbitraries, this is mostly for convenience. The
# main reason is that these represent map/filter/etc over a singular shrinkable
# valid value -- which might need particular care. The convenience is when we
# actually implement shrinking and distinguishing specific valid instance vs
# intermediate values an Arbitrary might generate along the way to generating a
# valid value are not the same thing.

proc map[T, U](s: Shrinkable[T], mapper: proc(t: T): U): Shrinkable[U] =
  result = Shrinkable[U](value: mapper(s.value))

proc filter[T](s: Shrinkable[T], predicate: proc(t: T): bool): Shrinkable[T] =
  result = Shrinkable[T](value: predicate(s.value))

proc shrinkableOf[T](v: T): Shrinkable[T] =
  result = Shrinkable[T](value: v)

proc shrinkableOf[T](v: var T): var Shrinkable[T] =
  result = Shrinkable[T](value: v)

#-- Arbitrary

proc generate*[T](a: Arbitrary[T], mrng: var Random): Shrinkable[T] =
  ## calls the internal implementation
  a.mgenerate(a, mrng)

proc map*[T,U](o: Arbitrary[T], mapper: proc(t: T): U): Arbitrary[U] =
  ## creates a new Arbitrary with mapped values
  ## XXX: constraining U by T isn't possible right now, need to fix generics
  let
    mgenerate = proc(a: Arbitrary[U], mrng: var Random): Shrinkable[U] =
                  result = o.generate(mrng).map(mapper)

  return Arbitrary[U](mgenerate: mgenerate)

proc filter*[T](a: Arbitrary[T], predicate: proc(t: T): bool): Arbitrary[T] =
  ## creates a new Arbitrary with filtered values, aggressive filters can lead
  ## to exhausted values.
  let
    mgenerate = proc(o: Arbitrary[T], mrng: Random): Shrinkable[T] =
                  var g = o.generate(mrng)
                  while not g.filter(predicate):
                    g = o.generate(mrng)
                  result = g

  return Arbitrary[T](mgenerate: mgenerate)

proc flatMap[T, U](s: Arbitrary[T], fmapper: proc(t: T): Arbitrary[U]): Arbitrary[U] =
  ## creates a new Arbitrary for every value when you want to make the value
  ## of an Arbitrary depend upon the value of another.
  let
    mgenerate = proc(a: Arbitrary[U], mrng: var Random): Shrinkable[U] =
                  result = fmapper(s.generate(mrng).value).generate(mrng)
  return Arbitrary[U](mgenerate: mgenerate)

proc take*[T](a: Arbitrary[T], n: uint, mrng: Random): Shrinkable[seq[T]] =
  ## generates a sequence of values meant to be used collectively
  var r = mrng
  result = shrinkableOf(newSeqOfCap[T](n))
  result.value.apply((_) => a.generate(r).value)

proc sample*[T](a: Arbitrary[T], n: uint, mrng: var Random): seq[Shrinkable[T]] =
  ## generate a sequence of values meant to be used individually
  result = shrinkableOf(newSeqOfCap[T](n))
  result.value.apply((_) => a.generate(mrng))

#-- Property

converter toPTStatus(b: bool): PTStatus =
  ## yes, they're evil, but in this case they're incredibly helpful
  ## XXX: does this need to be exported?
  if b: ptPass else: ptFail

proc newProperty*[T](arb: Arbitrary[T], p: Predicate): Property[T] =
  result = Property[T](arb: arb, predicate: p)

proc withBias[T](arb: Arbitrary[T], f: Frequency): Arbitrary[T] =
  ## create an arbitrary with bias
  ## XXX: implement biasing, 
  return arb

proc generateAux[T](p: Property[T], mrng: var Random,
                    r: PossibleRunId): Shrinkable[T] =
  result =
    if r.isUnspecified():
      p.arb.generate(mrng)
    else:
      p.arb.withBias(runIdToFrequency(r)).generate(mrng)

proc generate*[T](p: Property[T], mrng: var Random, runId: RunId): Shrinkable[T] =
  return generateAux(p, mrng, runId)

proc generate*[T](p: Property[T], mrng: var Random): Shrinkable[T] =
  return generateAux(p, mrng, noRunId)

proc run*[T](p: Property[T], v: T): PTStatus =
  try:
    result = p.predicate(v)
  except:
    # XXX: do some exception related checking here, for now pass through
    raise getCurrentException()
  finally:
    # XXX: for hooks
    discard

#-- Random Number Generation
# XXX: the trick with rngs is that the number of calls to them matter, so we'll
#      have to start tracking number of calls in between arbitrary generation
#      other such things (well beyond just the seed) in order to quickly
#      reproduce a failure. Additionally, different psuedo random number
#      generation schemes are required because they have various distribution
#      and performance characteristics which quickly become relevant at scale.
proc newRandom(seed: uint32 = 0): Random =
  Random(seed: seed, rng: newMersenneTwister(seed))

proc nextUint32(r: var Random): uint32 =
  result = r.rng.getNum()

proc nextInt(r: var Random): int =
  result = cast[int32](r.rng.getNum())

#-- Property Test Status

proc `and`(a, b: PTStatus): PTStatus =
  ## XXX: this probably shouldn't be boo
  var status = [a, b]
  status.sort()
  result = status[0]

#-- Basic Arbitraries
# these are so you can actually test a thing

proc tupleArb*[A](a1: Arbitrary[A]): Arbitrary[(A,)] =
  ## Arbitrary of single-value tuple
  result = Arbitrary[(A,)](
    mgenerate: proc(arb: Arbitrary[(A,)], rng: var Random): Shrinkable[(A,)] =
                  shrinkableOf((a1.generate(rng).value,))
  )

proc tupleArb*[A,B](a1: Arbitrary[A], a2: Arbitrary[B]): Arbitrary[(A,B)] =
  ## Arbitrary of pair tuple
  result = Arbitrary[(A,B)](
    mgenerate: proc(a: Arbitrary[(A,B)], rng: var Random): Shrinkable[(A,B)] =
                  shrinkableOf(
                    (a1.generate(rng).value, a2.generate(rng).value)
                  )
  )

proc intArb*(): Arbitrary[int] =
  result = Arbitrary[int](
    mgenerate: proc(arb: Arbitrary[int], rng: var Random): Shrinkable[int] =
                  shrinkableOf(rng.nextInt())
  )

proc uint32Arb*(): Arbitrary[uint32] =
  result = Arbitrary[uint32](
    mgenerate: proc(arb: Arbitrary[uint32], rng: var Random): Shrinkable[uint32] =
                  shrinkableOf(rng.nextUint32())
  )

proc uint32Arb*(min, max: uint32): Arbitrary[uint32] =
  ## create a uint32 arbitrary with values in the range of min and max which
  ## are inclusive.
  assert min < max, "max must be greater than min"
  let size = max - min
  result = Arbitrary[uint32](
    mgenerate: proc(arb: Arbitrary[uint32], rng: var Random): Shrinkable[uint32] =
                  shrinkableOf(min + (rng.nextUint32() mod size))
  )

proc charArb*(): Arbitrary[char] =
  ## create a char arbitrary
  result = uint32Arb(0, 255).map((i) => chr(cast[range[0 .. 255]](i)))

proc seqArbOf*[T](a: Arbitrary[T], min: uint32 = 0, max: uint32 = 100): Arbitrary[seq[T]] =
  ## create a sequence of varying size of some type
  assert min <= max
  result = uint32Arb(min, max).map((i) => a.take(i))

proc stringArb*(min: uint32 = 0, max: uint32 = 1000): Arbitrary[string] =
  result = uint32Arb(min, max).flatMap(
      proc(size: uint32): Arbitrary[string] =
        Arbitrary[string](
          mgenerate: proc(a: Arbitrary[string], mrng: var Random): Shrinkable[string] =
            charArb().take(size, mrng).map((cs) => cs.join()))
    )

#-- Assert Property Reporting

type
  RunExecution[T] = object
    ## result of run execution
    # XXX: move to using this rather than open state in `assertProperty` procs
    # XXX: lots to do to finish this:
    #      * save necessary state for quick reproduction (path, etc)
    #      * support async and streaming (iterator? CPS? magical other thing?)
    runId: uint32
    failureOn: PossibleRunId
    seed: uint32
    counterExample: Option[T]

  AssertReport*[T] = object
    ## result of a property assertion, with all runs information
    # XXX: don't need counter example and generic param here once
    #      `RunExecution` is being used.
    name: string ## XXX: populate me 
    runId: PossibleRunId
    failures: uint32
    firstFailure: PossibleRunId
    failureType: PTStatus
    counterExample: Option[T]

proc startRun[T](r: var AssertReport[T]) {.inline.} =
  r.runId.startRun()

proc recordFailure*[T](r: var AssertReport[T], example: T,
                      ft: PTStatus) =
  ## records the failure in the report, and notes first failure and associated
  ## counter-example as necessary
  assert ft in {ptFail, ptPreCondFail}, fmt"invalid failure status: {ft}"
  if r.firstFailure.isUnspecified():
    r.firstFailure = r.runId
    r.counterExample = some(example)
  inc r.failures
  when defined(debug):
    let exampleStr = $example.get() # XXX: handle non-stringable stuff
    echo fmt"Fail({r.runId}): {ft} - {exampleStr}"

proc hasFailure*(r: AssertReport): bool =
  result = not r.firstFailure.isUnspecified()

proc isSuccessful*(r: AssertReport): bool =
  result = r.firstFailure.isUnspecified()

proc `$`*[T](r: AssertReport[T]): string =
  # XXX: make this less ugly
  let status =
    if r.hasFailure:
      fmt"failures: {r.failures}, firstFailure: {r.firstFailure}, firstFailureType: {r.failureType}, counter-example: {r.counterExample}"
    else:
      "status: success"

  result = fmt"name: {r.name}, {status}, totalRuns: {r.runId.int}"

proc startReport[T](name: string = ""): AssertReport[T] =
  ## start a new report
  result = AssertReport[T](name: name, runId: noRunId, failures: 0,
                        firstFailure: noRunId, counterExample: none[T]())

#-- Assert Properties

type
  AssertParams* = object
    ## parameters for asserting properties
    # XXX: add more params to control tests, eg:
    #      * `examples` as a seq[T], for default values
    seed*: uint32
    random*: Random
    runsBeforeSuccess*: range[1..high(int)]

proc defAssertPropParams(): AssertParams =
  ## default params used for an `assertProperty`
  let seed: uint32 = 1
  result = AssertParams(seed: seed, random: newRandom(seed),
                        runsBeforeSuccess: 1000)

proc assertProperty*[T](arb: Arbitrary[T], pred: Predicate[T], 
                        params: AssertParams = defAssertPropParams()
                       ): AssertReport[T] =
  ## run a property
  var
    report = startReport[T]()
    rng = params.random # XXX: need a var version, but this might hurt replay
    p = newProperty(arb, pred)
  
  while(report.runId < params.runsBeforeSuccess):
    report.startRun()
    let
      s: Shrinkable[T] = p.generate(rng, report.runId)
      r: PTStatus = p.run(s.value)
      didSucceed = r notin {ptFail, ptPreCondFail}
    
    if not didSucceed:
      report.recordFailure(s.value, r)
  
  result = report
  # XXX: this shouldn't be an doAssert like this, need a proper report
  # doAssert didSucceed, fmt"Fail({runId}): {r} - {s.value}"
  
  # XXX: if we made it this far assume it worked
  # return true

proc assertProperty*[A, B](
  arb1: Arbitrary[A], arb2: Arbitrary[B],
  pred: Predicate[(A, B)],
  params: AssertParams = defAssertPropParams()): AssertReport[(A,B)] =
  ## run a property
  result = startReport[(A, B)]()
  var
    rng = params.random # XXX: need a var version
    arb = tupleArb[A,B](arb1, arb2)
    p = newProperty(arb, pred)
  
  while(result.runId < params.runsBeforeSuccess):
    result.startRun()
    let
      s: Shrinkable[(A,B)] = p.generate(rng, result.runId)
      r = p.run(s.value)
      didSucceed = r notin {ptFail, ptPreCondFail}
    
    if not didSucceed:
      result.recordFailure(s.value, r)

    # XXX: this shouldn't be an doAssert like this, need a proper report
    # doAssert didSucceed, fmt"Fail({runId}): {r} - {s.value}"
  
  # XXX: if we made it this far assume it worked
  # return true

proc assertProperty*[A, B, C](
  arb1: Arbitrary[A], arb2: Arbitrary[B], arb3: Arbitrary[C],
  pred: Predicate[(A, B, C)],
  params: AssertParams = defAssertPropParams()): AssertReport[(A,B,C)] =
  ## run a property
  result = startReport[(A, B, C)]()
  var
    rng = params.random # XXX: need a var version
    arb = tupleArb[A,B,C](arb1, arb2, arb3)
    p = newProperty(arb, pred)
  
  while(result.runId < params.runsBeforeSuccess):
    result.startRun()
    let
      s: Shrinkable[(A,B,C)] = p.generate(rng, result.runId)
      r = p.run(s.value)
      didSucceed = r notin {ptFail, ptPreCondFail}
    
    if not didSucceed:
      result.recordFailure(s.value, r)

    # XXX: this shouldn't be an doAssert like this, need a proper report
    # doAssert didSucceed, fmt"Fail({runId}): {r} - {s.value}"
  
  # XXX: if we made it this far assume it worked
  # return true

#-- Hackish Tests

when isMainModule:
  block:
    let foo = proc(i: uint32): PTStatus =
                case i >= 0
                of true: ptPass
                of false: ptFail
    var arb = uint32Arb()
    echo "uint32 are >= 0, yes it's silly ", assertProperty(arb, foo)

  block:
    let
      min: uint32 = 100000000
      max = high(uint32)
    echo fmt"uint32 within the range[{min}, {max}]"
    let foo = proc(i: uint32): PTStatus =
                case i >= min
                of true: ptPass
                of false: ptFail
    var arb = uint32Arb(min, max)
    echo assertProperty(arb, foo)

  block:
    echo "classic math assumption should fail"
    let foo = proc(t: ((uint32, uint32))): PTStatus =
                let (a, b) = t
                case a + b > a
                of true: ptPass
                of false: ptFail
    echo assertProperty(uint32Arb(), uint32Arb(), foo)
  
  block:
    echo "characters are oridinals - have corresponding int values"
    let foo = proc(c: char): PTStatus =
      c == chr(ord(c)) and ord(c) >= 0 and ord(c) <= 255
    echo assertProperty(charArb(), foo)

  block:
    echo """characters are ordinals - have successors and predecessors or
            are at the end range"""
    let gen = proc(c: char): (char, char, char) =
      let
        prev = if c == low(char): c else: pred(c)
        curr = c
        next = if c == high(char): c else: succ(c)
      (prev, curr, next)
    let foo = proc(cs: (char, char, char)): PTStatus =
      let (a, b, c) = cs
      (a < b and b < c) or (a <= b and b < c) or (a < b and b <= c)
    echo assertProperty(charArb().map(gen), foo)
  
  block:
    echo "strings - concatenation - len is >= the sum of the len of the parts"
    let foo = proc(ss: (string, string)): PTStatus =
      let (a, b) = ss
      a.len + b.len <= (a & b).len
    echo assertProperty(stringArb(), stringArb(), foo)

#-- Macro approach, need to revisit

# XXX: need to make these work
# proc initArbitrary[T: tuple]: Arbitrary[T] =
#   # Temporary procedure we need to figure out how to make for *all* types
#   let size = 100u32
#   result = Arbitrary[T](
#     mgenerate: proc(arb: Arbitrary[T], rng: var Random): Shrinkable[T] =
#       var a = default T
#       for field in a.fields:
#         field = type(field)(rng.nextUint32() mod size)
#   )

# macro assertProperty*(name: string, values: varargs[typed],
#                       params = defAssertPropParams(), body: untyped): untyped =
#   ## Generates and runs a property. Currently this auto-generates parameter
#   ## names from a to z based on the tuple width -- 26 parameters is good enough
#   ## for now.
#   # XXX: do we want to make the parameter naming explicit?
#   var tupleTyp = nnkTupleConstr.newTree()
#   let
#     isTuple = values.kind == nnkBracket and values[0].kind == nnkTupleConstr
#     values = if isTuple: values[0] else: values
#     possibleIdents = {'a'..'z'}.toSeq
#     idents = block: # Generate the tuple, and the name unpack varaibles
#       var
#         idents: seq[NimNode]
#       for i, x in values:
#         let retT = x[0].getImpl[3][0][1]
#         idents.add ident($possibleIdents[i])
#         tupleTyp.add retT
#       idents

#   # make the `let (a, b ...) = input`
#   let unpackNode = nnkLetSection.newTree(nnkVarTuple.newTree(idents))  
#   unpackNode[0].add newEmptyNode(), ident"input"
  
#   body.insert 0, unpackNode # add unpacking to the first step

#   result = newStmtList()
#   result.add newProc(ident"test",
#                      [
#                        ident"PTStatus",
#                        newIdentDefs(ident"input", tupleTyp, newEmptyNode())
#                      ],
#                      body) # Emit the proc
#   result.add quote do:
#     var
#       arb = initArbitrary[`tupleTyp`]()
#       report = startReport[`tupleTyp`](`name`)
#       rng = `params`.random
#       p = newProperty(arb, test)
#     while report.runId < `params`.runsBeforeSuccess:
#       report.startRun()
#       let
#         s: Shrinkable[`tupleTyp`] = p.generate(rng, report.runId)
#         r: PTStatus = p.run(s.value)
#         didSucceed = r notin {ptFail, ptPreCondFail}
      
#       if not didSucceed:
#         report.recordFailure(s.value, r)
  
#     # XXX: useful for debugging the macro code
#     # echo result.repr

#     echo report
#     if report.hasFailure:
#       doAssert report.isSuccessful, $report
