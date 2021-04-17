# XXX: scratch pad to easily run one test at a time, delete later

import cps

type
  InfiniteLoop = CatchableError
  Cont = ref object of RootObj
    fn: proc(c: Cont): Cont

var jumps: int

proc trampoline(c: Cont) =
  jumps = 0
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)
    inc jumps
    if jumps > 1000:
      raise newException(InfiniteLoop, $jumps & " iterations")

proc noop*(c: Cont): Cont {.cpsMagic.} = c

var r = 0

proc bar(a: int): int {.cps: Cont.} =
  inc r
  noop()
  return a * 2

proc foo() {.cps: Cont.} =
  inc r
  let x = bar(4)
  inc r
  doAssert x == 8

trampoline foo()
doAssert r == 3, $r

# import cps

# type
#   InfiniteLoop = CatchableError
#   Cont* = ref object of RootObj
#     when cpsMutant:
#       fn*: proc(c: var Cont) {.nimcall.}
#     else:
#       fn*: proc(c: Cont): Cont {.nimcall.}

# var jumps: int

# proc trampoline(c: Cont) =
#   jumps = 0
#   var c = c
#   while c != nil and c.fn != nil:
#     c = c.fn(c)
#     inc jumps
#     if jumps > 1000:
#       raise newException(InfiniteLoop, $jumps & " iterations")

# proc noop*(c: Cont): Cont {.cpsMagic.} = c

# ## while loops correctly
# var r = 0
# r = 0
# proc foo() {.cps: Cont.} =
#   inc r
#   var i = 0
#   while i < 2:
#     inc r
#     let x = i
#     noop()
#     inc r
#     inc i
#     noop()
#     inc r
#     assert x == i - 1
#   inc r
# trampoline foo()
# assert r == 8

# this is me thinking terrible thoughts in regards to the transform

# proc baz() =
#   echo "baz"

# baz()

# # ----

# var exec = DefaultExecutor()

# m_IndicateLeavingTopLevel(exec) # how we know what we're leaving (drop a marker const/let/var)
# m_setInnerBazLocation(exec)     # how to know where we're going
# m_considerPreemption(exec)      # could run something else
# m_considerAbort(exec)           # after we come back we decide it was all wrong
# m_pushBazContinuations(exec)
# m_pushBazEnv(exec)
# m_pushBazParams(exec)
# #inside baz
# let
#   continuations = popBazContinuations()
#   env = popBazEnv()
#   params = popBazParams()
# innerBazBeforeEcho(params)

# pushEchoContinuations()
# pushEchoEnv()
# pushEchoParams()
# gotoEcho()


