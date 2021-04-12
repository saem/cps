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
  # generated start -- this needs two passes :(
  # block:
  #   var c = x
  #   while c != nil and c.fn != nil:
  #     c = c.fn(c)

  # generated end
  # let x = #[receiver/sender]# int bar(4)
  inc r
  # doAssert x == 8

trampoline foo()
doAssert r == 2

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