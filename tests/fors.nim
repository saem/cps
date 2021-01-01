import testes

import cps
from cps/eventqueue import trampoline, Cont

testes:
  var r = 0

  proc adder(x: var int) =
    inc x

  block:
    ## for loop with continue, break
    proc foo() {.cps: Cont.} =
      r = 1
      while true:
        for i in 0 .. 3:
          if i == 0:
            continue
          if i > 2:
            break
          r = r + i
        inc r
        if r == 5:
          break
      inc r
    trampoline foo()
    check r == 6, "r is " & $r
