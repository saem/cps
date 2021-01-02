import cps

type
  C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}

proc trampoline(c: C) =
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)

proc loop() {.cps: C.} =
  for i in 0 .. 3:
    discard
trampoline loop()
