entry:
  B for.cond
bb0:
for.body:
for.inc:
  %mul = MUL %res.0, %n.addr.0
  %dec = ADDI %n.addr.0, -1
for.cond:
  %n.addr.0 = PHI (%n, entry) (%dec, for.inc)
  %res.0 = PHI (%n, entry) (%mul, for.inc)
  %tmp.1 = SLTi %n.addr.0, 1
  BEQ %tmp.1, $0, bb0
bb1:
for.end:
  RetRA %res.0
