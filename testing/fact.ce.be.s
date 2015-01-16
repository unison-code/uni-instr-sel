entry:
  bgt r?,r?,bb1
bb0:
  j for.end
bb3:
  j for.body
for.end:
  phi HILO (HILO, entry) (HILO, for.body)
  jr $31
bb2:
  j for.end
for.body:
  phi HILO (HILO, for.body) (HILO, entry)
  phi HILO (HILO, for.body) (HILO, entry)
  add r?,$0,$0
  mul r?,$0,$0
  bgt r?,r?,bb3
bb1:
  j for.body
