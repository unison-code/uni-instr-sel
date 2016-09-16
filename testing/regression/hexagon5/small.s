entry:
  %add = add(%a, %a)
  %cmp = cmp.eq(%add, 10)
  if (%cmp) jump bb0
bb1:
if.end:
  %add.copy.2 = %add
  jump return
bb0:
if.then:
  %const.10.copy.1 = ##10
return:
  %retval.0 = PHI (%const.10.copy.1, if.then) (%add.copy.2, if.end)
  jumpr %retval.0
