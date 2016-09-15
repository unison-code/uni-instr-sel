entry:
  %0 = LW 0(%a)
  %add = ADDI %0, 1
  SW %add, 0(%a)
  RetRA %add
