; ModuleID = 'mem.c'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @func(i32* %a) #0 {
entry:
  %a.addr = alloca i32*, align 8
  %x = alloca i32, align 4
  store i32* %a, i32** %a.addr, align 8
  %0 = load i32*, i32** %a.addr, align 8
  %1 = load i32, i32* %0, align 4
  %add = add nsw i32 %1, 1
  store i32 %add, i32* %x, align 4
  %2 = load i32, i32* %x, align 4
  %3 = load i32*, i32** %a.addr, align 8
  store i32 %2, i32* %3, align 4
  %4 = load i32, i32* %x, align 4
  ret i32 %4
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (/home/ghb/programs/clang/ 07a6361e0f32f699d47c124106e7911b584974d4) (ssh://git@luggage.sics.se/zapbar-llvm-int.git 3489288de822323d56f1806b977ebf039698ec82)"}
