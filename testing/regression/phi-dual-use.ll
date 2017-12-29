; ModuleID = 'phi.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @func(i32 %n) #0 {
entry:
  %cmp = icmp sgt i32 %n, 1
  %x = add i32 %n, 10
  br i1 %cmp, label %bb1, label %bb2
bb1:
  br label %bb2
bb2:
  %y = phi i32 [ %x, %entry ], [ 0, %bb1 ]
  %z = phi i32 [ %x, %entry ], [ 0, %bb1 ]
  %bla = add i32 %y, %z
  ret i32 %bla
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (/home/ghb/programs/clang/ 07a6361e0f32f699d47c124106e7911b584974d4) (ssh://git@luggage.sics.se/zapbar-llvm-int.git 3489288de822323d56f1806b977ebf039698ec82)"}
