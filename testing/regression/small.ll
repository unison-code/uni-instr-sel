; ModuleID = 'small.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @func(i32 %a) #0 {
entry:
  %add = add nsw i32 %a, %a
  %cmp = icmp eq i32 %add, 10
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 10, %if.then ], [ %add, %if.end ]
  ret i32 %retval.0
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (/home/ghb/programs/clang/ 07a6361e0f32f699d47c124106e7911b584974d4) (ssh://git@luggage.sics.se/zapbar-llvm-int.git 3489288de822323d56f1806b977ebf039698ec82)"}
