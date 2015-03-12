; ModuleID = 'small.ll'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @func(i32 %a) #0 {
entry:
  %add = add nsw i32 %a, %a
  %cmp = icmp eq i32 %add, 10
  br i1 %cmp, label %if.then, label %if.end, !exec_freq !1

if.then:                                          ; preds = %entry
  br label %return, !exec_freq !2

if.end:                                           ; preds = %entry
  br label %return, !exec_freq !2

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 10, %if.then ], [ %add, %if.end ]
  ret i32 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4 (tags/RELEASE_34/final 207957)"}
!1 = metadata !{i64 1}
!2 = metadata !{i64 0}
