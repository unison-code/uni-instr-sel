; ModuleID = 'gsm.add.gsm_abs.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define signext i16 @gsm_abs(i16 signext %a) #0 {
entry:
  %cmp = icmp slt i16 %a, 0
  br i1 %cmp, label %cond.true, label %cond.end9, !exec_freq !1

cond.true:                                        ; preds = %entry
  %cmp3 = icmp eq i16 %a, -32768
  %sub = sub i16 0, %a
  br i1 %cmp3, label %cond.true.selecttrue, label %cond.true.selectcont, !exec_freq !2

cond.true.selecttrue:                             ; preds = %cond.true
  br label %cond.true.selectcont, !exec_freq !3

cond.true.selectcont:                             ; preds = %cond.true, %cond.true.selecttrue
  %.sub = phi i16 [ 32767, %cond.true.selecttrue ], [ %sub, %cond.true ]
  ret i16 %.sub, !exec_freq !2

cond.end9:                                        ; preds = %entry
  ret i16 %a, !exec_freq !4
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 6144}
!3 = metadata !{i64 3072}
!4 = metadata !{i64 10240}
