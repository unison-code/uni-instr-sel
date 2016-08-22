; ModuleID = 'gsm.add.gsm_sub.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define signext i16 @gsm_sub(i16 signext %a, i16 signext %b) #0 {
entry:
  %conv = sext i16 %a to i32
  %conv1 = sext i16 %b to i32
  %sub = sub nsw i32 %conv, %conv1
  %cmp = icmp slt i32 %sub, -32768
  br i1 %cmp, label %cond.end7, label %cond.false, !exec_freq !1

cond.false:                                       ; preds = %entry
  %cmp3 = icmp sgt i32 %sub, 32767
  %0 = trunc i32 %sub to i16
  br i1 %cmp3, label %cond.false.selecttrue, label %cond.false.selectcont, !exec_freq !2

cond.false.selecttrue:                            ; preds = %cond.false
  br label %cond.false.selectcont, !exec_freq !3

cond.false.selectcont:                            ; preds = %cond.false, %cond.false.selecttrue
  %phitmp = phi i16 [ 32767, %cond.false.selecttrue ], [ %0, %cond.false ]
  br label %cond.end7, !exec_freq !2

cond.end7:                                        ; preds = %cond.false.selectcont, %entry
  %cond8 = phi i16 [ -32768, %entry ], [ %phitmp, %cond.false.selectcont ]
  ret i16 %cond8, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 8192}
!3 = metadata !{i64 4096}
