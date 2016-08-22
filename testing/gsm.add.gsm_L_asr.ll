; ModuleID = 'gsm.add.gsm_L_asr.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @gsm_L_asr(i32 %a, i32 %n) #0 {
entry:
  %cmp = icmp sgt i32 %n, 31
  br i1 %cmp, label %if.then, label %if.end, !exec_freq !1

if.then:                                          ; preds = %entry
  %sub = ashr i32 %a, 31
  br label %return, !exec_freq !2

if.end:                                           ; preds = %entry
  %cmp2 = icmp slt i32 %n, -31
  br i1 %cmp2, label %return, label %if.end5, !exec_freq !2

if.end5:                                          ; preds = %if.end
  %cmp6 = icmp slt i32 %n, 0
  br i1 %cmp6, label %if.then8, label %if.end10, !exec_freq !3

if.then8:                                         ; preds = %if.end5
  %sub9 = sub nsw i32 0, %n
  %shl = shl i32 %a, %sub9
  br label %return, !exec_freq !4

if.end10:                                         ; preds = %if.end5
  %shr = ashr i32 %a, %n
  br label %return, !exec_freq !5

return:                                           ; preds = %if.end10, %if.then8, %if.end, %if.then
  %retval.0 = phi i32 [ %sub, %if.then ], [ %shl, %if.then8 ], [ %shr, %if.end10 ], [ 0, %if.end ]
  ret i32 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 8192}
!3 = metadata !{i64 4096}
!4 = metadata !{i64 1536}
!5 = metadata !{i64 2560}
