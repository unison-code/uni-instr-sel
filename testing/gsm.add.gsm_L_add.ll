; ModuleID = 'gsm.add.gsm_L_add.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @gsm_L_add(i32 %a, i32 %b) #0 {
entry:
  %cmp = icmp slt i32 %a, 0
  br i1 %cmp, label %if.then, label %if.else10, !exec_freq !1

if.then:                                          ; preds = %entry
  %cmp1 = icmp sgt i32 %b, -1
  br i1 %cmp1, label %if.then2, label %if.else, !exec_freq !2

if.then2:                                         ; preds = %if.then
  %add = add nsw i32 %b, %a
  br label %return, !exec_freq !3

if.else:                                          ; preds = %if.then
  %sub = xor i32 %a, -1
  %sub5 = xor i32 %b, -1
  %add6 = add i32 %sub5, %sub
  %cmp7 = icmp ugt i32 %add6, 2147483646
  %sub9 = sub i32 -2, %add6
  br i1 %cmp7, label %if.else.selecttrue, label %if.else.selectcont, !exec_freq !4

if.else.selecttrue:                               ; preds = %if.else
  br label %if.else.selectcont, !exec_freq !5

if.else.selectcont:                               ; preds = %if.else, %if.else.selecttrue
  %.sub9 = phi i32 [ -2147483648, %if.else.selecttrue ], [ %sub9, %if.else ]
  br label %return, !exec_freq !4

if.else10:                                        ; preds = %entry
  %cmp11 = icmp slt i32 %b, 1
  %add13 = add i32 %b, %a
  br i1 %cmp11, label %return, label %if.else14, !exec_freq !6

if.else14:                                        ; preds = %if.else10
  %cmp17 = icmp slt i32 %add13, 0
  br i1 %cmp17, label %if.else14.selecttrue, label %if.else14.selectcont, !exec_freq !7

if.else14.selecttrue:                             ; preds = %if.else14
  br label %if.else14.selectcont, !exec_freq !8

if.else14.selectcont:                             ; preds = %if.else14, %if.else14.selecttrue
  %cond21 = phi i32 [ 2147483647, %if.else14.selecttrue ], [ %add13, %if.else14 ]
  br label %return, !exec_freq !7

return:                                           ; preds = %if.else14.selectcont, %if.else10, %if.else.selectcont, %if.then2
  %retval.0 = phi i32 [ %add, %if.then2 ], [ %.sub9, %if.else.selectcont ], [ %cond21, %if.else14.selectcont ], [ %add13, %if.else10 ]
  ret i32 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 6144}
!3 = metadata !{i64 3840}
!4 = metadata !{i64 2304}
!5 = metadata !{i64 1152}
!6 = metadata !{i64 10240}
!7 = metadata !{i64 6400}
!8 = metadata !{i64 2400}
