; ModuleID = 'gsm.add.gsm_L_sub.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @gsm_L_sub(i32 %a, i32 %b) #0 {
entry:
  %cmp = icmp sgt i32 %a, -1
  br i1 %cmp, label %if.then, label %if.else7, !exec_freq !1

if.then:                                          ; preds = %entry
  %cmp1 = icmp sgt i32 %b, -1
  br i1 %cmp1, label %if.then2, label %if.else, !exec_freq !2

if.then2:                                         ; preds = %if.then
  %sub = sub nsw i32 %a, %b
  br label %return, !exec_freq !3

if.else:                                          ; preds = %if.then
  %sub3 = xor i32 %b, -1
  %add4 = add i32 %sub3, %a
  %cmp5 = icmp ugt i32 %add4, 2147483646
  %add6 = add i32 %add4, 1
  br i1 %cmp5, label %if.else.selecttrue, label %if.else.selectcont, !exec_freq !4

if.else.selecttrue:                               ; preds = %if.else
  br label %if.else.selectcont, !exec_freq !5

if.else.selectcont:                               ; preds = %if.else, %if.else.selecttrue
  %.add6 = phi i32 [ 2147483647, %if.else.selecttrue ], [ %add6, %if.else ]
  br label %return, !exec_freq !4

if.else7:                                         ; preds = %entry
  %cmp8 = icmp slt i32 %b, 1
  br i1 %cmp8, label %if.then9, label %if.else11, !exec_freq !6

if.then9:                                         ; preds = %if.else7
  %sub10 = sub nsw i32 %a, %b
  br label %return, !exec_freq !7

if.else11:                                        ; preds = %if.else7
  %sub14 = xor i32 %a, -1
  %add15 = add i32 %b, %sub14
  %cmp16 = icmp ugt i32 %add15, 2147483646
  %sub20 = xor i32 %add15, -1
  br i1 %cmp16, label %if.else11.selecttrue, label %if.else11.selectcont, !exec_freq !4

if.else11.selecttrue:                             ; preds = %if.else11
  br label %if.else11.selectcont, !exec_freq !5

if.else11.selectcont:                             ; preds = %if.else11, %if.else11.selecttrue
  %.sub20 = phi i32 [ -2147483648, %if.else11.selecttrue ], [ %sub20, %if.else11 ]
  br label %return, !exec_freq !4

return:                                           ; preds = %if.else11.selectcont, %if.then9, %if.else.selectcont, %if.then2
  %retval.0 = phi i32 [ %sub, %if.then2 ], [ %.add6, %if.else.selectcont ], [ %sub10, %if.then9 ], [ %.sub20, %if.else11.selectcont ]
  ret i32 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 10240}
!3 = metadata !{i64 6400}
!4 = metadata !{i64 3840}
!5 = metadata !{i64 1920}
!6 = metadata !{i64 6144}
!7 = metadata !{i64 2304}
