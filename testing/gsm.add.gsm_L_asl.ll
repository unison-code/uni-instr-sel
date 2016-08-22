; ModuleID = 'gsm.add.gsm_L_asl.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @gsm_L_asl(i32 %a, i32 %n) #0 {
entry:
  %cmp = icmp sgt i32 %n, 31
  br i1 %cmp, label %return, label %if.end, !exec_freq !1

if.end:                                           ; preds = %entry
  %cmp1 = icmp slt i32 %n, -31
  br i1 %cmp1, label %if.then2, label %if.end4, !exec_freq !2

if.then2:                                         ; preds = %if.end
  %sub = ashr i32 %a, 31
  br label %return, !exec_freq !3

if.end4:                                          ; preds = %if.end
  %cmp5 = icmp slt i32 %n, 0
  br i1 %cmp5, label %if.then7, label %if.end9, !exec_freq !3

if.then7:                                         ; preds = %if.end4
  %sub8 = sub nsw i32 0, %n
  %cmp.i = icmp sgt i32 %sub8, 31
  br i1 %cmp.i, label %if.then.i, label %if.end.i, !exec_freq !4

if.then.i:                                        ; preds = %if.then7
  %sub.i = ashr i32 %a, 31
  br label %return, !exec_freq !5

if.end.i:                                         ; preds = %if.then7
  %cmp2.i = icmp slt i32 %sub8, -31
  br i1 %cmp2.i, label %return, label %if.end5.i, !exec_freq !5

if.end5.i:                                        ; preds = %if.end.i
  %cmp6.i = icmp sgt i32 %n, 0
  br i1 %cmp6.i, label %if.then8.i, label %if.end10.i, !exec_freq !6

if.then8.i:                                       ; preds = %if.end5.i
  %shl.i = shl i32 %a, %n
  br label %return, !exec_freq !7

if.end10.i:                                       ; preds = %if.end5.i
  %shr.i = ashr i32 %a, %sub8
  br label %return, !exec_freq !8

if.end9:                                          ; preds = %if.end4
  %shl = shl i32 %a, %n
  br label %return, !exec_freq !9

return:                                           ; preds = %if.end9, %if.end10.i, %if.then8.i, %if.end.i, %if.then.i, %if.then2, %entry
  %retval.0 = phi i32 [ %sub, %if.then2 ], [ %shl, %if.end9 ], [ 0, %entry ], [ %sub.i, %if.then.i ], [ %shl.i, %if.then8.i ], [ %shr.i, %if.end10.i ], [ 0, %if.end.i ]
  ret i32 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 8192}
!3 = metadata !{i64 4096}
!4 = metadata !{i64 1536}
!5 = metadata !{i64 768}
!6 = metadata !{i64 384}
!7 = metadata !{i64 240}
!8 = metadata !{i64 144}
!9 = metadata !{i64 2560}
