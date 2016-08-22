; ModuleID = 'g721.g72x.reconstruct.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @reconstruct(i32 %sign, i32 %dqln, i32 %y) #0 {
entry:
  %shr22 = lshr i32 %y, 2
  %add = add nsw i32 %shr22, %dqln
  %sext.mask = and i32 %add, 32768
  %cmp = icmp eq i32 %sext.mask, 0
  br i1 %cmp, label %if.else, label %if.then, !exec_freq !1

if.then:                                          ; preds = %entry
  %tobool = icmp ne i32 %sign, 0
  br i1 %tobool, label %if.then.selecttrue, label %if.then.selectcont, !exec_freq !2

if.then.selecttrue:                               ; preds = %if.then
  br label %if.then.selectcont, !exec_freq !3

if.then.selectcont:                               ; preds = %if.then, %if.then.selecttrue
  %cond = phi i32 [ -32768, %if.then.selecttrue ], [ 0, %if.then ]
  br label %return, !exec_freq !2

if.else:                                          ; preds = %entry
  %0 = lshr i32 %add, 7
  %conv5 = and i32 %0, 15
  %and7 = shl i32 %add, 7
  %add825 = and i32 %and7, 16256
  %shl = or i32 %add825, 16384
  %sub = sub nsw i32 14, %conv5
  %shr12 = lshr i32 %shl, %sub
  %tobool14 = icmp ne i32 %sign, 0
  %sext28 = shl i32 %shr12, 16
  %conv15 = ashr exact i32 %sext28, 16
  %sub16 = add nsw i32 %conv15, -32768
  br i1 %tobool14, label %if.else.selecttrue, label %if.else.selectcont, !exec_freq !4

if.else.selecttrue:                               ; preds = %if.else
  br label %if.else.selectcont, !exec_freq !5

if.else.selectcont:                               ; preds = %if.else, %if.else.selecttrue
  %cond18 = phi i32 [ %sub16, %if.else.selecttrue ], [ %conv15, %if.else ]
  br label %return, !exec_freq !4

return:                                           ; preds = %if.else.selectcont, %if.then.selectcont
  %retval.0 = phi i32 [ %cond, %if.then.selectcont ], [ %cond18, %if.else.selectcont ]
  ret i32 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 10240}
!3 = metadata !{i64 6400}
!4 = metadata !{i64 6144}
!5 = metadata !{i64 3840}
