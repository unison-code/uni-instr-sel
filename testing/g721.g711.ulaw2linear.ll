; ModuleID = 'g721.g711.ulaw2linear.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @ulaw2linear(i8 zeroext %u_val) #0 {
entry:
  %neg = xor i8 %u_val, -1
  %conv2 = zext i8 %neg to i32
  %and = shl nuw nsw i32 %conv2, 3
  %shl = and i32 %and, 120
  %add13 = or i32 %shl, 132
  %and4 = lshr i32 %conv2, 4
  %shr = and i32 %and4, 7
  %shl5 = shl i32 %add13, %shr
  %and7 = and i32 %conv2, 128
  %tobool = icmp ne i32 %and7, 0
  %sub = sub nsw i32 132, %shl5
  %sub8 = add nsw i32 %shl5, -132
  br i1 %tobool, label %entry.selecttrue, label %entry.selectcont, !exec_freq !1

entry.selecttrue:                                 ; preds = %entry
  br label %entry.selectcont, !exec_freq !2

entry.selectcont:                                 ; preds = %entry, %entry.selecttrue
  %cond = phi i32 [ %sub, %entry.selecttrue ], [ %sub8, %entry ]
  ret i32 %cond, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 10240}
