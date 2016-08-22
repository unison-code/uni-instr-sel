; ModuleID = 'g721.g711.alaw2linear.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @alaw2linear(i8 zeroext %a_val) #0 {
entry:
  %xor = xor i8 %a_val, 85
  %conv2 = zext i8 %xor to i32
  %and = shl nuw nsw i32 %conv2, 4
  %shl = and i32 %and, 240
  %and4 = lshr i32 %conv2, 4
  %shr = and i32 %and4, 7
  br label %NodeBlock, !exec_freq !1

NodeBlock:                                        ; preds = %entry
  %Pivot = icmp slt i32 %shr, 1
  br i1 %Pivot, label %LeafBlock, label %LeafBlock1, !exec_freq !1

LeafBlock1:                                       ; preds = %NodeBlock
  %SwitchLeaf2 = icmp eq i32 %shr, 1
  br i1 %SwitchLeaf2, label %sw.bb5, label %NewDefault, !exec_freq !2

LeafBlock:                                        ; preds = %NodeBlock
  %SwitchLeaf = icmp eq i32 %shr, 0
  br i1 %SwitchLeaf, label %sw.bb, label %NewDefault, !exec_freq !3

sw.bb:                                            ; preds = %LeafBlock
  %add21 = or i32 %shl, 8
  br label %sw.epilog, !exec_freq !4

sw.bb5:                                           ; preds = %LeafBlock1
  %add620 = or i32 %shl, 264
  br label %sw.epilog, !exec_freq !5

NewDefault:                                       ; preds = %LeafBlock1, %LeafBlock
  br label %sw.default, !exec_freq !6

sw.default:                                       ; preds = %NewDefault
  %add722 = or i32 %shl, 264
  %sub = add nsw i32 %shr, -1
  %shl8 = shl i32 %add722, %sub
  br label %sw.epilog, !exec_freq !6

sw.epilog:                                        ; preds = %sw.default, %sw.bb5, %sw.bb
  %t.0 = phi i32 [ %shl8, %sw.default ], [ %add620, %sw.bb5 ], [ %add21, %sw.bb ]
  %and10 = and i32 %conv2, 128
  %tobool = icmp ne i32 %and10, 0
  %sub11 = sub nsw i32 0, %t.0
  br i1 %tobool, label %sw.epilog.selecttrue, label %sw.epilog.selectcont, !exec_freq !1

sw.epilog.selecttrue:                             ; preds = %sw.epilog
  br label %sw.epilog.selectcont, !exec_freq !2

sw.epilog.selectcont:                             ; preds = %sw.epilog, %sw.epilog.selecttrue
  %cond = phi i32 [ %t.0, %sw.epilog.selecttrue ], [ %sub11, %sw.epilog ]
  ret i32 %cond, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 10240}
!3 = metadata !{i64 6144}
!4 = metadata !{i64 2304}
!5 = metadata !{i64 5120}
!6 = metadata !{i64 8960}
