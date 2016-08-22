; ModuleID = 'jpeg.jcparam.jpeg_quality_scaling.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define i32 @jpeg_quality_scaling(i32 %quality) #0 {
entry:
  %cmp = icmp slt i32 %quality, 1
  br i1 %cmp, label %entry.selecttrue, label %entry.selectcont, !exec_freq !1

entry.selecttrue:                                 ; preds = %entry
  br label %entry.selectcont, !exec_freq !2

entry.selectcont:                                 ; preds = %entry, %entry.selecttrue
  %.quality = phi i32 [ 1, %entry.selecttrue ], [ %quality, %entry ]
  %cmp1 = icmp sgt i32 %.quality, 100
  br i1 %cmp1, label %entry.selectcont.selecttrue, label %entry.selectcont.selectcont, !exec_freq !1

entry.selectcont.selecttrue:                      ; preds = %entry.selectcont
  br label %entry.selectcont.selectcont, !exec_freq !3

entry.selectcont.selectcont:                      ; preds = %entry.selectcont, %entry.selectcont.selecttrue
  %..quality = phi i32 [ 100, %entry.selectcont.selecttrue ], [ %.quality, %entry.selectcont ]
  %cmp4 = icmp slt i32 %..quality, 50
  br i1 %cmp4, label %if.then5, label %if.else, !exec_freq !1

if.then5:                                         ; preds = %entry.selectcont.selectcont
  %div = sdiv i32 5000, %..quality
  br label %if.end6, !exec_freq !3

if.else:                                          ; preds = %entry.selectcont.selectcont
  %mul = shl nsw i32 %..quality, 1
  %sub = sub nsw i32 200, %mul
  br label %if.end6, !exec_freq !3

if.end6:                                          ; preds = %if.else, %if.then5
  %quality.addr.0 = phi i32 [ %div, %if.then5 ], [ %sub, %if.else ]
  ret i32 %quality.addr.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 6144}
!3 = metadata !{i64 8192}
