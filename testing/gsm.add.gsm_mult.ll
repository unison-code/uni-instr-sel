; ModuleID = 'gsm.add.gsm_mult.ll'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

; Function Attrs: nounwind readnone
define signext i16 @gsm_mult(i16 signext %a, i16 signext %b) #0 {
entry:
  %cmp = icmp eq i16 %a, -32768
  %cmp3 = icmp eq i16 %b, -32768
  %or.cond = and i1 %cmp, %cmp3
  br i1 %or.cond, label %return, label %if.else, !exec_freq !1

if.else:                                          ; preds = %entry
  %conv = sext i16 %a to i32
  %conv6 = sext i16 %b to i32
  %mul = mul nsw i32 %conv6, %conv
  %shr10 = lshr i32 %mul, 15
  %conv7 = trunc i32 %shr10 to i16
  br label %return, !exec_freq !2

return:                                           ; preds = %if.else, %entry
  %retval.0 = phi i16 [ %conv7, %if.else ], [ 32767, %entry ]
  ret i16 %retval.0, !exec_freq !1
}

attributes #0 = { nounwind readnone "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4.2 (http://llvm.org/git/clang.git adb6bc4cf0e09a5aca773ab7680bc4c7bf581909) (ssh://git@luggage.sics.se/zapbar-llvm-int.git f6945c50db3c3713f87ee2d43f2efc8042b70574)"}
!1 = metadata !{i64 16384}
!2 = metadata !{i64 8192}
