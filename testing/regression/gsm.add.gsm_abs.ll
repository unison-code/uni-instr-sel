; ModuleID = 'gsm.add.gsm_abs.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define signext i16 @gsm_abs(i16 signext %a) #0 {
  %1 = sext i16 %a to i32
  %2 = icmp slt i16 %a, 0
  br i1 %2, label %3, label %6

; <label>:3                                       ; preds = %0
  %4 = icmp eq i16 %a, -32768
  %5 = sub nsw i32 0, %1
  %. = select i1 %4, i32 32767, i32 %5
  br label %6

; <label>:6                                       ; preds = %3, %0
  %7 = phi i32 [ %1, %0 ], [ %., %3 ]
  %8 = trunc i32 %7 to i16
  ret i16 %8
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
