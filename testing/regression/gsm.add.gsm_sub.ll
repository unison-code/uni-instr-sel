; ModuleID = 'gsm.add.gsm_sub.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define signext i16 @gsm_sub(i16 signext %a, i16 signext %b) #0 {
  %1 = sext i16 %a to i32
  %2 = sext i16 %b to i32
  %3 = sub nsw i32 %1, %2
  %4 = icmp slt i32 %3, -32768
  br i1 %4, label %8, label %5

; <label>:5                                       ; preds = %0
  %6 = icmp sgt i32 %3, 32767
  %7 = select i1 %6, i32 32767, i32 %3
  %phitmp = trunc i32 %7 to i16
  br label %8

; <label>:8                                       ; preds = %5, %0
  %9 = phi i16 [ -32768, %0 ], [ %phitmp, %5 ]
  ret i16 %9
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
