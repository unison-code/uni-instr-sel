; ModuleID = 'gsm.add.gsm_mult_r.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define signext i16 @gsm_mult_r(i16 signext %a, i16 signext %b) #0 {
  %1 = icmp eq i16 %b, -32768
  %2 = icmp eq i16 %a, -32768
  %or.cond = and i1 %2, %1
  br i1 %or.cond, label %10, label %3

; <label>:3                                       ; preds = %0
  %4 = sext i16 %b to i32
  %5 = sext i16 %a to i32
  %6 = mul nsw i32 %4, %5
  %7 = add nsw i32 %6, 16384
  %8 = lshr i32 %7, 15
  %9 = trunc i32 %8 to i16
  br label %10

; <label>:10                                      ; preds = %3, %0
  %.0 = phi i16 [ %9, %3 ], [ 32767, %0 ]
  ret i16 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
