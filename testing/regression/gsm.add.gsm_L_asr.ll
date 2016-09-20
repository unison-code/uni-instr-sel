; ModuleID = 'gsm.add.gsm_L_asr.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @gsm_L_asr(i32 signext %a, i32 signext %n) #0 {
  %1 = icmp sgt i32 %n, 31
  br i1 %1, label %2, label %4

; <label>:2                                       ; preds = %0
  %3 = ashr i32 %a, 31
  br label %13

; <label>:4                                       ; preds = %0
  %5 = icmp slt i32 %n, -31
  br i1 %5, label %13, label %6

; <label>:6                                       ; preds = %4
  %7 = icmp slt i32 %n, 0
  br i1 %7, label %8, label %11

; <label>:8                                       ; preds = %6
  %9 = sub nsw i32 0, %n
  %10 = shl i32 %a, %9
  br label %13

; <label>:11                                      ; preds = %6
  %12 = ashr i32 %a, %n
  br label %13

; <label>:13                                      ; preds = %11, %8, %4, %2
  %.0 = phi i32 [ %3, %2 ], [ %10, %8 ], [ %12, %11 ], [ 0, %4 ]
  ret i32 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
