; ModuleID = 'gsm.add.gsm_asr.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define signext i16 @gsm_asr(i16 signext %a, i32 signext %n) #0 {
  %1 = icmp sgt i32 %n, 15
  br i1 %1, label %2, label %6

; <label>:2                                       ; preds = %0
  %a.lobit = lshr i16 %a, 15
  %3 = zext i16 %a.lobit to i32
  %4 = sub nsw i32 0, %3
  %5 = trunc i32 %4 to i16
  br label %18

; <label>:6                                       ; preds = %0
  %7 = icmp slt i32 %n, -15
  br i1 %7, label %18, label %8

; <label>:8                                       ; preds = %6
  %9 = icmp slt i32 %n, 0
  %10 = sext i16 %a to i32
  br i1 %9, label %11, label %15

; <label>:11                                      ; preds = %8
  %12 = sub nsw i32 0, %n
  %13 = shl i32 %10, %12
  %14 = trunc i32 %13 to i16
  br label %18

; <label>:15                                      ; preds = %8
  %16 = ashr i32 %10, %n
  %17 = trunc i32 %16 to i16
  br label %18

; <label>:18                                      ; preds = %15, %11, %6, %2
  %.0 = phi i16 [ %5, %2 ], [ %14, %11 ], [ %17, %15 ], [ 0, %6 ]
  ret i16 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
