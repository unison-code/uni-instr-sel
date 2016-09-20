; ModuleID = 'gsm.add.gsm_asl.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define signext i16 @gsm_asl(i16 signext %a, i32 signext %n) #0 {
  %1 = icmp sgt i32 %n, 15
  br i1 %1, label %gsm_asr.exit, label %2

; <label>:2                                       ; preds = %0
  %3 = icmp slt i32 %n, -15
  br i1 %3, label %4, label %8

; <label>:4                                       ; preds = %2
  %a.lobit = lshr i16 %a, 15
  %5 = zext i16 %a.lobit to i32
  %6 = sub nsw i32 0, %5
  %7 = trunc i32 %6 to i16
  br label %gsm_asr.exit

; <label>:8                                       ; preds = %2
  %9 = icmp slt i32 %n, 0
  br i1 %9, label %10, label %15

; <label>:10                                      ; preds = %8
  %11 = sub nsw i32 0, %n
  %12 = sext i16 %a to i32
  %13 = ashr i32 %12, %11
  %14 = trunc i32 %13 to i16
  br label %gsm_asr.exit

; <label>:15                                      ; preds = %8
  %16 = sext i16 %a to i32
  %17 = shl i32 %16, %n
  %18 = trunc i32 %17 to i16
  br label %gsm_asr.exit

gsm_asr.exit:                                     ; preds = %15, %10, %4, %0
  %.0 = phi i16 [ %7, %4 ], [ %18, %15 ], [ 0, %0 ], [ %14, %10 ]
  ret i16 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
