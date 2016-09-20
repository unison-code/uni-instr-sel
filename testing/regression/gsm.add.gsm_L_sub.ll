; ModuleID = 'gsm.add.gsm_L_sub.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @gsm_L_sub(i32 signext %a, i32 signext %b) #0 {
  %1 = icmp sgt i32 %a, -1
  br i1 %1, label %2, label %12

; <label>:2                                       ; preds = %0
  %3 = icmp sgt i32 %b, -1
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %2
  %5 = sub nsw i32 %a, %b
  br label %21

; <label>:6                                       ; preds = %2
  %7 = xor i32 %b, -1
  %8 = add i32 %7, %a
  %9 = icmp ugt i32 %8, 2147483646
  %10 = add i32 %8, 1
  %11 = select i1 %9, i32 2147483647, i32 %10
  br label %21

; <label>:12                                      ; preds = %0
  %13 = icmp slt i32 %b, 1
  br i1 %13, label %14, label %16

; <label>:14                                      ; preds = %12
  %15 = sub nsw i32 %a, %b
  br label %21

; <label>:16                                      ; preds = %12
  %17 = xor i32 %a, -1
  %18 = add i32 %b, %17
  %19 = icmp ugt i32 %18, 2147483646
  %20 = xor i32 %18, -1
  %. = select i1 %19, i32 -2147483648, i32 %20
  br label %21

; <label>:21                                      ; preds = %16, %14, %6, %4
  %.0 = phi i32 [ %5, %4 ], [ %11, %6 ], [ %15, %14 ], [ %., %16 ]
  ret i32 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
