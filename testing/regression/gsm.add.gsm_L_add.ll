; ModuleID = 'gsm.add.gsm_L_add.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @gsm_L_add(i32 signext %a, i32 signext %b) #0 {
  %1 = icmp slt i32 %a, 0
  br i1 %1, label %2, label %12

; <label>:2                                       ; preds = %0
  %3 = icmp sgt i32 %b, -1
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %2
  %5 = add nsw i32 %b, %a
  br label %18

; <label>:6                                       ; preds = %2
  %7 = xor i32 %a, -1
  %8 = xor i32 %b, -1
  %9 = add i32 %8, %7
  %10 = icmp ugt i32 %9, 2147483646
  %11 = sub i32 -2, %9
  %. = select i1 %10, i32 -2147483648, i32 %11
  br label %18

; <label>:12                                      ; preds = %0
  %13 = icmp slt i32 %b, 1
  %14 = add i32 %b, %a
  br i1 %13, label %18, label %15

; <label>:15                                      ; preds = %12
  %16 = icmp slt i32 %14, 0
  %17 = select i1 %16, i32 2147483647, i32 %14
  br label %18

; <label>:18                                      ; preds = %15, %12, %6, %4
  %.0 = phi i32 [ %5, %4 ], [ %., %6 ], [ %17, %15 ], [ %14, %12 ]
  ret i32 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
