; ModuleID = 'g721.g711.alaw2linear.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @alaw2linear(i8 zeroext %a_val) #0 {
  %1 = zext i8 %a_val to i32
  %2 = xor i32 %1, 85
  %3 = shl nuw nsw i32 %2, 4
  %4 = and i32 %3, 240
  %5 = lshr i32 %2, 4
  %6 = and i32 %5, 7
  switch i32 %6, label %11 [
    i32 0, label %7
    i32 1, label %9
  ]

; <label>:7                                       ; preds = %0
  %8 = or i32 %4, 8
  br label %15

; <label>:9                                       ; preds = %0
  %10 = or i32 %4, 264
  br label %15

; <label>:11                                      ; preds = %0
  %12 = or i32 %4, 264
  %13 = add nsw i32 %6, -1
  %14 = shl i32 %12, %13
  br label %15

; <label>:15                                      ; preds = %11, %9, %7
  %t.0 = phi i32 [ %14, %11 ], [ %10, %9 ], [ %8, %7 ]
  %16 = and i32 %1, 128
  %17 = icmp ne i32 %16, 0
  %18 = sub nsw i32 0, %t.0
  %19 = select i1 %17, i32 %t.0, i32 %18
  ret i32 %19
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
