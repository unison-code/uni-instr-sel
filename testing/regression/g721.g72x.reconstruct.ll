; ModuleID = 'g721.g72x.reconstruct.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @reconstruct(i32 signext %sign, i32 signext %dqln, i32 signext %y) #0 {
  %1 = lshr i32 %y, 2
  %2 = add i32 %1, %dqln
  %sext.mask = and i32 %2, 32768
  %3 = icmp eq i32 %sext.mask, 0
  br i1 %3, label %7, label %4

; <label>:4                                       ; preds = %0
  %5 = icmp ne i32 %sign, 0
  %6 = select i1 %5, i32 -32768, i32 0
  br label %19

; <label>:7                                       ; preds = %0
  %8 = lshr i32 %2, 7
  %9 = and i32 %8, 15
  %10 = shl i32 %2, 7
  %11 = and i32 %10, 16256
  %12 = or i32 %11, 16384
  %13 = sub nsw i32 14, %9
  %14 = lshr i32 %12, %13
  %15 = icmp ne i32 %sign, 0
  %sext1 = shl i32 %14, 16
  %16 = ashr exact i32 %sext1, 16
  %17 = add nsw i32 %16, -32768
  %18 = select i1 %15, i32 %17, i32 %16
  br label %19

; <label>:19                                      ; preds = %7, %4
  %.0 = phi i32 [ %6, %4 ], [ %18, %7 ]
  ret i32 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
