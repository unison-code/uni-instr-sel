; ModuleID = 'jpeg.jcparam.jpeg_quality_scaling.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @jpeg_quality_scaling(i32 signext %quality) #0 {
  %1 = icmp slt i32 %quality, 1
  %.quality = select i1 %1, i32 1, i32 %quality
  %2 = icmp sgt i32 %.quality, 100
  %..quality = select i1 %2, i32 100, i32 %.quality
  %3 = icmp slt i32 %..quality, 50
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  %5 = sdiv i32 5000, %..quality
  br label %9

; <label>:6                                       ; preds = %0
  %7 = shl nsw i32 %..quality, 1
  %8 = sub nsw i32 200, %7
  br label %9

; <label>:9                                       ; preds = %6, %4
  %.0 = phi i32 [ %5, %4 ], [ %8, %6 ]
  ret i32 %.0
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
