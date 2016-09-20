; ModuleID = 'g721.g711.ulaw2linear.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

; Function Attrs: norecurse nounwind readnone
define i32 @ulaw2linear(i8 zeroext %u_val) #0 {
  %1 = zext i8 %u_val to i32
  %2 = xor i32 %1, 255
  %3 = shl nuw nsw i32 %2, 3
  %4 = and i32 %3, 120
  %5 = or i32 %4, 132
  %6 = lshr i32 %2, 4
  %7 = and i32 %6, 7
  %8 = shl i32 %5, %7
  %9 = and i32 %2, 128
  %10 = icmp ne i32 %9, 0
  %11 = sub nsw i32 132, %8
  %12 = add nsw i32 %8, -132
  %13 = select i1 %10, i32 %11, i32 %12
  ret i32 %13
}

attributes #0 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
