; ModuleID = 'pgp.rsaglue2.make_RSA_PUBLIC_KEY.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.R_RSA_PUBLIC_KEY.18 = type { i32, [256 x i8], [256 x i8] }

@global_precision = external global i16, align 2

; Function Attrs: nounwind
define i32 @make_RSA_PUBLIC_KEY(%struct.R_RSA_PUBLIC_KEY.18* nocapture %rpk, i16* nocapture readonly %e, i16* %n) #1 {
  %1 = tail call i32 @countbits(i16* %n) #3
  %2 = bitcast %struct.R_RSA_PUBLIC_KEY.18* %rpk to i32*
  store i32 %1, i32* %2, align 4, !tbaa !1
  %3 = icmp ugt i32 %1, 2048
  br i1 %3, label %reg2rsaref.exit1, label %4, !exec_freq !6

; <label>:4                                       ; preds = %0
  %gep_int = ptrtoint %struct.R_RSA_PUBLIC_KEY.18* %rpk to i32
  %gep = add i32 %gep_int, 4
  %5 = inttoptr i32 %gep to i8*
  %6 = load i16, i16* @global_precision, align 2, !tbaa !7
  %7 = sext i16 %6 to i32
  %8 = shl nsw i32 %7, 1
  %9 = icmp slt i32 %8, 256
  br i1 %9, label %10, label %16, !exec_freq !9

; <label>:10                                      ; preds = %4
  %11 = sub nsw i32 256, %8
  %gep_int1 = ptrtoint %struct.R_RSA_PUBLIC_KEY.18* %rpk to i32
  %gep2 = add i32 %gep_int1, 260
  %12 = inttoptr i32 %gep2 to i8*
  %13 = sub nsw i32 0, %8
  %gep_int3 = ptrtoint i8* %12 to i32
  %gep4 = add i32 %gep_int3, %13
  %14 = inttoptr i32 %gep4 to i8*
  %15 = bitcast i16* %n to i8*
  br label %reg2rsaref.exit, !exec_freq !10

; <label>:16                                      ; preds = %4
  %gep_int5 = ptrtoint i16* %n to i32
  %gep_array = mul i32 %7, 2
  %gep6 = add i32 %gep_int5, %gep_array
  %17 = inttoptr i32 %gep6 to i16*
  %gep_int7 = ptrtoint i16* %17 to i32
  %gep8 = add i32 %gep_int7, -256
  %18 = inttoptr i32 %gep8 to i16*
  %19 = bitcast i16* %18 to i8*
  br label %reg2rsaref.exit, !exec_freq !10

reg2rsaref.exit:                                  ; preds = %16, %10
  %gep_int9 = ptrtoint %struct.R_RSA_PUBLIC_KEY.18* %rpk to i32
  %gep10 = add i32 %gep_int9, 260
  %20 = inttoptr i32 %gep10 to i8*
  %21 = load i16, i16* @global_precision, align 2, !tbaa !7
  %22 = sext i16 %21 to i32
  %23 = shl nsw i32 %22, 1
  %24 = icmp slt i32 %23, 256
  br i1 %24, label %25, label %31, !exec_freq !9

; <label>:25                                      ; preds = %reg2rsaref.exit
  %26 = sub nsw i32 256, %23
  %gep_int11 = ptrtoint %struct.R_RSA_PUBLIC_KEY.18* %rpk to i32
  %gep12 = add i32 %gep_int11, 516
  %27 = inttoptr i32 %gep12 to i8*
  %28 = sub nsw i32 0, %23
  %gep_int13 = ptrtoint i8* %27 to i32
  %gep14 = add i32 %gep_int13, %28
  %29 = inttoptr i32 %gep14 to i8*
  %30 = bitcast i16* %e to i8*
  br label %reg2rsaref.exit1, !exec_freq !10

; <label>:31                                      ; preds = %reg2rsaref.exit
  %gep_int15 = ptrtoint i16* %e to i32
  %gep_array16 = mul i32 %22, 2
  %gep17 = add i32 %gep_int15, %gep_array16
  %32 = inttoptr i32 %gep17 to i16*
  %gep_int18 = ptrtoint i16* %32 to i32
  %gep19 = add i32 %gep_int18, -256
  %33 = inttoptr i32 %gep19 to i16*
  %34 = bitcast i16* %33 to i8*
  br label %reg2rsaref.exit1, !exec_freq !10

reg2rsaref.exit1:                                 ; preds = %31, %25, %0
  %.0 = phi i32 [ -3, %0 ], [ 0, %25 ], [ 0, %31 ]
  ret i32 %.0, !exec_freq !6
}

declare i32 @countbits(i16*) #2

attributes #0 = { argmemonly nounwind }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !3, i64 0}
!2 = !{!"", !3, i64 0, !4, i64 4, !4, i64 260}
!3 = !{!"int", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{i64 32}
!7 = !{!8, !8, i64 0}
!8 = !{!"short", !4, i64 0}
!9 = !{i64 16}
!10 = !{i64 8}
