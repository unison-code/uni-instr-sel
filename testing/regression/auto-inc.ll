; ModuleID = 'pgp.mpiio.checksum.low.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

; Function Attrs: norecurse nounwind readonly
define  i16 @checksum(i8* nocapture readonly %buf, i16  %count) #0 {
  br label %loop, !exec_freq !1

loop:
  %.012.prol = phi i8* [ %cgep16, %loop ], [ %buf, %0 ]
  %1 = load i8, i8* %.012.prol, align 1, !tbaa !4
  %gep_int = ptrtoint i8* %.012.prol to i32
  %gep = add i32 %gep_int, 1
  %cgep16 = inttoptr i32 %gep to i8*
  %cmp = icmp eq i8 %1, 0
  br i1 %cmp, label %exit, label %loop, !llvm.loop !7, !exec_freq !9

exit:
  ret i16 %count, !exec_freq !1
}

attributes #0 = { norecurse nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{i64 20}
!2 = !{i64 12}
!3 = !{i64 8}
!4 = !{!5, !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
!7 = distinct !{!7, !8}
!8 = !{!"llvm.loop.unroll.disable"}
!9 = !{i64 255}
!10 = !{i64 204}
