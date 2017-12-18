; ModuleID = 'pgp.mpilib.mp_shortmod.mp_shortmod.low.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "mips--linux-gnu"

@global_precision = external global i16, align 2

; Function Attrs: norecurse nounwind readonly
define zeroext i16 @mp_shortmod(i16* nocapture readonly %dividend, i16 zeroext %divisor) #0 {
  %1 = icmp eq i16 %divisor, 0
  br i1 %1, label %significance.exit.thread, label %2, !exec_freq !1

; <label>:2                                       ; preds = %0
  %3 = load i16, i16* @global_precision, align 2, !tbaa !2
  %4 = sext i16 %3 to i32
  br label %5, !exec_freq !6

; <label>:5                                       ; preds = %8, %2
  %.0.i = phi i16* [ %dividend, %2 ], [ %9, %8 ]
  %precision.0.i = phi i16 [ %3, %2 ], [ %10, %8 ]
  %6 = load i16, i16* %.0.i, align 2, !tbaa !2
  %7 = icmp eq i16 %6, 0
  br i1 %7, label %8, label %significance.exit, !exec_freq !7

; <label>:8                                       ; preds = %5
  %gep_int = ptrtoint i16* %.0.i to i32
  %gep = add i32 %gep_int, 2
  %9 = inttoptr i32 %gep to i16*
  %10 = add i16 %precision.0.i, -1
  %11 = icmp eq i16 %10, 0
  br i1 %11, label %significance.exit.thread, label %5, !exec_freq !8

significance.exit:                                ; preds = %5
  %12 = sext i16 %precision.0.i to i32
  %13 = sub nsw i32 %4, %12
  %gep_int1 = ptrtoint i16* %dividend to i32
  %gep_array = shl nsw i32 %13, 1
  %gep2 = add i32 %gep_array, %gep_int1
  %14 = icmp eq i16 %precision.0.i, 0
  br i1 %14, label %significance.exit.thread, label %15, !exec_freq !9

; <label>:15                                      ; preds = %significance.exit
  %16 = inttoptr i32 %gep2 to i16*
  %17 = shl nsw i32 %12, 4
  %18 = load i16, i16* %16, align 2, !tbaa !2
  %19 = icmp sgt i16 %18, -1
  br i1 %19, label %.lr.ph10, label %.preheader, !exec_freq !10

.preheader:                                       ; preds = %.lr.ph10, %15
  %bitmask.0.lcssa = phi i16 [ -32768, %15 ], [ %23, %.lr.ph10 ]
  %bits.0.lcssa = phi i32 [ %17, %15 ], [ %24, %.lr.ph10 ]
  %20 = icmp eq i32 %bits.0.lcssa, 0
  br i1 %20, label %significance.exit.thread, label %.lr.ph, !exec_freq !10

.lr.ph:                                           ; preds = %.preheader
  %21 = inttoptr i32 %gep2 to i16*
  %22 = zext i16 %divisor to i32
  br label %27, !exec_freq !11

.lr.ph10:                                         ; preds = %.lr.ph10, %15
  %bitmask.09 = phi i16 [ %23, %.lr.ph10 ], [ -32768, %15 ]
  %bits.08 = phi i32 [ %24, %.lr.ph10 ], [ %17, %15 ]
  %23 = lshr i16 %bitmask.09, 1
  %24 = add nsw i32 %bits.08, -1
  %25 = and i16 %23, %18
  %26 = icmp eq i16 %25, 0
  br i1 %26, label %.lr.ph10, label %.preheader, !exec_freq !12

; <label>:27                                      ; preds = %._crit_edge.selectcont, %.lr.ph
  %28 = phi i16 [ %18, %.lr.ph ], [ %.pre, %._crit_edge.selectcont ]
  %.in = phi i32 [ %bits.0.lcssa, %.lr.ph ], [ %29, %._crit_edge.selectcont ]
  %remainder.07 = phi i16 [ 0, %.lr.ph ], [ %remainder.2, %._crit_edge.selectcont ]
  %bitmask.16 = phi i16 [ %bitmask.0.lcssa, %.lr.ph ], [ %.1, %._crit_edge.selectcont ]
  %.015 = phi i16* [ %21, %.lr.ph ], [ %..015, %._crit_edge.selectcont ]
  %29 = add nsw i32 %.in, -1
  %30 = zext i16 %remainder.07 to i32
  %31 = shl nuw nsw i32 %30, 1
  %32 = trunc i32 %31 to i16
  %33 = and i16 %bitmask.16, %28
  %not. = icmp ne i16 %33, 0
  %34 = zext i1 %not. to i16
  %. = or i16 %34, %32
  %35 = icmp ult i16 %., %divisor
  br i1 %35, label %.selectcont, label %36, !exec_freq !12

; <label>:36                                      ; preds = %27
  %37 = zext i16 %. to i32
  %38 = sub nsw i32 %37, %22
  %39 = trunc i32 %38 to i16
  br label %.selectcont, !exec_freq !13

.selectcont:                                      ; preds = %36, %27
  %remainder.2 = phi i16 [ %39, %36 ], [ %., %27 ]
  %40 = lshr i16 %bitmask.16, 1
  %41 = icmp eq i16 %40, 0
  %gep_int3 = ptrtoint i16* %.015 to i32
  %gep4 = add i32 %gep_int3, 2
  %42 = inttoptr i32 %gep4 to i16*
  br i1 %41, label %.selectcont.selecttrue, label %.selectcont.selectcont, !exec_freq !12

.selectcont.selecttrue:                           ; preds = %.selectcont
  br label %.selectcont.selectcont, !exec_freq !14

.selectcont.selectcont:                           ; preds = %.selectcont.selecttrue, %.selectcont
  %..015 = phi i16* [ %42, %.selectcont.selecttrue ], [ %.015, %.selectcont ]
  %43 = icmp eq i32 %29, 0
  br i1 %43, label %significance.exit.thread, label %._crit_edge, !exec_freq !12

._crit_edge:                                      ; preds = %.selectcont.selectcont
  %44 = icmp eq i16 %40, 0
  br i1 %44, label %._crit_edge.selecttrue, label %._crit_edge.selectcont, !exec_freq !15

._crit_edge.selecttrue:                           ; preds = %._crit_edge
  br label %._crit_edge.selectcont, !exec_freq !16

._crit_edge.selectcont:                           ; preds = %._crit_edge.selecttrue, %._crit_edge
  %.1 = phi i16 [ -32768, %._crit_edge.selecttrue ], [ %40, %._crit_edge ]
  %.pre = load i16, i16* %..015, align 2, !tbaa !2
  br label %27, !exec_freq !15

significance.exit.thread:                         ; preds = %.selectcont.selectcont, %.preheader, %significance.exit, %8, %0
  %.0 = phi i16 [ -1, %0 ], [ 0, %significance.exit ], [ 0, %.preheader ], [ %remainder.2, %.selectcont.selectcont ], [ 0, %8 ]
  ret i16 %.0, !exec_freq !1
}

attributes #0 = { norecurse nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{i64 64}
!2 = !{!3, !3, i64 0}
!3 = !{!"short", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{i64 40}
!7 = !{i64 655}
!8 = !{i64 634}
!9 = !{i64 20}
!10 = !{i64 12}
!11 = !{i64 7}
!12 = !{i64 255}
!13 = !{i64 127}
!14 = !{i64 95}
!15 = !{i64 247}
!16 = !{i64 92}
