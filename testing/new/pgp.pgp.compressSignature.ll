; ModuleID = 'pgp.pgp.compressSignature.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

@.str.41 = external hidden unnamed_addr constant [5 x i8], align 1
@.str.42 = external hidden unnamed_addr constant [5 x i8], align 1
@.str.43 = external hidden unnamed_addr constant [5 x i8], align 1
@.str.44 = external hidden unnamed_addr constant [3 x i8], align 1
@.str.45 = external hidden unnamed_addr constant [5 x i8], align 1
@.str.46 = external hidden unnamed_addr constant [3 x i8], align 1
@.str.47 = external hidden unnamed_addr constant [3 x i8], align 1
@.str.48 = external hidden unnamed_addr constant [3 x i8], align 1
@.str.49 = external hidden unnamed_addr constant [5 x i8], align 1

; Function Attrs: nounwind readonly
define i32 @compressSignature(i8* nocapture readonly %header) #0 {
  %1 = tail call i32 @strncmp(i8* %header, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.41, i32 0, i32 0), i32 signext 4) #1
  %2 = icmp eq i32 %1, 0
  br i1 %2, label %.loopexit, label %3, !exec_freq !1

; <label>:3                                       ; preds = %0
  %4 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([5 x i8], [5 x i8]* @.str.42, i32 0, i32 0), i32 signext 4) #1
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %.loopexit, label %19, !exec_freq !2

; <label>:6                                       ; preds = %40
  %gep_int = ptrtoint i8* %header to i32
  %gep = add i32 %gep_int, 3
  %7 = inttoptr i32 %gep to i8*
  %8 = load i8, i8* %7, align 1, !tbaa !3
  %9 = icmp eq i8 %8, 108
  br i1 %9, label %10, label %18, !exec_freq !6

; <label>:10                                      ; preds = %6
  %gep_int3 = ptrtoint i8* %header to i32
  %gep4 = add i32 %gep_int3, 4
  %11 = inttoptr i32 %gep4 to i8*
  %12 = load i8, i8* %11, align 1, !tbaa !3
  %13 = zext i8 %12 to i32
  br label %NodeBlock, !exec_freq !7

NodeBlock:                                        ; preds = %10
  %Pivot = icmp slt i32 %13, 122
  br i1 %Pivot, label %LeafBlock, label %LeafBlock1, !exec_freq !7

LeafBlock1:                                       ; preds = %NodeBlock
  %SwitchLeaf2 = icmp eq i32 %13, 122
  br i1 %SwitchLeaf2, label %14, label %NewDefault, !exec_freq !8

LeafBlock:                                        ; preds = %NodeBlock
  %SwitchLeaf = icmp eq i32 %13, 104
  br i1 %SwitchLeaf, label %14, label %NewDefault, !exec_freq !8

; <label>:14                                      ; preds = %LeafBlock, %LeafBlock1
  %gep_int5 = ptrtoint i8* %header to i32
  %gep6 = add i32 %gep_int5, 6
  %15 = inttoptr i32 %gep6 to i8*
  %16 = load i8, i8* %15, align 1, !tbaa !3
  %17 = icmp eq i8 %16, 45
  br i1 %17, label %.loopexit, label %18, !exec_freq !8

NewDefault:                                       ; preds = %LeafBlock, %LeafBlock1
  br label %18, !exec_freq !8

; <label>:18                                      ; preds = %40, %NewDefault, %14, %6
  br label %.loopexit, !exec_freq !9

.loopexit:                                        ; preds = %37, %34, %31, %28, %25, %22, %19, %18, %14, %3, %0
  %.0 = phi i32 [ -1, %18 ], [ 9, %14 ], [ 0, %0 ], [ 1, %3 ], [ 2, %19 ], [ 3, %22 ], [ 4, %25 ], [ 5, %28 ], [ 6, %31 ], [ 7, %34 ], [ 8, %37 ]
  ret i32 %.0, !exec_freq !1

; <label>:19                                      ; preds = %3
  %20 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([5 x i8], [5 x i8]* @.str.43, i32 0, i32 0), i32 signext 4) #1
  %21 = icmp eq i32 %20, 0
  br i1 %21, label %.loopexit, label %22, !exec_freq !10

; <label>:22                                      ; preds = %19
  %23 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([3 x i8], [3 x i8]* @.str.44, i32 0, i32 0), i32 signext 2) #1
  %24 = icmp eq i32 %23, 0
  br i1 %24, label %.loopexit, label %25, !exec_freq !11

; <label>:25                                      ; preds = %22
  %26 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([5 x i8], [5 x i8]* @.str.45, i32 0, i32 0), i32 signext 4) #1
  %27 = icmp eq i32 %26, 0
  br i1 %27, label %.loopexit, label %28, !exec_freq !12

; <label>:28                                      ; preds = %25
  %29 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([3 x i8], [3 x i8]* @.str.46, i32 0, i32 0), i32 signext 2) #1
  %30 = icmp eq i32 %29, 0
  br i1 %30, label %.loopexit, label %31, !exec_freq !13

; <label>:31                                      ; preds = %28
  %32 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([3 x i8], [3 x i8]* @.str.47, i32 0, i32 0), i32 signext 2) #1
  %33 = icmp eq i32 %32, 0
  br i1 %33, label %.loopexit, label %34, !exec_freq !14

; <label>:34                                      ; preds = %31
  %35 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([3 x i8], [3 x i8]* @.str.48, i32 0, i32 0), i32 signext 2) #1
  %36 = icmp eq i32 %35, 0
  br i1 %36, label %.loopexit, label %37, !exec_freq !15

; <label>:37                                      ; preds = %34
  %38 = tail call i32 @strncmp(i8* %header, i8* nonnull getelementptr inbounds ([5 x i8], [5 x i8]* @.str.49, i32 0, i32 0), i32 signext 4) #1
  %39 = icmp eq i32 %38, 0
  br i1 %39, label %.loopexit, label %40, !exec_freq !16

; <label>:40                                      ; preds = %37
  %gep_int7 = ptrtoint i8* %header to i32
  %gep8 = add i32 %gep_int7, 2
  %41 = inttoptr i32 %gep8 to i8*
  %42 = load i8, i8* %41, align 1, !tbaa !3
  %43 = icmp eq i8 %42, 45
  br i1 %43, label %6, label %18, !exec_freq !17
}

; Function Attrs: nounwind readonly
declare i32 @strncmp(i8* nocapture, i8* nocapture, i32 signext) #0

attributes #0 = { nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readonly }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{i64 4398}
!2 = !{i64 2748}
!3 = !{!4, !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{i64 32}
!7 = !{i64 16}
!8 = !{i64 8}
!9 = !{i64 60}
!10 = !{i64 1717}
!11 = !{i64 1073}
!12 = !{i64 671}
!13 = !{i64 419}
!14 = !{i64 262}
!15 = !{i64 163}
!16 = !{i64 102}
!17 = !{i64 64}
