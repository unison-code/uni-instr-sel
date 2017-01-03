; ModuleID = 'ghostscript.infblock.inflate_blocks_new.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.inflate_blocks_state.30 = type { i32, %union.anon.31, i32, i32, i32, i8*, i8*, i8*, i8*, i32 (i32, i8*, i32)*, i32 }
%union.anon.31 = type { %struct.anon.32 }
%struct.anon.32 = type { i32, i32, i32*, i32, %struct.inflate_huft_s.33* }
%struct.inflate_huft_s.33 = type { %union.anon.0.34, %union.anon.2.35 }
%union.anon.0.34 = type { i8* }
%union.anon.2.35 = type { i32 }
%struct.z_stream_s.36 = type { i8*, i32, i32, i8*, i32, i32, i8*, %struct.internal_state.37*, i8* (i8*, i32, i32)*, void (i8*, i8*)*, i8*, i32, i32, i32 }
%struct.internal_state.37 = type { i32 }

; Function Attrs: nounwind
declare void @inflate_blocks_reset(%struct.inflate_blocks_state.30* nocapture, %struct.z_stream_s.36*, i32* nocapture) #0

; Function Attrs: nounwind
define %struct.inflate_blocks_state.30* @inflate_blocks_new(%struct.z_stream_s.36* %z, i32 (i32, i8*, i32)* %c, i32 signext %w) #0 {
  %gep_int = ptrtoint %struct.z_stream_s.36* %z to i32
  %gep = add i32 %gep_int, 32
  %1 = inttoptr i32 %gep to i8* (i8*, i32, i32)**
  %2 = load i8* (i8*, i32, i32)*, i8* (i8*, i32, i32)** %1, align 4, !tbaa !1
  %gep_int1 = ptrtoint %struct.z_stream_s.36* %z to i32
  %gep2 = add i32 %gep_int1, 40
  %3 = inttoptr i32 %gep2 to i8**
  %4 = load i8*, i8** %3, align 4, !tbaa !8
  %5 = tail call i8* %2(i8* %4, i32 signext 1, i32 signext 60) #1
  %6 = bitcast i8* %5 to %struct.inflate_blocks_state.30*
  %7 = icmp eq i8* %5, null
  br i1 %7, label %29, label %8, !exec_freq !9

; <label>:8                                       ; preds = %0
  %sunkaddr = ptrtoint %struct.z_stream_s.36* %z to i32
  %sunkaddr1 = add i32 %sunkaddr, 32
  %sunkaddr2 = inttoptr i32 %sunkaddr1 to i8* (i8*, i32, i32)**
  %9 = load i8* (i8*, i32, i32)*, i8* (i8*, i32, i32)** %sunkaddr2, align 4, !tbaa !1
  %sunkaddr3 = ptrtoint %struct.z_stream_s.36* %z to i32
  %sunkaddr4 = add i32 %sunkaddr3, 40
  %sunkaddr5 = inttoptr i32 %sunkaddr4 to i8**
  %10 = load i8*, i8** %sunkaddr5, align 4, !tbaa !8
  %11 = tail call i8* %9(i8* %10, i32 signext 1, i32 signext %w) #1
  %gep_int3 = ptrtoint i8* %5 to i32
  %gep4 = add i32 %gep_int3, 36
  %12 = inttoptr i32 %gep4 to i8*
  %13 = bitcast i8* %12 to i8**
  store i8* %11, i8** %13, align 4, !tbaa !10
  %14 = icmp eq i8* %11, null
  br i1 %14, label %15, label %19, !exec_freq !12

; <label>:15                                      ; preds = %8
  %gep_int5 = ptrtoint %struct.z_stream_s.36* %z to i32
  %gep6 = add i32 %gep_int5, 36
  %16 = inttoptr i32 %gep6 to void (i8*, i8*)**
  %17 = load void (i8*, i8*)*, void (i8*, i8*)** %16, align 4, !tbaa !13
  %sunkaddr6 = ptrtoint %struct.z_stream_s.36* %z to i32
  %sunkaddr7 = add i32 %sunkaddr6, 40
  %sunkaddr8 = inttoptr i32 %sunkaddr7 to i8**
  %18 = load i8*, i8** %sunkaddr8, align 4, !tbaa !8
  tail call void %17(i8* %18, i8* nonnull %5) #1
  br label %29, !exec_freq !14

; <label>:19                                      ; preds = %8
  %20 = bitcast i8* %5 to %struct.inflate_blocks_state.30*
  %gep_int7 = ptrtoint i8* %11 to i32
  %gep8 = add i32 %gep_int7, %w
  %21 = inttoptr i32 %gep8 to i8*
  %gep_int9 = ptrtoint i8* %5 to i32
  %gep10 = add i32 %gep_int9, 40
  %22 = inttoptr i32 %gep10 to i8*
  %23 = bitcast i8* %22 to i8**
  store i8* %21, i8** %23, align 4, !tbaa !15
  %gep_int11 = ptrtoint i8* %5 to i32
  %gep12 = add i32 %gep_int11, 52
  %24 = inttoptr i32 %gep12 to i8*
  %25 = bitcast i8* %24 to i32 (i32, i8*, i32)**
  store i32 (i32, i8*, i32)* %c, i32 (i32, i8*, i32)** %25, align 4, !tbaa !16
  %26 = bitcast i8* %5 to i32*
  store i32 0, i32* %26, align 4, !tbaa !17
  %gep_int13 = ptrtoint i8* %5 to i32
  %gep14 = add i32 %gep_int13, 56
  %27 = inttoptr i32 %gep14 to i8*
  %28 = bitcast i8* %27 to i32*
  tail call void @inflate_blocks_reset(%struct.inflate_blocks_state.30* nonnull %20, %struct.z_stream_s.36* nonnull %z, i32* %28)
  br label %29, !exec_freq !18

; <label>:29                                      ; preds = %19, %15, %0
  %.0 = phi %struct.inflate_blocks_state.30* [ null, %15 ], [ %20, %19 ], [ %6, %0 ]
  ret %struct.inflate_blocks_state.30* %.0, !exec_freq !9
}

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !3, i64 32}
!2 = !{!"z_stream_s", !3, i64 0, !6, i64 4, !7, i64 8, !3, i64 12, !6, i64 16, !7, i64 20, !3, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !6, i64 44, !7, i64 48, !7, i64 52}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!"int", !4, i64 0}
!7 = !{!"long", !4, i64 0}
!8 = !{!2, !3, i64 40}
!9 = !{i64 34}
!10 = !{!11, !3, i64 36}
!11 = !{!"inflate_blocks_state", !4, i64 0, !4, i64 4, !6, i64 24, !6, i64 28, !7, i64 32, !3, i64 36, !3, i64 40, !3, i64 44, !3, i64 48, !3, i64 52, !7, i64 56}
!12 = !{i64 21}
!13 = !{!2, !3, i64 36}
!14 = !{i64 8}
!15 = !{!11, !3, i64 40}
!16 = !{!11, !3, i64 52}
!17 = !{!11, !4, i64 0}
!18 = !{i64 13}
