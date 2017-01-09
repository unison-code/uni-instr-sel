; ModuleID = 'jpeg.jcmainct.jinit_c_main_controller.low.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct.jpeg_compress_struct.0 = type { %struct.jpeg_error_mgr.1*, %struct.jpeg_memory_mgr.2*, %struct.jpeg_progress_mgr.3*, i32, i32, %struct.jpeg_destination_mgr.4*, i32, i32, i32, i8, double, i32, i32, i8, %struct.jpeg_component_info.5*, [4 x %struct.JQUANT_TBL.6*], [4 x %struct.JHUFF_TBL.7*], [4 x %struct.JHUFF_TBL.7*], [16 x i8], [16 x i8], [16 x i8], i32, %struct.jpeg_scan_info.8*, i32, i32, i32, i32, i32, i8, i32, i32, i32, i8, i16, i16, i32, i32, i32, i32, i32, i32, i32, [4 x %struct.jpeg_component_info.5*], i32, i32, i32, [10 x i32], i32, i32, i32, i32, %struct.jpeg_comp_master.9*, %struct.jpeg_c_main_controller.10*, %struct.jpeg_c_prep_controller.11*, %struct.jpeg_c_coef_controller.12*, %struct.jpeg_marker_writer.13*, %struct.jpeg_color_converter.14*, %struct.jpeg_downsampler.15*, %struct.jpeg_forward_dct.16*, %struct.jpeg_entropy_encoder.17* }
%struct.jpeg_error_mgr.1 = type { void (%struct.jpeg_common_struct.18*)*, void (%struct.jpeg_common_struct.18*, i32)*, void (%struct.jpeg_common_struct.18*)*, void (%struct.jpeg_common_struct.18*, i8*)*, void (%struct.jpeg_common_struct.18*)*, i32, %union.anon.19, i32, i32, i8**, i32, i8**, i32, i32 }
%struct.jpeg_common_struct.18 = type { %struct.jpeg_error_mgr.1*, %struct.jpeg_memory_mgr.2*, %struct.jpeg_progress_mgr.3*, i32, i32 }
%union.anon.19 = type { [8 x i32], [48 x i8] }
%struct.jpeg_memory_mgr.2 = type { i8* (%struct.jpeg_common_struct.18*, i32, i32)*, i8* (%struct.jpeg_common_struct.18*, i32, i32)*, i8** (%struct.jpeg_common_struct.18*, i32, i32, i32)*, [64 x i16]** (%struct.jpeg_common_struct.18*, i32, i32, i32)*, %struct.jvirt_sarray_control.20* (%struct.jpeg_common_struct.18*, i32, i32, i32, i32, i32)*, %struct.jvirt_barray_control.21* (%struct.jpeg_common_struct.18*, i32, i32, i32, i32, i32)*, {}*, i8** (%struct.jpeg_common_struct.18*, %struct.jvirt_sarray_control.20*, i32, i32, i32)*, [64 x i16]** (%struct.jpeg_common_struct.18*, %struct.jvirt_barray_control.21*, i32, i32, i32)*, void (%struct.jpeg_common_struct.18*, i32)*, {}*, i32 }
%struct.jvirt_sarray_control.20 = type opaque
%struct.jvirt_barray_control.21 = type opaque
%struct.jpeg_progress_mgr.3 = type { {}*, i32, i32, i32, i32 }
%struct.jpeg_destination_mgr.4 = type { i8*, i32, void (%struct.jpeg_compress_struct.0*)*, i32 (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)* }
%struct.jpeg_component_info.5 = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.JQUANT_TBL.6*, i8* }
%struct.JQUANT_TBL.6 = type { [64 x i16], i32 }
%struct.JHUFF_TBL.7 = type { [17 x i8], [256 x i8], i32 }
%struct.jpeg_scan_info.8 = type { i32, [4 x i32], i32, i32, i32, i32 }
%struct.jpeg_comp_master.9 = type { void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)*, i32, i32 }
%struct.jpeg_c_main_controller.10 = type { void (%struct.jpeg_compress_struct.0*, i8)*, void (%struct.jpeg_compress_struct.0*, i8**, i32*, i32)* }
%struct.jpeg_c_prep_controller.11 = type { void (%struct.jpeg_compress_struct.0*, i8)*, void (%struct.jpeg_compress_struct.0*, i8**, i32*, i32, i8***, i32*, i32)* }
%struct.jpeg_c_coef_controller.12 = type { void (%struct.jpeg_compress_struct.0*, i8)*, i32 (%struct.jpeg_compress_struct.0*, i8***)* }
%struct.jpeg_marker_writer.13 = type { void (%struct.jpeg_compress_struct.0*, i32, i8*, i32)*, void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*)* }
%struct.jpeg_color_converter.14 = type { void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*, i8**, i8***, i32, i32)* }
%struct.jpeg_downsampler.15 = type { void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*, i8***, i32, i8***, i32)*, i32 }
%struct.jpeg_forward_dct.16 = type { void (%struct.jpeg_compress_struct.0*)*, void (%struct.jpeg_compress_struct.0*, %struct.jpeg_component_info.5*, i8**, [64 x i16]*, i32, i32, i32)* }
%struct.jpeg_entropy_encoder.17 = type { {}*, i32 (%struct.jpeg_compress_struct.0*, [64 x i16]**)*, void (%struct.jpeg_compress_struct.0*)* }

; Function Attrs: nounwind
define void @jinit_c_main_controller(%struct.jpeg_compress_struct.0* %cinfo, i32 %need_full_buffer) #0 {
  %cgep38 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_compress_struct.0*
  %gep_int = ptrtoint %struct.jpeg_compress_struct.0* %cgep38 to i32
  %gep = add i32 %gep_int, 4
  %cgep23 = inttoptr i32 %gep to %struct.jpeg_memory_mgr.2**
  %1 = load %struct.jpeg_memory_mgr.2*, %struct.jpeg_memory_mgr.2** %cgep23, align 4, !tbaa !1
  %cgep939 = bitcast %struct.jpeg_memory_mgr.2* %1 to %struct.jpeg_memory_mgr.2*
  %cgep3140 = bitcast %struct.jpeg_memory_mgr.2* %cgep939 to i8* (%struct.jpeg_common_struct.18*, i32, i32)**
  %2 = load i8* (%struct.jpeg_common_struct.18*, i32, i32)*, i8* (%struct.jpeg_common_struct.18*, i32, i32)** %cgep3140, align 4, !tbaa !9
  %3 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_common_struct.18*
  %4 = tail call i8* %2(%struct.jpeg_common_struct.18* %3, i32 1, i32 64) #1
  %gep_int1 = ptrtoint %struct.jpeg_compress_struct.0* %cgep38 to i32
  %gep2 = add i32 %gep_int1, 328
  %cgep18 = inttoptr i32 %gep2 to %struct.jpeg_c_main_controller.10**
  %5 = bitcast %struct.jpeg_c_main_controller.10** %cgep18 to i8**
  store i8* %4, i8** %5, align 8, !tbaa !12
  %6 = bitcast i8* %4 to void (%struct.jpeg_compress_struct.0*, i8)**
  store void (%struct.jpeg_compress_struct.0*, i8)* @start_pass_main, void (%struct.jpeg_compress_struct.0*, i8)** %6, align 4, !tbaa !13
  %gep_int3 = ptrtoint %struct.jpeg_compress_struct.0* %cgep38 to i32
  %gep4 = add i32 %gep_int3, 168
  %cgep25 = inttoptr i32 %gep4 to i32*
  %7 = load i32, i32* %cgep25, align 8, !tbaa !16
  %8 = icmp eq i32 %7, 0
  br i1 %8, label %9, label %.loopexit, !exec_freq !17

; <label>:9                                       ; preds = %0
  %10 = icmp eq i32 %need_full_buffer, 0
  br i1 %10, label %16, label %11, !exec_freq !18

; <label>:11                                      ; preds = %9
  %12 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_common_struct.18*
  %13 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_compress_struct.0*
  %cgep2941 = bitcast %struct.jpeg_compress_struct.0* %13 to %struct.jpeg_error_mgr.1**
  %14 = load %struct.jpeg_error_mgr.1*, %struct.jpeg_error_mgr.1** %cgep2941, align 8, !tbaa !19
  %cgep1642 = bitcast %struct.jpeg_error_mgr.1* %14 to %struct.jpeg_error_mgr.1*
  %gep_int5 = ptrtoint %struct.jpeg_error_mgr.1* %cgep1642 to i32
  %gep6 = add i32 %gep_int5, 20
  %cgep36 = inttoptr i32 %gep6 to i32*
  store i32 4, i32* %cgep36, align 4, !tbaa !20
  %cgep3743 = bitcast %struct.jpeg_error_mgr.1* %cgep1642 to void (%struct.jpeg_common_struct.18*)**
  %15 = load void (%struct.jpeg_common_struct.18*)*, void (%struct.jpeg_common_struct.18*)** %cgep3743, align 4, !tbaa !22
  tail call void %15(%struct.jpeg_common_struct.18* %12) #1
  ret void, !exec_freq !23

; <label>:16                                      ; preds = %9
  %17 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_compress_struct.0*
  %gep_int7 = ptrtoint %struct.jpeg_compress_struct.0* %17 to i32
  %gep8 = add i32 %gep_int7, 52
  %cgep26 = inttoptr i32 %gep8 to i32*
  %18 = load i32, i32* %cgep26, align 4, !tbaa !24
  %19 = icmp sgt i32 %18, 0
  br i1 %19, label %.lr.ph, label %.loopexit, !exec_freq !25

.lr.ph:                                           ; preds = %16
  %20 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_compress_struct.0*
  %gep_int9 = ptrtoint %struct.jpeg_compress_struct.0* %20 to i32
  %gep10 = add i32 %gep_int9, 60
  %cgep28 = inttoptr i32 %gep10 to %struct.jpeg_component_info.5**
  %21 = load %struct.jpeg_component_info.5*, %struct.jpeg_component_info.5** %cgep28, align 4, !tbaa !26
  %gep_int11 = ptrtoint %struct.jpeg_component_info.5* %21 to i32
  %gep12 = add i32 %gep_int11, 28
  %cgep10 = inttoptr i32 %gep12 to i32*
  %scevgep1 = bitcast i32* %cgep10 to %struct.jpeg_component_info.5*
  %gep_int13 = ptrtoint i8* %4 to i32
  %gep14 = add i32 %gep_int13, 24
  %cgep11 = inttoptr i32 %gep14 to i8*
  br label %22, !exec_freq !27

; <label>:22                                      ; preds = %22, %.lr.ph
  %lsr.iv6 = phi i8* [ %cgep15, %22 ], [ %cgep11, %.lr.ph ]
  %lsr.iv = phi %struct.jpeg_component_info.5* [ %cgep14, %22 ], [ %scevgep1, %.lr.ph ]
  %ci.01 = phi i32 [ 0, %.lr.ph ], [ %32, %22 ]
  %23 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_common_struct.18*
  %24 = bitcast %struct.jpeg_compress_struct.0* %cinfo to %struct.jpeg_compress_struct.0*
  %lsr.iv68 = bitcast i8* %lsr.iv6 to i8***
  %lsr.iv3 = bitcast %struct.jpeg_component_info.5* %lsr.iv to i32*
  %gep_int15 = ptrtoint %struct.jpeg_compress_struct.0* %24 to i32
  %gep16 = add i32 %gep_int15, 4
  %cgep24 = inttoptr i32 %gep16 to %struct.jpeg_memory_mgr.2**
  %25 = load %struct.jpeg_memory_mgr.2*, %struct.jpeg_memory_mgr.2** %cgep24, align 4, !tbaa !1
  %cgep1244 = bitcast %struct.jpeg_memory_mgr.2* %25 to %struct.jpeg_memory_mgr.2*
  %gep_int17 = ptrtoint %struct.jpeg_memory_mgr.2* %cgep1244 to i32
  %gep18 = add i32 %gep_int17, 8
  %cgep33 = inttoptr i32 %gep18 to i8** (%struct.jpeg_common_struct.18*, i32, i32, i32)**
  %26 = load i8** (%struct.jpeg_common_struct.18*, i32, i32, i32)*, i8** (%struct.jpeg_common_struct.18*, i32, i32, i32)** %cgep33, align 4, !tbaa !28
  %27 = load i32, i32* %lsr.iv3, align 4, !tbaa !29
  %28 = shl i32 %27, 3
  %gep_int19 = ptrtoint i32* %lsr.iv3 to i32
  %gep20 = add i32 %gep_int19, -16
  %cgep13 = inttoptr i32 %gep20 to i32*
  %29 = load i32, i32* %cgep13, align 4, !tbaa !31
  %30 = shl nsw i32 %29, 3
  %31 = tail call i8** %26(%struct.jpeg_common_struct.18* nonnull %23, i32 1, i32 %28, i32 %30) #1
  store i8** %31, i8*** %lsr.iv68, align 4, !tbaa !32
  %32 = add nuw nsw i32 %ci.01, 1
  %gep_int21 = ptrtoint %struct.jpeg_compress_struct.0* %24 to i32
  %gep22 = add i32 %gep_int21, 52
  %cgep27 = inttoptr i32 %gep22 to i32*
  %33 = load i32, i32* %cgep27, align 4, !tbaa !24
  %34 = icmp slt i32 %32, %33
  %gep_int23 = ptrtoint %struct.jpeg_component_info.5* %lsr.iv to i32
  %gep24 = add i32 %gep_int23, 84
  %cgep14 = inttoptr i32 %gep24 to %struct.jpeg_component_info.5*
  %gep_int25 = ptrtoint i8* %lsr.iv6 to i32
  %gep26 = add i32 %gep_int25, 4
  %cgep15 = inttoptr i32 %gep26 to i8*
  br i1 %34, label %22, label %.loopexit, !exec_freq !33

.loopexit:                                        ; preds = %22, %16, %0
  ret void, !exec_freq !34
}

; Function Attrs: nounwind
declare hidden void @start_pass_main(%struct.jpeg_compress_struct.0*, i8 zeroext) #0

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !3, i64 4}
!2 = !{!"jpeg_compress_struct", !3, i64 0, !3, i64 4, !3, i64 8, !6, i64 12, !6, i64 16, !3, i64 20, !6, i64 24, !6, i64 28, !6, i64 32, !4, i64 36, !7, i64 40, !6, i64 48, !6, i64 52, !4, i64 56, !3, i64 60, !4, i64 64, !4, i64 80, !4, i64 96, !4, i64 112, !4, i64 128, !4, i64 144, !6, i64 160, !3, i64 164, !6, i64 168, !6, i64 172, !6, i64 176, !6, i64 180, !6, i64 184, !4, i64 188, !6, i64 192, !6, i64 196, !6, i64 200, !4, i64 204, !8, i64 206, !8, i64 208, !6, i64 212, !6, i64 216, !6, i64 220, !6, i64 224, !6, i64 228, !6, i64 232, !6, i64 236, !4, i64 240, !6, i64 256, !6, i64 260, !6, i64 264, !4, i64 268, !6, i64 308, !6, i64 312, !6, i64 316, !6, i64 320, !3, i64 324, !3, i64 328, !3, i64 332, !3, i64 336, !3, i64 340, !3, i64 344, !3, i64 348, !3, i64 352, !3, i64 356}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!"int", !4, i64 0}
!7 = !{!"double", !4, i64 0}
!8 = !{!"short", !4, i64 0}
!9 = !{!10, !3, i64 0}
!10 = !{!"jpeg_memory_mgr", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !11, i64 44}
!11 = !{!"long", !4, i64 0}
!12 = !{!2, !3, i64 328}
!13 = !{!14, !3, i64 0}
!14 = !{!"", !15, i64 0, !6, i64 8, !6, i64 12, !6, i64 16, !4, i64 20, !4, i64 24}
!15 = !{!"jpeg_c_main_controller", !3, i64 0, !3, i64 4}
!16 = !{!2, !6, i64 168}
!17 = !{i64 91}
!18 = !{i64 34}
!19 = !{!2, !3, i64 0}
!20 = !{!21, !6, i64 20}
!21 = !{!"jpeg_error_mgr", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !6, i64 20, !4, i64 24, !6, i64 104, !11, i64 108, !3, i64 112, !6, i64 116, !3, i64 120, !6, i64 124, !6, i64 128}
!22 = !{!21, !3, i64 0}
!23 = !{i64 21}
!24 = !{!2, !6, i64 52}
!25 = !{i64 12}
!26 = !{!2, !3, i64 60}
!27 = !{i64 8}
!28 = !{!10, !3, i64 8}
!29 = !{!30, !6, i64 28}
!30 = !{!"", !6, i64 0, !6, i64 4, !6, i64 8, !6, i64 12, !6, i64 16, !6, i64 20, !6, i64 24, !6, i64 28, !6, i64 32, !6, i64 36, !6, i64 40, !6, i64 44, !6, i64 48, !6, i64 52, !6, i64 56, !6, i64 60, !6, i64 64, !6, i64 68, !6, i64 72, !3, i64 76, !3, i64 80}
!31 = !{!30, !6, i64 12}
!32 = !{!3, !3, i64 0}
!33 = !{i64 255}
!34 = !{i64 69}
