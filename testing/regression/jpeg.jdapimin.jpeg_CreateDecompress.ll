; ModuleID = 'jpeg.jdapimin.jpeg_CreateDecompress.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.jpeg_decompress_struct.0 = type { %struct.jpeg_error_mgr.1*, %struct.jpeg_memory_mgr.2*, %struct.jpeg_progress_mgr.3*, i32, i32, %struct.jpeg_source_mgr.4*, i32, i32, i32, i32, i32, i32, i32, double, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i8**, i32, i32, i32, i32, i32, [64 x i32]*, [4 x %struct.JQUANT_TBL.5*], [4 x %struct.JHUFF_TBL.6*], [4 x %struct.JHUFF_TBL.6*], i32, %struct.jpeg_component_info.7*, i32, i32, [16 x i8], [16 x i8], [16 x i8], i32, i32, i8, i16, i16, i32, i8, i32, i32, i32, i32, i32, i8*, i32, [4 x %struct.jpeg_component_info.7*], i32, i32, i32, [10 x i32], i32, i32, i32, i32, i32, %struct.jpeg_decomp_master.8*, %struct.jpeg_d_main_controller.9*, %struct.jpeg_d_coef_controller.10*, %struct.jpeg_d_post_controller.11*, %struct.jpeg_input_controller.12*, %struct.jpeg_marker_reader.13*, %struct.jpeg_entropy_decoder.14*, %struct.jpeg_inverse_dct.15*, %struct.jpeg_upsampler.16*, %struct.jpeg_color_deconverter.17*, %struct.jpeg_color_quantizer.18* }
%struct.jpeg_error_mgr.1 = type { void (%struct.jpeg_common_struct.19*)*, void (%struct.jpeg_common_struct.19*, i32)*, void (%struct.jpeg_common_struct.19*)*, void (%struct.jpeg_common_struct.19*, i8*)*, void (%struct.jpeg_common_struct.19*)*, i32, %union.anon.20, i32, i32, i8**, i32, i8**, i32, i32 }
%struct.jpeg_common_struct.19 = type { %struct.jpeg_error_mgr.1*, %struct.jpeg_memory_mgr.2*, %struct.jpeg_progress_mgr.3*, i32, i32 }
%union.anon.20 = type { [8 x i32], [48 x i8] }
%struct.jpeg_memory_mgr.2 = type { i8* (%struct.jpeg_common_struct.19*, i32, i32)*, i8* (%struct.jpeg_common_struct.19*, i32, i32)*, i8** (%struct.jpeg_common_struct.19*, i32, i32, i32)*, [64 x i16]** (%struct.jpeg_common_struct.19*, i32, i32, i32)*, %struct.jvirt_sarray_control.21* (%struct.jpeg_common_struct.19*, i32, i32, i32, i32, i32)*, %struct.jvirt_barray_control.22* (%struct.jpeg_common_struct.19*, i32, i32, i32, i32, i32)*, {}*, i8** (%struct.jpeg_common_struct.19*, %struct.jvirt_sarray_control.21*, i32, i32, i32)*, [64 x i16]** (%struct.jpeg_common_struct.19*, %struct.jvirt_barray_control.22*, i32, i32, i32)*, void (%struct.jpeg_common_struct.19*, i32)*, {}*, i32 }
%struct.jvirt_sarray_control.21 = type opaque
%struct.jvirt_barray_control.22 = type opaque
%struct.jpeg_progress_mgr.3 = type { {}*, i32, i32, i32, i32 }
%struct.jpeg_source_mgr.4 = type { i8*, i32, void (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*, i32)*, i32 (%struct.jpeg_decompress_struct.0*, i32)*, void (%struct.jpeg_decompress_struct.0*)* }
%struct.JQUANT_TBL.5 = type { [64 x i16], i32 }
%struct.JHUFF_TBL.6 = type { [17 x i8], [256 x i8], i32 }
%struct.jpeg_component_info.7 = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.JQUANT_TBL.5*, i8* }
%struct.jpeg_decomp_master.8 = type { void (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*)*, i32 }
%struct.jpeg_d_main_controller.9 = type { void (%struct.jpeg_decompress_struct.0*, i32)*, void (%struct.jpeg_decompress_struct.0*, i8**, i32*, i32)* }
%struct.jpeg_d_coef_controller.10 = type { void (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*, i8***)*, %struct.jvirt_barray_control.22** }
%struct.jpeg_d_post_controller.11 = type { void (%struct.jpeg_decompress_struct.0*, i32)*, void (%struct.jpeg_decompress_struct.0*, i8***, i32*, i32, i8**, i32*, i32)* }
%struct.jpeg_input_controller.12 = type { i32 (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*)*, i32, i32 }
%struct.jpeg_marker_reader.13 = type { void (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*)*, [16 x i32 (%struct.jpeg_decompress_struct.0*)*], i32, i32, i32, i32 }
%struct.jpeg_entropy_decoder.14 = type { void (%struct.jpeg_decompress_struct.0*)*, i32 (%struct.jpeg_decompress_struct.0*, [64 x i16]**)* }
%struct.jpeg_inverse_dct.15 = type { void (%struct.jpeg_decompress_struct.0*)*, [10 x void (%struct.jpeg_decompress_struct.0*, %struct.jpeg_component_info.7*, i16*, i8**, i32)*] }
%struct.jpeg_upsampler.16 = type { void (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*, i8***, i32*, i32, i8**, i32*, i32)*, i32 }
%struct.jpeg_color_deconverter.17 = type { void (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*, i8***, i32, i8**, i32)* }
%struct.jpeg_color_quantizer.18 = type { void (%struct.jpeg_decompress_struct.0*, i32)*, void (%struct.jpeg_decompress_struct.0*, i8**, i8**, i32)*, void (%struct.jpeg_decompress_struct.0*)*, void (%struct.jpeg_decompress_struct.0*)* }

; Function Attrs: nounwind
define void @jpeg_CreateDecompress(%struct.jpeg_decompress_struct.0* %cinfo, i32 signext %version, i32 signext %structsize) #0 {
  %gep_int = ptrtoint %struct.jpeg_decompress_struct.0* %cinfo to i32
  %gep = add i32 %gep_int, 4
  %1 = inttoptr i32 %gep to %struct.jpeg_memory_mgr.2**
  store %struct.jpeg_memory_mgr.2* null, %struct.jpeg_memory_mgr.2** %1, align 4, !tbaa !1
  %2 = icmp eq i32 %version, 61
  br i1 %2, label %12, label %3, !exec_freq !9

; <label>:3                                       ; preds = %0
  %4 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_error_mgr.1**
  %5 = load %struct.jpeg_error_mgr.1*, %struct.jpeg_error_mgr.1** %4, align 8, !tbaa !10
  %gep_int1 = ptrtoint %struct.jpeg_error_mgr.1* %5 to i32
  %gep2 = add i32 %gep_int1, 20
  %6 = inttoptr i32 %gep2 to i32*
  store i32 10, i32* %6, align 4, !tbaa !11
  %gep_int3 = ptrtoint %struct.jpeg_error_mgr.1* %5 to i32
  %gep4 = add i32 %gep_int3, 24
  %7 = inttoptr i32 %gep4 to i32*
  store i32 61, i32* %7, align 4, !tbaa !14
  %gep_int5 = ptrtoint %struct.jpeg_error_mgr.1* %5 to i32
  %gep6 = add i32 %gep_int5, 28
  %8 = inttoptr i32 %gep6 to i32*
  store i32 %version, i32* %8, align 4, !tbaa !14
  %9 = bitcast %struct.jpeg_error_mgr.1* %5 to void (%struct.jpeg_common_struct.19*)**
  %10 = load void (%struct.jpeg_common_struct.19*)*, void (%struct.jpeg_common_struct.19*)** %9, align 4, !tbaa !15
  %11 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_common_struct.19*
  tail call void %10(%struct.jpeg_common_struct.19* %11) #3
  br label %12, !exec_freq !16

; <label>:12                                      ; preds = %3, %0
  %13 = icmp eq i32 %structsize, 464
  br i1 %13, label %..preheader.preheader_crit_edge, label %14, !exec_freq !9

..preheader.preheader_crit_edge:                  ; preds = %12
  %.pre = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_common_struct.19*
  br label %.preheader.preheader, !exec_freq !16

; <label>:14                                      ; preds = %12
  %15 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_error_mgr.1**
  %16 = load %struct.jpeg_error_mgr.1*, %struct.jpeg_error_mgr.1** %15, align 8, !tbaa !10
  %gep_int7 = ptrtoint %struct.jpeg_error_mgr.1* %16 to i32
  %gep8 = add i32 %gep_int7, 20
  %17 = inttoptr i32 %gep8 to i32*
  store i32 19, i32* %17, align 4, !tbaa !11
  %gep_int9 = ptrtoint %struct.jpeg_error_mgr.1* %16 to i32
  %gep10 = add i32 %gep_int9, 24
  %18 = inttoptr i32 %gep10 to i32*
  store i32 464, i32* %18, align 4, !tbaa !14
  %gep_int11 = ptrtoint %struct.jpeg_error_mgr.1* %16 to i32
  %gep12 = add i32 %gep_int11, 28
  %19 = inttoptr i32 %gep12 to i32*
  store i32 %structsize, i32* %19, align 4, !tbaa !14
  %20 = bitcast %struct.jpeg_error_mgr.1* %16 to void (%struct.jpeg_common_struct.19*)**
  %21 = load void (%struct.jpeg_common_struct.19*)*, void (%struct.jpeg_common_struct.19*)** %20, align 4, !tbaa !15
  %22 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_common_struct.19*
  tail call void %21(%struct.jpeg_common_struct.19* %22) #3
  br label %.preheader.preheader, !exec_freq !16

.preheader.preheader:                             ; preds = %14, %..preheader.preheader_crit_edge
  %.pre-phi = phi %struct.jpeg_common_struct.19* [ %.pre, %..preheader.preheader_crit_edge ], [ %22, %14 ]
  %23 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to i32*
  %24 = load i32, i32* %23, align 8, !tbaa !10
  %25 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to i8*
  store i32 %24, i32* %23, align 8, !tbaa !10
  %gep_int13 = ptrtoint %struct.jpeg_decompress_struct.0* %cinfo to i32
  %gep14 = add i32 %gep_int13, 12
  %26 = inttoptr i32 %gep14 to i32*
  store i32 1, i32* %26, align 4, !tbaa !17
  tail call void @jinit_memory_mgr(%struct.jpeg_common_struct.19* %.pre-phi) #3
  %gep_int15 = ptrtoint %struct.jpeg_decompress_struct.0* %cinfo to i32
  %gep16 = add i32 %gep_int15, 8
  %27 = inttoptr i32 %gep16 to %struct.jpeg_progress_mgr.3**
  store %struct.jpeg_progress_mgr.3* null, %struct.jpeg_progress_mgr.3** %27, align 8, !tbaa !18
  %gep_int17 = ptrtoint %struct.jpeg_decompress_struct.0* %cinfo to i32
  %gep18 = add i32 %gep_int17, 20
  %28 = inttoptr i32 %gep18 to %struct.jpeg_source_mgr.4**
  store %struct.jpeg_source_mgr.4* null, %struct.jpeg_source_mgr.4** %28, align 4, !tbaa !19
  %gep_int19 = ptrtoint %struct.jpeg_decompress_struct.0* %cinfo to i32
  %gep20 = add i32 %gep_int19, 164
  %scevgep = inttoptr i32 %gep20 to %struct.JQUANT_TBL.5**
  %29 = bitcast %struct.JQUANT_TBL.5** %scevgep to i8*
  tail call void @jinit_marker_reader(%struct.jpeg_decompress_struct.0* nonnull %cinfo) #3
  tail call void @jinit_input_controller(%struct.jpeg_decompress_struct.0* nonnull %cinfo) #3
  %gep_int21 = ptrtoint %struct.jpeg_decompress_struct.0* %cinfo to i32
  %gep22 = add i32 %gep_int21, 16
  %30 = inttoptr i32 %gep22 to i32*
  store i32 200, i32* %30, align 8, !tbaa !20
  ret void, !exec_freq !9
}


declare void @jinit_memory_mgr(%struct.jpeg_common_struct.19*) #2

declare void @jinit_marker_reader(%struct.jpeg_decompress_struct.0*) #2

declare void @jinit_input_controller(%struct.jpeg_decompress_struct.0*) #2

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
attributes #2 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !3, i64 4}
!2 = !{!"jpeg_decompress_struct", !3, i64 0, !3, i64 4, !3, i64 8, !6, i64 12, !6, i64 16, !3, i64 20, !6, i64 24, !6, i64 28, !6, i64 32, !4, i64 36, !4, i64 40, !6, i64 44, !6, i64 48, !7, i64 56, !6, i64 64, !6, i64 68, !4, i64 72, !6, i64 76, !6, i64 80, !6, i64 84, !4, i64 88, !6, i64 92, !6, i64 96, !6, i64 100, !6, i64 104, !6, i64 108, !6, i64 112, !6, i64 116, !6, i64 120, !6, i64 124, !6, i64 128, !6, i64 132, !3, i64 136, !6, i64 140, !6, i64 144, !6, i64 148, !6, i64 152, !6, i64 156, !3, i64 160, !4, i64 164, !4, i64 180, !4, i64 196, !6, i64 212, !3, i64 216, !6, i64 220, !6, i64 224, !4, i64 228, !4, i64 244, !4, i64 260, !6, i64 276, !6, i64 280, !4, i64 284, !8, i64 286, !8, i64 288, !6, i64 292, !4, i64 296, !6, i64 300, !6, i64 304, !6, i64 308, !6, i64 312, !6, i64 316, !3, i64 320, !6, i64 324, !4, i64 328, !6, i64 344, !6, i64 348, !6, i64 352, !4, i64 356, !6, i64 396, !6, i64 400, !6, i64 404, !6, i64 408, !6, i64 412, !3, i64 416, !3, i64 420, !3, i64 424, !3, i64 428, !3, i64 432, !3, i64 436, !3, i64 440, !3, i64 444, !3, i64 448, !3, i64 452, !3, i64 456}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!"int", !4, i64 0}
!7 = !{!"double", !4, i64 0}
!8 = !{!"short", !4, i64 0}
!9 = !{i64 16}
!10 = !{!2, !3, i64 0}
!11 = !{!12, !6, i64 20}
!12 = !{!"jpeg_error_mgr", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !6, i64 20, !4, i64 24, !6, i64 104, !13, i64 108, !3, i64 112, !6, i64 116, !3, i64 120, !6, i64 124, !6, i64 128}
!13 = !{!"long", !4, i64 0}
!14 = !{!6, !6, i64 0}
!15 = !{!12, !3, i64 0}
!16 = !{i64 8}
!17 = !{!2, !6, i64 12}
!18 = !{!2, !3, i64 8}
!19 = !{!2, !3, i64 20}
!20 = !{!2, !6, i64 16}
