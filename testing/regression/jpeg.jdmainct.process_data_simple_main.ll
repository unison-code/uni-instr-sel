; ModuleID = 'jpeg.jdmainct.process_data_simple_main.low.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct.jpeg_decompress_struct.72 = type { %struct.jpeg_error_mgr.73*, %struct.jpeg_memory_mgr.74*, %struct.jpeg_progress_mgr.75*, i32, i32, %struct.jpeg_source_mgr.76*, i32, i32, i32, i8, i8, i32, i32, double, i32, i32, i8, i32, i32, i32, i8, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i8**, i32, i32, i32, i32, i32, [64 x i32]*, [4 x %struct.JQUANT_TBL.77*], [4 x %struct.JHUFF_TBL.78*], [4 x %struct.JHUFF_TBL.78*], i32, %struct.jpeg_component_info.79*, i32, i32, [16 x i8], [16 x i8], [16 x i8], i32, i32, i8, i16, i16, i32, i8, i32, i32, i32, i32, i32, i8*, i32, [4 x %struct.jpeg_component_info.79*], i32, i32, i32, [10 x i32], i32, i32, i32, i32, i32, %struct.jpeg_decomp_master.80*, %struct.jpeg_d_main_controller.81*, %struct.jpeg_d_coef_controller.82*, %struct.jpeg_d_post_controller.83*, %struct.jpeg_input_controller.84*, %struct.jpeg_marker_reader.85*, %struct.jpeg_entropy_decoder.86*, %struct.jpeg_inverse_dct.87*, %struct.jpeg_upsampler.88*, %struct.jpeg_color_deconverter.89*, %struct.jpeg_color_quantizer.90* }
%struct.jpeg_error_mgr.73 = type { void (%struct.jpeg_common_struct.91*)*, void (%struct.jpeg_common_struct.91*, i32)*, void (%struct.jpeg_common_struct.91*)*, void (%struct.jpeg_common_struct.91*, i8*)*, void (%struct.jpeg_common_struct.91*)*, i32, %union.anon.92, i32, i32, i8**, i32, i8**, i32, i32 }
%struct.jpeg_common_struct.91 = type { %struct.jpeg_error_mgr.73*, %struct.jpeg_memory_mgr.74*, %struct.jpeg_progress_mgr.75*, i32, i32 }
%union.anon.92 = type { [8 x i32], [48 x i8] }
%struct.jpeg_memory_mgr.74 = type { i8* (%struct.jpeg_common_struct.91*, i32, i32)*, i8* (%struct.jpeg_common_struct.91*, i32, i32)*, i8** (%struct.jpeg_common_struct.91*, i32, i32, i32)*, [64 x i16]** (%struct.jpeg_common_struct.91*, i32, i32, i32)*, %struct.jvirt_sarray_control.93* (%struct.jpeg_common_struct.91*, i32, i32, i32, i32, i32)*, %struct.jvirt_barray_control.94* (%struct.jpeg_common_struct.91*, i32, i32, i32, i32, i32)*, {}*, i8** (%struct.jpeg_common_struct.91*, %struct.jvirt_sarray_control.93*, i32, i32, i32)*, [64 x i16]** (%struct.jpeg_common_struct.91*, %struct.jvirt_barray_control.94*, i32, i32, i32)*, void (%struct.jpeg_common_struct.91*, i32)*, {}*, i32 }
%struct.jvirt_sarray_control.93 = type opaque
%struct.jvirt_barray_control.94 = type opaque
%struct.jpeg_progress_mgr.75 = type { {}*, i32, i32, i32, i32 }
%struct.jpeg_source_mgr.76 = type { i8*, i32, void (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*, i32)*, i32 (%struct.jpeg_decompress_struct.72*, i32)*, void (%struct.jpeg_decompress_struct.72*)* }
%struct.JQUANT_TBL.77 = type { [64 x i16], i32 }
%struct.JHUFF_TBL.78 = type { [17 x i8], [256 x i8], i32 }
%struct.jpeg_component_info.79 = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.JQUANT_TBL.77*, i8* }
%struct.jpeg_decomp_master.80 = type { void (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*)*, i32 }
%struct.jpeg_d_main_controller.81 = type { void (%struct.jpeg_decompress_struct.72*, i8)*, void (%struct.jpeg_decompress_struct.72*, i8**, i32*, i32)* }
%struct.jpeg_d_coef_controller.82 = type { void (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*, i8***)*, %struct.jvirt_barray_control.94** }
%struct.jpeg_d_post_controller.83 = type { void (%struct.jpeg_decompress_struct.72*, i8)*, void (%struct.jpeg_decompress_struct.72*, i8***, i32*, i32, i8**, i32*, i32)* }
%struct.jpeg_input_controller.84 = type { i32 (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*)*, i32, i32 }
%struct.jpeg_marker_reader.85 = type { void (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*)*, [16 x i32 (%struct.jpeg_decompress_struct.72*)*], i32, i32, i32, i32 }
%struct.jpeg_entropy_decoder.86 = type { void (%struct.jpeg_decompress_struct.72*)*, i32 (%struct.jpeg_decompress_struct.72*, [64 x i16]**)* }
%struct.jpeg_inverse_dct.87 = type { void (%struct.jpeg_decompress_struct.72*)*, [10 x void (%struct.jpeg_decompress_struct.72*, %struct.jpeg_component_info.79*, i16*, i8**, i32)*] }
%struct.jpeg_upsampler.88 = type { void (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*, i8***, i32*, i32, i8**, i32*, i32)*, i32 }
%struct.jpeg_color_deconverter.89 = type { void (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*, i8***, i32, i8**, i32)* }
%struct.jpeg_color_quantizer.90 = type { {}*, void (%struct.jpeg_decompress_struct.72*, i8**, i8**, i32)*, void (%struct.jpeg_decompress_struct.72*)*, void (%struct.jpeg_decompress_struct.72*)* }
%struct.my_main_controller.95 = type { %struct.jpeg_d_main_controller.81, [10 x i8**], i32, i32, [2 x i8***], i32, i32, i32, i32 }

; Function Attrs: nounwind
define hidden void @process_data_simple_main(%struct.jpeg_decompress_struct.72* %cinfo, i8** %output_buf, i32* %out_row_ctr, i32 %out_rows_avail) #0 {
  %cgep23 = bitcast %struct.jpeg_decompress_struct.72* %cinfo to %struct.jpeg_decompress_struct.72*
  %gep_int = ptrtoint %struct.jpeg_decompress_struct.72* %cgep23 to i32
  %gep = add i32 %gep_int, 412
  %cgep4 = inttoptr i32 %gep to %struct.jpeg_d_main_controller.81**
  %1 = bitcast %struct.jpeg_d_main_controller.81** %cgep4 to %struct.my_main_controller.95**
  %2 = load %struct.my_main_controller.95*, %struct.my_main_controller.95** %1, align 4, !tbaa !1
  %cgep124 = bitcast %struct.my_main_controller.95* %2 to %struct.my_main_controller.95*
  %gep_int1 = ptrtoint %struct.my_main_controller.95* %cgep124 to i32
  %gep2 = add i32 %gep_int1, 48
  %cgep14 = inttoptr i32 %gep2 to i32*
  %3 = load i32, i32* %cgep14, align 4, !tbaa !9
  %4 = icmp eq i32 %3, 0
  %gep_int3 = ptrtoint %struct.my_main_controller.95* %cgep124 to i32
  %gep4 = add i32 %gep_int3, 8
  %cgep12 = inttoptr i32 %gep4 to i8***
  br i1 %4, label %5, label %13, !exec_freq !12

; <label>:5                                       ; preds = %0
  %6 = bitcast %struct.jpeg_decompress_struct.72* %cinfo to %struct.jpeg_decompress_struct.72*
  %gep_int5 = ptrtoint %struct.jpeg_decompress_struct.72* %6 to i32
  %gep6 = add i32 %gep_int5, 416
  %cgep8 = inttoptr i32 %gep6 to %struct.jpeg_d_coef_controller.82**
  %7 = load %struct.jpeg_d_coef_controller.82*, %struct.jpeg_d_coef_controller.82** %cgep8, align 8, !tbaa !13
  %cgep225 = bitcast %struct.jpeg_d_coef_controller.82* %7 to %struct.jpeg_d_coef_controller.82*
  %gep_int7 = ptrtoint %struct.jpeg_d_coef_controller.82* %cgep225 to i32
  %gep8 = add i32 %gep_int7, 12
  %cgep20 = inttoptr i32 %gep8 to i32 (%struct.jpeg_decompress_struct.72*, i8***)**
  %8 = load i32 (%struct.jpeg_decompress_struct.72*, i8***)*, i32 (%struct.jpeg_decompress_struct.72*, i8***)** %cgep20, align 4, !tbaa !14
  %9 = tail call i32 %8(%struct.jpeg_decompress_struct.72* nonnull %cinfo, i8*** %cgep12) #1
  %10 = icmp eq i32 %9, 0
  br i1 %10, label %23, label %11, !exec_freq !16

; <label>:11                                      ; preds = %5
  %12 = bitcast %struct.my_main_controller.95* %2 to %struct.my_main_controller.95*
  %gep_int9 = ptrtoint %struct.my_main_controller.95* %12 to i32
  %gep10 = add i32 %gep_int9, 48
  %cgep15 = inttoptr i32 %gep10 to i32*
  store i32 1, i32* %cgep15, align 4, !tbaa !9
  br label %13, !exec_freq !17

; <label>:13                                      ; preds = %11, %0
  %14 = bitcast %struct.my_main_controller.95* %2 to %struct.my_main_controller.95*
  %15 = bitcast %struct.jpeg_decompress_struct.72* %cinfo to %struct.jpeg_decompress_struct.72*
  %gep_int11 = ptrtoint %struct.jpeg_decompress_struct.72* %15 to i32
  %gep12 = add i32 %gep_int11, 304
  %cgep9 = inttoptr i32 %gep12 to i32*
  %16 = load i32, i32* %cgep9, align 8, !tbaa !18
  %gep_int13 = ptrtoint %struct.jpeg_decompress_struct.72* %15 to i32
  %gep14 = add i32 %gep_int13, 420
  %cgep10 = inttoptr i32 %gep14 to %struct.jpeg_d_post_controller.83**
  %17 = load %struct.jpeg_d_post_controller.83*, %struct.jpeg_d_post_controller.83** %cgep10, align 4, !tbaa !19
  %cgep326 = bitcast %struct.jpeg_d_post_controller.83* %17 to %struct.jpeg_d_post_controller.83*
  %gep_int15 = ptrtoint %struct.jpeg_d_post_controller.83* %cgep326 to i32
  %gep16 = add i32 %gep_int15, 4
  %cgep22 = inttoptr i32 %gep16 to void (%struct.jpeg_decompress_struct.72*, i8***, i32*, i32, i8**, i32*, i32)**
  %18 = load void (%struct.jpeg_decompress_struct.72*, i8***, i32*, i32, i8**, i32*, i32)*, void (%struct.jpeg_decompress_struct.72*, i8***, i32*, i32, i8**, i32*, i32)** %cgep22, align 4, !tbaa !20
  %gep_int17 = ptrtoint %struct.my_main_controller.95* %14 to i32
  %gep18 = add i32 %gep_int17, 52
  %cgep13 = inttoptr i32 %gep18 to i32*
  tail call void %18(%struct.jpeg_decompress_struct.72* nonnull %cinfo, i8*** %cgep12, i32* %cgep13, i32 %16, i8** %output_buf, i32* %out_row_ctr, i32 %out_rows_avail) #1
  %gep_int19 = ptrtoint %struct.my_main_controller.95* %14 to i32
  %gep20 = add i32 %gep_int19, 52
  %cgep17 = inttoptr i32 %gep20 to i32*
  %19 = load i32, i32* %cgep17, align 4, !tbaa !22
  %20 = icmp ult i32 %19, %16
  br i1 %20, label %23, label %21, !exec_freq !23

; <label>:21                                      ; preds = %13
  %22 = bitcast %struct.my_main_controller.95* %2 to %struct.my_main_controller.95*
  %gep_int21 = ptrtoint %struct.my_main_controller.95* %22 to i32
  %gep22 = add i32 %gep_int21, 48
  %cgep16 = inttoptr i32 %gep22 to i32*
  store i32 0, i32* %cgep16, align 4, !tbaa !9
  %gep_int23 = ptrtoint %struct.my_main_controller.95* %22 to i32
  %gep24 = add i32 %gep_int23, 52
  %cgep18 = inttoptr i32 %gep24 to i32*
  store i32 0, i32* %cgep18, align 4, !tbaa !22
  br label %23, !exec_freq !24

; <label>:23                                      ; preds = %21, %13, %5
  ret void, !exec_freq !12
}

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !3, i64 412}
!2 = !{!"jpeg_decompress_struct", !3, i64 0, !3, i64 4, !3, i64 8, !6, i64 12, !6, i64 16, !3, i64 20, !6, i64 24, !6, i64 28, !6, i64 32, !4, i64 36, !4, i64 37, !6, i64 40, !6, i64 44, !7, i64 48, !6, i64 56, !6, i64 60, !4, i64 64, !6, i64 68, !6, i64 72, !6, i64 76, !4, i64 80, !6, i64 84, !6, i64 88, !6, i64 92, !6, i64 96, !6, i64 100, !6, i64 104, !6, i64 108, !6, i64 112, !6, i64 116, !6, i64 120, !6, i64 124, !3, i64 128, !6, i64 132, !6, i64 136, !6, i64 140, !6, i64 144, !6, i64 148, !3, i64 152, !4, i64 156, !4, i64 172, !4, i64 188, !6, i64 204, !3, i64 208, !6, i64 212, !6, i64 216, !4, i64 220, !4, i64 236, !4, i64 252, !6, i64 268, !6, i64 272, !4, i64 276, !8, i64 278, !8, i64 280, !6, i64 284, !4, i64 288, !6, i64 292, !6, i64 296, !6, i64 300, !6, i64 304, !6, i64 308, !3, i64 312, !6, i64 316, !4, i64 320, !6, i64 336, !6, i64 340, !6, i64 344, !4, i64 348, !6, i64 388, !6, i64 392, !6, i64 396, !6, i64 400, !6, i64 404, !3, i64 408, !3, i64 412, !3, i64 416, !3, i64 420, !3, i64 424, !3, i64 428, !3, i64 432, !3, i64 436, !3, i64 440, !3, i64 444, !3, i64 448}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!"int", !4, i64 0}
!7 = !{!"double", !4, i64 0}
!8 = !{!"short", !4, i64 0}
!9 = !{!10, !6, i64 48}
!10 = !{!"", !11, i64 0, !4, i64 8, !6, i64 48, !6, i64 52, !4, i64 56, !6, i64 64, !6, i64 68, !6, i64 72, !6, i64 76}
!11 = !{!"jpeg_d_main_controller", !3, i64 0, !3, i64 4}
!12 = !{i64 34}
!13 = !{!2, !3, i64 416}
!14 = !{!15, !3, i64 12}
!15 = !{!"jpeg_d_coef_controller", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16}
!16 = !{i64 12}
!17 = !{i64 8}
!18 = !{!2, !6, i64 304}
!19 = !{!2, !3, i64 420}
!20 = !{!21, !3, i64 4}
!21 = !{!"jpeg_d_post_controller", !3, i64 0, !3, i64 4}
!22 = !{!10, !6, i64 52}
!23 = !{i64 29}
!24 = !{i64 14}
