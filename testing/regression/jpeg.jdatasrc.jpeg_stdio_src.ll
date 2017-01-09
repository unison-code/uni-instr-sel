; ModuleID = 'jpeg.jdatasrc.jpeg_stdio_src.low.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct.jpeg_decompress_struct.0 = type { %struct.jpeg_error_mgr.1*, %struct.jpeg_memory_mgr.2*, %struct.jpeg_progress_mgr.3*, i32, i32, %struct.jpeg_source_mgr.4*, i32, i32, i32, i8, i8, i32, i32, double, i32, i32, i8, i32, i32, i32, i8, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i8**, i32, i32, i32, i32, i32, [64 x i32]*, [4 x %struct.JQUANT_TBL.5*], [4 x %struct.JHUFF_TBL.6*], [4 x %struct.JHUFF_TBL.6*], i32, %struct.jpeg_component_info.7*, i32, i32, [16 x i8], [16 x i8], [16 x i8], i32, i32, i8, i16, i16, i32, i8, i32, i32, i32, i32, i32, i8*, i32, [4 x %struct.jpeg_component_info.7*], i32, i32, i32, [10 x i32], i32, i32, i32, i32, i32, %struct.jpeg_decomp_master.8*, %struct.jpeg_d_main_controller.9*, %struct.jpeg_d_coef_controller.10*, %struct.jpeg_d_post_controller.11*, %struct.jpeg_input_controller.12*, %struct.jpeg_marker_reader.13*, %struct.jpeg_entropy_decoder.14*, %struct.jpeg_inverse_dct.15*, %struct.jpeg_upsampler.16*, %struct.jpeg_color_deconverter.17*, %struct.jpeg_color_quantizer.18* }
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
%struct.jpeg_decomp_master.8 = type opaque
%struct.jpeg_d_main_controller.9 = type opaque
%struct.jpeg_d_coef_controller.10 = type opaque
%struct.jpeg_d_post_controller.11 = type opaque
%struct.jpeg_input_controller.12 = type opaque
%struct.jpeg_marker_reader.13 = type opaque
%struct.jpeg_entropy_decoder.14 = type opaque
%struct.jpeg_inverse_dct.15 = type opaque
%struct.jpeg_upsampler.16 = type opaque
%struct.jpeg_color_deconverter.17 = type opaque
%struct.jpeg_color_quantizer.18 = type opaque
%struct._IO_FILE.23 = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker.24*, %struct._IO_FILE.23*, i32, i32, i32, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i32, i32, [40 x i8] }
%struct._IO_marker.24 = type { %struct._IO_marker.24*, %struct._IO_FILE.23*, i32 }
%struct.my_source_mgr.25 = type { %struct.jpeg_source_mgr.4, %struct._IO_FILE.23*, i8*, i32 }

; Function Attrs: nounwind
define void @jpeg_stdio_src(%struct.jpeg_decompress_struct.0* %cinfo, %struct._IO_FILE.23* %infile) #0 {
  %cgep31 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_decompress_struct.0*
  %gep_int = ptrtoint %struct.jpeg_decompress_struct.0* %cgep31 to i32
  %gep = add i32 %gep_int, 20
  %cgep7 = inttoptr i32 %gep to %struct.jpeg_source_mgr.4**
  %1 = load %struct.jpeg_source_mgr.4*, %struct.jpeg_source_mgr.4** %cgep7, align 4, !tbaa !1
  %2 = icmp eq %struct.jpeg_source_mgr.4* %1, null
  br i1 %2, label %4, label %._crit_edge, !exec_freq !9

._crit_edge:                                      ; preds = %0
  %3 = bitcast %struct.jpeg_source_mgr.4* %1 to %struct.my_source_mgr.25*
  br label %15, !exec_freq !10

; <label>:4                                       ; preds = %0
  %5 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_decompress_struct.0*
  %6 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_decompress_struct.0*
  %gep_int1 = ptrtoint %struct.jpeg_decompress_struct.0* %6 to i32
  %gep2 = add i32 %gep_int1, 4
  %cgep8 = inttoptr i32 %gep2 to %struct.jpeg_memory_mgr.2**
  %7 = load %struct.jpeg_memory_mgr.2*, %struct.jpeg_memory_mgr.2** %cgep8, align 4, !tbaa !11
  %cgep132 = bitcast %struct.jpeg_memory_mgr.2* %7 to %struct.jpeg_memory_mgr.2*
  %cgep1133 = bitcast %struct.jpeg_memory_mgr.2* %cgep132 to i8* (%struct.jpeg_common_struct.19*, i32, i32)**
  %8 = load i8* (%struct.jpeg_common_struct.19*, i32, i32)*, i8* (%struct.jpeg_common_struct.19*, i32, i32)** %cgep1133, align 4, !tbaa !12
  %9 = bitcast %struct.jpeg_decompress_struct.0* %cinfo to %struct.jpeg_common_struct.19*
  %10 = tail call i8* %8(%struct.jpeg_common_struct.19* %9, i32 0, i32 40) #4
  %sunkaddr = ptrtoint %struct.jpeg_decompress_struct.0* %5 to i32
  %sunkaddr34 = add i32 %sunkaddr, 20
  %sunkaddr35 = inttoptr i32 %sunkaddr34 to i8**
  store i8* %10, i8** %sunkaddr35, align 4, !tbaa !1
  %gep_int3 = ptrtoint %struct.jpeg_decompress_struct.0* %6 to i32
  %gep4 = add i32 %gep_int3, 4
  %cgep9 = inttoptr i32 %gep4 to %struct.jpeg_memory_mgr.2**
  %11 = load %struct.jpeg_memory_mgr.2*, %struct.jpeg_memory_mgr.2** %cgep9, align 4, !tbaa !11
  %cgep236 = bitcast %struct.jpeg_memory_mgr.2* %11 to %struct.jpeg_memory_mgr.2*
  %cgep1337 = bitcast %struct.jpeg_memory_mgr.2* %cgep236 to i8* (%struct.jpeg_common_struct.19*, i32, i32)**
  %12 = load i8* (%struct.jpeg_common_struct.19*, i32, i32)*, i8* (%struct.jpeg_common_struct.19*, i32, i32)** %cgep1337, align 4, !tbaa !12
  %13 = tail call i8* %12(%struct.jpeg_common_struct.19* %9, i32 0, i32 4096) #4
  %gep_int5 = ptrtoint i8* %10 to i32
  %gep6 = add i32 %gep_int5, 32
  %cgep3 = inttoptr i32 %gep6 to i8*
  %14 = bitcast i8* %cgep3 to i8**
  store i8* %13, i8** %14, align 4, !tbaa !15
  %sunkaddr38 = ptrtoint %struct.jpeg_decompress_struct.0* %5 to i32
  %sunkaddr39 = add i32 %sunkaddr38, 20
  %sunkaddr40 = inttoptr i32 %sunkaddr39 to %struct.my_source_mgr.25**
  %.pre = load %struct.my_source_mgr.25*, %struct.my_source_mgr.25** %sunkaddr40, align 4, !tbaa !1
  br label %15, !exec_freq !18

; <label>:15                                      ; preds = %4, %._crit_edge
  %16 = phi %struct.my_source_mgr.25* [ %.pre, %4 ], [ %3, %._crit_edge ]
  %cgep441 = bitcast %struct.my_source_mgr.25* %16 to %struct.my_source_mgr.25*
  %gep_int7 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep8 = add i32 %gep_int7, 8
  %cgep16 = inttoptr i32 %gep8 to void (%struct.jpeg_decompress_struct.0*)**
  store void (%struct.jpeg_decompress_struct.0*)* @init_source, void (%struct.jpeg_decompress_struct.0*)** %cgep16, align 4, !tbaa !19
  %gep_int9 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep10 = add i32 %gep_int9, 12
  %cgep17 = inttoptr i32 %gep10 to i32 (%struct.jpeg_decompress_struct.0*)**
  store i32 (%struct.jpeg_decompress_struct.0*)* @fill_input_buffer, i32 (%struct.jpeg_decompress_struct.0*)** %cgep17, align 4, !tbaa !20
  %gep_int11 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep12 = add i32 %gep_int11, 16
  %cgep18 = inttoptr i32 %gep12 to void (%struct.jpeg_decompress_struct.0*, i32)**
  store void (%struct.jpeg_decompress_struct.0*, i32)* @skip_input_data, void (%struct.jpeg_decompress_struct.0*, i32)** %cgep18, align 4, !tbaa !21
  %gep_int13 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep14 = add i32 %gep_int13, 20
  %cgep19 = inttoptr i32 %gep14 to i32 (%struct.jpeg_decompress_struct.0*, i32)**
  store i32 (%struct.jpeg_decompress_struct.0*, i32)* @jpeg_resync_to_restart, i32 (%struct.jpeg_decompress_struct.0*, i32)** %cgep19, align 4, !tbaa !22
  %gep_int15 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep16 = add i32 %gep_int15, 24
  %cgep20 = inttoptr i32 %gep16 to void (%struct.jpeg_decompress_struct.0*)**
  store void (%struct.jpeg_decompress_struct.0*)* @term_source, void (%struct.jpeg_decompress_struct.0*)** %cgep20, align 4, !tbaa !23
  %gep_int17 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep18 = add i32 %gep_int17, 28
  %cgep21 = inttoptr i32 %gep18 to %struct._IO_FILE.23**
  store %struct._IO_FILE.23* %infile, %struct._IO_FILE.23** %cgep21, align 4, !tbaa !24
  %gep_int19 = ptrtoint %struct.my_source_mgr.25* %cgep441 to i32
  %gep20 = add i32 %gep_int19, 4
  %cgep22 = inttoptr i32 %gep20 to i32*
  store i32 0, i32* %cgep22, align 4, !tbaa !25
  %cgep2342 = bitcast %struct.my_source_mgr.25* %cgep441 to i8**
  store i8* null, i8** %cgep2342, align 4, !tbaa !26
  ret void, !exec_freq !9
}

; Function Attrs: norecurse nounwind
declare hidden void @init_source(%struct.jpeg_decompress_struct.0* nocapture readonly) #1

; Function Attrs: nounwind
declare hidden i32 @fill_input_buffer(%struct.jpeg_decompress_struct.0*) #0

; Function Attrs: nounwind
declare hidden void @skip_input_data(%struct.jpeg_decompress_struct.0*, i32) #0

declare i32 @jpeg_resync_to_restart(%struct.jpeg_decompress_struct.0*, i32) #2

; Function Attrs: norecurse nounwind readnone
declare hidden void @term_source(%struct.jpeg_decompress_struct.0* nocapture) #3

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { norecurse nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { norecurse nounwind readnone "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !3, i64 20}
!2 = !{!"jpeg_decompress_struct", !3, i64 0, !3, i64 4, !3, i64 8, !6, i64 12, !6, i64 16, !3, i64 20, !6, i64 24, !6, i64 28, !6, i64 32, !4, i64 36, !4, i64 37, !6, i64 40, !6, i64 44, !7, i64 48, !6, i64 56, !6, i64 60, !4, i64 64, !6, i64 68, !6, i64 72, !6, i64 76, !4, i64 80, !6, i64 84, !6, i64 88, !6, i64 92, !6, i64 96, !6, i64 100, !6, i64 104, !6, i64 108, !6, i64 112, !6, i64 116, !6, i64 120, !6, i64 124, !3, i64 128, !6, i64 132, !6, i64 136, !6, i64 140, !6, i64 144, !6, i64 148, !3, i64 152, !4, i64 156, !4, i64 172, !4, i64 188, !6, i64 204, !3, i64 208, !6, i64 212, !6, i64 216, !4, i64 220, !4, i64 236, !4, i64 252, !6, i64 268, !6, i64 272, !4, i64 276, !8, i64 278, !8, i64 280, !6, i64 284, !4, i64 288, !6, i64 292, !6, i64 296, !6, i64 300, !6, i64 304, !6, i64 308, !3, i64 312, !6, i64 316, !4, i64 320, !6, i64 336, !6, i64 340, !6, i64 344, !4, i64 348, !6, i64 388, !6, i64 392, !6, i64 396, !6, i64 400, !6, i64 404, !3, i64 408, !3, i64 412, !3, i64 416, !3, i64 420, !3, i64 424, !3, i64 428, !3, i64 432, !3, i64 436, !3, i64 440, !3, i64 444, !3, i64 448}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!"int", !4, i64 0}
!7 = !{!"double", !4, i64 0}
!8 = !{!"short", !4, i64 0}
!9 = !{i64 21}
!10 = !{i64 13}
!11 = !{!2, !3, i64 4}
!12 = !{!13, !3, i64 0}
!13 = !{!"jpeg_memory_mgr", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !14, i64 44}
!14 = !{!"long", !4, i64 0}
!15 = !{!16, !3, i64 32}
!16 = !{!"", !17, i64 0, !3, i64 28, !3, i64 32, !6, i64 36}
!17 = !{!"jpeg_source_mgr", !3, i64 0, !6, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 24}
!18 = !{i64 8}
!19 = !{!16, !3, i64 8}
!20 = !{!16, !3, i64 12}
!21 = !{!16, !3, i64 16}
!22 = !{!16, !3, i64 20}
!23 = !{!16, !3, i64 24}
!24 = !{!16, !3, i64 28}
!25 = !{!16, !6, i64 4}
!26 = !{!16, !3, i64 0}
