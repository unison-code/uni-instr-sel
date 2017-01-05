; ModuleID = 'mesa.dlist.gl_save_Accum.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%union.node.86 = type { i32 }
%struct.gl_context.87 = type { %struct.gl_shared_state.88*, %struct.api_function_table.89, %struct.api_function_table.89, %struct.api_function_table.89, %struct.gl_visual.90*, %struct.gl_frame_buffer.91*, %struct.dd_function_table.92, i8*, [16 x float], [16 x float], i8, i32, [32 x [16 x float]], [16 x float], i32, [32 x [16 x float]], [16 x float], i8, i32, [10 x [16 x float]], i32, i8, i8, i32, [16 x %struct.gl_attrib_node.93*], %struct.gl_accum_attrib.94, %struct.gl_colorbuffer_attrib.95, %struct.gl_current_attrib.96, %struct.gl_depthbuffer_attrib.97, %struct.gl_eval_attrib.98, %struct.gl_fog_attrib.99, %struct.gl_hint_attrib.100, %struct.gl_light_attrib.101, %struct.gl_line_attrib.102, %struct.gl_list_attrib.103, %struct.gl_pixel_attrib.104, %struct.gl_point_attrib.105, %struct.gl_polygon_attrib.106, [32 x i32], %struct.gl_scissor_attrib.107, %struct.gl_stencil_attrib.108, %struct.gl_texture_attrib.109, %struct.gl_transform_attrib.110, %struct.gl_viewport_attrib.111, i32, [16 x %struct.gl_attrib_node.93*], %struct.gl_array_attrib.112, %struct.gl_pixelstore_attrib.113, %struct.gl_pixelstore_attrib.113, %struct.gl_evaluators.114, %struct.gl_feedback.115, %struct.gl_selection.116, i32, i32, i32, i32, i32, i32, i32, i32, i8, float, float, float, i8, i8, i8, i8, i32, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32*, i32)*, void (%struct.gl_context.87*, i32, i32*, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32)*, %struct.vertex_buffer.117*, %struct.pixel_buffer.118*, i8 }
%struct.gl_shared_state.88 = type { i32, [7000 x %union.node.86*], %struct.gl_texture_object.119* }
%struct.gl_texture_object.119 = type { i32, i32, i32, float, [4 x i32], i32, i32, i32, i32, i32, [11 x %struct.gl_texture_image.120*], i8, %struct.gl_texture_object.119* }
%struct.gl_texture_image.120 = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i8* }
%struct.api_function_table.89 = type { {}*, {}*, i8 (%struct.gl_context.87*, i32, i32*, i8*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32, i32, float, float, float, float, %struct.gl_image.121*)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i8*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, double)*, void (%struct.gl_context.87*, float)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, float*)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, i8, i8, i8, i8)*, void (%struct.gl_context.87*, i8, i8, i8, i8)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32, i32*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i8)*, void (%struct.gl_context.87*, double, double)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i8)*, void (%struct.gl_context.87*, i32, i8*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, float)*, void (%struct.gl_context.87*, float, float)*, void (%struct.gl_context.87*, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32, float*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, double, double, double, double, double, double)*, i32 (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32*)*, void (%struct.gl_context.87*, i32, i8*)*, void (%struct.gl_context.87*, i32, double*)*, void (%struct.gl_context.87*, i32, double*)*, i32 (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32, float*)*, void (%struct.gl_context.87*, i32, i32*)*, i8* (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32, double*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, float*)*, void (%struct.gl_context.87*, i32, i32*)*, void (%struct.gl_context.87*, i32, i16*)*, void (%struct.gl_context.87*, i32, i8**)*, void (%struct.gl_context.87*, i8*)*, void (%struct.gl_context.87*, i32, i32*, float*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32, double*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, float)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i8*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32, i32, i8*)*, i8 (%struct.gl_context.87*, i32)*, i8 (%struct.gl_context.87*, i32)*, i8 (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, float*)*, void (%struct.gl_context.87*, i32, i32, float*, i32)*, void (%struct.gl_context.87*, i32, i16)*, void (%struct.gl_context.87*, float)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, float*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, float, float, i32, i32, float*, i8)*, void (%struct.gl_context.87*, i32, float, float, i32, i32, float, float, i32, i32, float*, i8)*, void (%struct.gl_context.87*, i32, float, float)*, void (%struct.gl_context.87*, i32, float, float, i32, float, float)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, float*)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, float, float, float)*, void (%struct.gl_context.87*, float*)*, void (%struct.gl_context.87*, i32, i32, i8*)*, void (%struct.gl_context.87*, float)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32)*, {}*, void (%struct.gl_context.87*, float, float)*, void (%struct.gl_context.87*, float)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, float, float)*, void (%struct.gl_context.87*, i8*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, float, float, float, float)*, i32 (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, float, float, float)*, void (%struct.gl_context.87*, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32, i32, i32)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32, i32, float*)*, void (%struct.gl_context.87*, float, float, float)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i32)*, void (%struct.gl_context.87*, float, float, float, float)*, void (%struct.gl_context.87*)* }
%struct.gl_image.121 = type { i32, i32, i32, i32, i32, i32, i8*, i8, i32 }
%struct.gl_visual.90 = type { i8, i8, float, float, float, float, i8, float, float, float, float, i32, i32, i32, i32, i8, i8 }
%struct.gl_frame_buffer.91 = type { %struct.gl_visual.90*, i32, i32, i16*, i8*, i16*, i8*, i8*, i8*, i32, i32, i32, i32 }
%struct.dd_function_table.92 = type { void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i8, i8, i8, i8)*, void (%struct.gl_context.87*, i8, i32, i32, i32, i32)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i8, i8, i8, i8)*, i8 (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i32*, i32*)*, void (%struct.gl_context.87*, i32, i32, i32, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i32*, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i32*, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, i32*)*, void (%struct.gl_context.87*, i32, i32, i32, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i32*, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, i8 (%struct.gl_context.87*, i32)*, i8 (%struct.gl_context.87*, i8, i8, i8, i8)*, i8 (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*, i8)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*)*, i32 (%struct.gl_context.87*, i32, i32, i32, i16*, i8*)*, void (%struct.gl_context.87*, i32, i32*, i32*, i16*, i8*)*, void (%struct.gl_context.87*, i32, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32, i16*)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32)*, void (%struct.gl_context.87*, i32, i32, i32, i32)*, i8 (%struct.gl_context.87*, i32, i32, i32, i32, i32, i32, i8, i8*)*, i8 (%struct.gl_context.87*, i32, i32, float, float, float, float, %struct.gl_image.121*)*, void (%struct.gl_context.87*, i32)*, void (%struct.gl_context.87*)*, void (%struct.gl_context.87*, i32, float*)*, void (%struct.gl_context.87*, i32, i32, i32, i32, %struct.gl_texture_image.120*)*, void (%struct.gl_context.87*, i32, i32, i32, float*)*, void (%struct.gl_context.87*, i32, i32)*, void (%struct.gl_context.87*, i32)* }
%struct.gl_accum_attrib.94 = type { [4 x float] }
%struct.gl_colorbuffer_attrib.95 = type { i32, [4 x float], i32, i32, i8, i32, i8, i32, float, i8, i8, i32, i32, i32, [4 x float], i32, i8, i8, i8, i8 }
%struct.gl_current_attrib.96 = type { [4 x i32], i32, [3 x float], [4 x float], [4 x float], float, [4 x float], i32, [4 x float], i8, i8 }
%struct.gl_depthbuffer_attrib.97 = type { i32, float, i8, i8 }
%struct.gl_eval_attrib.98 = type { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i32, float, float, i32, i32, float, float, float, float }
%struct.gl_fog_attrib.99 = type { i8, [4 x float], float, float, float, float, i32 }
%struct.gl_hint_attrib.100 = type { i32, i32, i32, i32, i32 }
%struct.gl_light_attrib.101 = type { [8 x %struct.gl_light.122], %struct.gl_lightmodel.123, [2 x %struct.gl_material.124], i8, i32, i32, i32, i32, i8, %struct.gl_light.122*, i8, [4 x float] }
%struct.gl_light.122 = type { [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], float, float, float, float, float, float, i8, %struct.gl_light.122*, [3 x float], [3 x float], [3 x float], [512 x [2 x float]], [3 x float], [3 x float], [3 x float], float, float }
%struct.gl_lightmodel.123 = type { [4 x float], i8, i8 }
%struct.gl_material.124 = type { [4 x float], [4 x float], [4 x float], [4 x float], float, float, float, float, [200 x float] }
%struct.gl_line_attrib.102 = type { i8, i8, i16, i32, float }
%struct.gl_list_attrib.103 = type { i32 }
%struct.gl_pixel_attrib.104 = type { i32, float, float, float, float, float, float, float, float, float, float, i32, i32, i8, i8, float, float, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, [256 x i32], [256 x i32], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float] }
%struct.gl_point_attrib.105 = type { i8, float }
%struct.gl_polygon_attrib.106 = type { i32, i32, i32, i8, i8, i32, i32, i8, i8, float, float, i8, i8, i8, i8 }
%struct.gl_scissor_attrib.107 = type { i8, i32, i32, i32, i32 }
%struct.gl_stencil_attrib.108 = type { i8, i32, i32, i32, i32, i8, i8, i8, i8 }
%struct.gl_texture_attrib.109 = type { i32, i32, [4 x float], i32, i32, i32, i32, i32, [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], %struct.gl_texture_object.119*, %struct.gl_texture_object.119*, %struct.gl_texture_object.119*, %struct.gl_texture_object.119*, %struct.gl_texture_object.119*, %struct.gl_texture_object.119* }
%struct.gl_transform_attrib.110 = type { i32, [6 x [4 x float]], [6 x i8], i8, i8 }
%struct.gl_viewport_attrib.111 = type { i32, i32, i32, i32, float, float, float, float, float, float, float, float }
%struct.gl_attrib_node.93 = type { i32, i8*, %struct.gl_attrib_node.93* }
%struct.gl_array_attrib.112 = type { i32, i32, i32, i32, i8*, i8, i32, i32, i32, i8*, i8, i32, i32, i32, i32, i8*, i8, i32, i32, i32, i8*, i8, i32, i32, i32, i32, i8*, i8, i32, i32, i8*, i8 }
%struct.gl_pixelstore_attrib.113 = type { i32, i32, i32, i32, i32, i32, i8, i8 }
%struct.gl_evaluators.114 = type { %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_1d_map.125, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126, %struct.gl_2d_map.126 }
%struct.gl_1d_map.125 = type { i32, float, float, float*, i8 }
%struct.gl_2d_map.126 = type { i32, i32, float, float, float, float, float*, i8 }
%struct.gl_feedback.115 = type { i32, i32, float*, i32, i32 }
%struct.gl_selection.116 = type { i32*, i32, i32, i32, i32, [64 x i32], i8, float, float }
%struct.vertex_buffer.117 = type { [504 x [4 x float]], [504 x [4 x float]], [504 x [4 x float]], [504 x [3 x float]], [504 x [3 x float]], [504 x [4 x i32]], [504 x [4 x i32]], [4 x i32]*, [504 x i32], [504 x i32], i32*, [504 x i8], [504 x [4 x float]], [504 x i8], i8, i32, i32, i32, i8, [504 x i32], [504 x [2 x %struct.gl_material.124]], i8 }
%struct.pixel_buffer.118 = type opaque

@CurrentBlock = external hidden unnamed_addr global %union.node.86*, align 4
@InstSize = external hidden unnamed_addr global [106 x i32], align 4
@CurrentPos = external hidden unnamed_addr global i32, align 4
@.str.5 = external hidden unnamed_addr constant [20 x i8], align 1
@.str.6 = external hidden unnamed_addr constant [8 x i8], align 1
@__PRETTY_FUNCTION__.alloc_instruction = external hidden unnamed_addr constant [52 x i8], align 1
@.str.7 = external hidden unnamed_addr constant [22 x i8], align 1

; Function Attrs: nounwind
define void @gl_save_Accum(%struct.gl_context.87* %ctx, i32 signext %op, float %value) #0 {
  %1 = load i32, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 0), align 4, !tbaa !1
  %2 = icmp eq i32 %1, 3
  br i1 %2, label %4, label %3, !exec_freq !5

; <label>:3                                       ; preds = %0
  tail call void @__assert_fail(i8* nonnull getelementptr inbounds ([20 x i8], [20 x i8]* @.str.5, i32 0, i32 0), i8* nonnull getelementptr inbounds ([8 x i8], [8 x i8]* @.str.6, i32 0, i32 0), i32 signext 319, i8* nonnull getelementptr inbounds ([52 x i8], [52 x i8]* @__PRETTY_FUNCTION__.alloc_instruction, i32 0, i32 0)) #3
  unreachable, !exec_freq !6

; <label>:4                                       ; preds = %0
  %5 = load i32, i32* @CurrentPos, align 4, !tbaa !1
  %6 = add i32 %5, 5
  %7 = icmp ugt i32 %6, 500
  %8 = load %union.node.86*, %union.node.86** @CurrentBlock, align 4, !tbaa !7
  br i1 %7, label %9, label %alloc_instruction.exit, !exec_freq !9

; <label>:9                                       ; preds = %4
  %gep_int = ptrtoint %union.node.86* %8 to i32
  %gep_array = mul i32 %5, 4
  %gep = add i32 %gep_int, %gep_array
  %10 = inttoptr i32 %gep to %union.node.86*
  %11 = bitcast %union.node.86* %10 to i32*
  store i32 104, i32* %11, align 4, !tbaa !10
  %12 = tail call noalias i8* @malloc(i32 signext 2000) #4
  %13 = icmp eq i8* %12, null
  br i1 %13, label %alloc_instruction.exit.thread, label %14, !exec_freq !11

alloc_instruction.exit.thread:                    ; preds = %9
  tail call void @gl_error(%struct.gl_context.87* %ctx, i32 signext 1285, i8* nonnull getelementptr inbounds ([22 x i8], [22 x i8]* @.str.7, i32 0, i32 0)) #4
  br label %28, !exec_freq !12

; <label>:14                                      ; preds = %9
  %gep_int1 = ptrtoint %union.node.86* %10 to i32
  %gep2 = add i32 %gep_int1, 4
  %15 = inttoptr i32 %gep2 to %union.node.86*
  %16 = bitcast %union.node.86* %15 to i8**
  store i8* %12, i8** %16, align 4, !tbaa !7
  store i8* %12, i8** bitcast (%union.node.86** @CurrentBlock to i8**), align 4, !tbaa !7
  store i32 0, i32* @CurrentPos, align 4, !tbaa !1
  %17 = bitcast i8* %12 to %union.node.86*
  br label %alloc_instruction.exit, !exec_freq !13

alloc_instruction.exit:                           ; preds = %14, %4
  %18 = phi i32 [ 0, %14 ], [ %5, %4 ]
  %19 = phi %union.node.86* [ %17, %14 ], [ %8, %4 ]
  %gep_int3 = ptrtoint %union.node.86* %19 to i32
  %gep_array4 = mul i32 %18, 4
  %gep5 = add i32 %gep_int3, %gep_array4
  %20 = inttoptr i32 %gep5 to %union.node.86*
  %21 = add i32 %18, 3
  store i32 %21, i32* @CurrentPos, align 4, !tbaa !1
  %22 = bitcast %union.node.86* %20 to i32*
  store i32 0, i32* %22, align 4, !tbaa !10
  %23 = icmp eq %union.node.86* %20, null
  br i1 %23, label %28, label %24, !exec_freq !14

; <label>:24                                      ; preds = %alloc_instruction.exit
  %gep_int6 = ptrtoint %union.node.86* %20 to i32
  %gep7 = add i32 %gep_int6, 4
  %25 = inttoptr i32 %gep7 to i32*
  store i32 %op, i32* %25, align 4, !tbaa !10
  %gep_int8 = ptrtoint %union.node.86* %20 to i32
  %gep9 = add i32 %gep_int8, 8
  %26 = inttoptr i32 %gep9 to %union.node.86*
  %27 = bitcast %union.node.86* %26 to float*
  store float %value, float* %27, align 4, !tbaa !15
  br label %28, !exec_freq !17

; <label>:28                                      ; preds = %24, %alloc_instruction.exit, %alloc_instruction.exit.thread
  %gep_int10 = ptrtoint %struct.gl_context.87* %ctx to i32
  %gep11 = add i32 %gep_int10, 7228
  %29 = inttoptr i32 %gep11 to i8*
  %30 = load i8, i8* %29, align 4, !tbaa !18
  %31 = icmp eq i8 %30, 0
  br i1 %31, label %33, label %32, !exec_freq !9

; <label>:32                                      ; preds = %28
  tail call void @gl_Accum(%struct.gl_context.87* nonnull %ctx, i32 signext %op, float %value) #4
  br label %33, !exec_freq !49

; <label>:33                                      ; preds = %32, %28
  ret void, !exec_freq !9
}

declare void @gl_Accum(%struct.gl_context.87*, i32 signext, float) #1

declare void @gl_error(%struct.gl_context.87*, i32 signext, i8*) #1

; Function Attrs: nounwind
declare noalias i8* @malloc(i32 signext) #0

; Function Attrs: noreturn nounwind
declare void @__assert_fail(i8*, i8*, i32 signext, i8*) #2

attributes #0 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { noreturn nounwind }
attributes #4 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !2, i64 0}
!2 = !{!"int", !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
!5 = !{i64 8388608}
!6 = !{i64 8}
!7 = !{!8, !8, i64 0}
!8 = !{!"any pointer", !3, i64 0}
!9 = !{i64 8388600}
!10 = !{!3, !3, i64 0}
!11 = !{i64 4194300}
!12 = !{i64 1572862}
!13 = !{i64 2621437}
!14 = !{i64 6815737}
!15 = !{!16, !16, i64 0}
!16 = !{!"float", !3, i64 0}
!17 = !{i64 4259835}
!18 = !{!19, !3, i64 7228}
!19 = !{!"gl_context", !8, i64 0, !20, i64 4, !20, i64 676, !20, i64 1348, !8, i64 2020, !8, i64 2024, !21, i64 2028, !8, i64 2208, !3, i64 2212, !3, i64 2276, !3, i64 2340, !2, i64 2344, !3, i64 2348, !3, i64 4396, !2, i64 4460, !3, i64 4464, !3, i64 6512, !3, i64 6576, !2, i64 6580, !3, i64 6584, !2, i64 7224, !3, i64 7228, !3, i64 7229, !2, i64 7232, !3, i64 7236, !22, i64 7300, !23, i64 7316, !24, i64 7404, !25, i64 7512, !26, i64 7524, !27, i64 7580, !28, i64 7620, !29, i64 7640, !31, i64 43772, !33, i64 43784, !34, i64 43788, !35, i64 54132, !36, i64 54140, !3, i64 54180, !37, i64 54308, !38, i64 54328, !39, i64 54352, !40, i64 54548, !41, i64 54656, !2, i64 54704, !3, i64 54708, !42, i64 54772, !43, i64 54900, !43, i64 54928, !44, i64 54956, !47, i64 55424, !48, i64 55444, !3, i64 55732, !2, i64 55736, !3, i64 55740, !3, i64 55744, !2, i64 55748, !2, i64 55752, !2, i64 55756, !2, i64 55760, !3, i64 55764, !16, i64 55768, !16, i64 55772, !16, i64 55776, !3, i64 55780, !3, i64 55781, !3, i64 55782, !3, i64 55783, !2, i64 55784, !8, i64 55788, !8, i64 55792, !8, i64 55796, !8, i64 55800, !8, i64 55804, !8, i64 55808, !8, i64 55812, !3, i64 55816}
!20 = !{!"api_function_table", !8, i64 0, !8, i64 4, !8, i64 8, !8, i64 12, !8, i64 16, !8, i64 20, !8, i64 24, !8, i64 28, !8, i64 32, !8, i64 36, !8, i64 40, !8, i64 44, !8, i64 48, !8, i64 52, !8, i64 56, !8, i64 60, !8, i64 64, !8, i64 68, !8, i64 72, !8, i64 76, !8, i64 80, !8, i64 84, !8, i64 88, !8, i64 92, !8, i64 96, !8, i64 100, !8, i64 104, !8, i64 108, !8, i64 112, !8, i64 116, !8, i64 120, !8, i64 124, !8, i64 128, !8, i64 132, !8, i64 136, !8, i64 140, !8, i64 144, !8, i64 148, !8, i64 152, !8, i64 156, !8, i64 160, !8, i64 164, !8, i64 168, !8, i64 172, !8, i64 176, !8, i64 180, !8, i64 184, !8, i64 188, !8, i64 192, !8, i64 196, !8, i64 200, !8, i64 204, !8, i64 208, !8, i64 212, !8, i64 216, !8, i64 220, !8, i64 224, !8, i64 228, !8, i64 232, !8, i64 236, !8, i64 240, !8, i64 244, !8, i64 248, !8, i64 252, !8, i64 256, !8, i64 260, !8, i64 264, !8, i64 268, !8, i64 272, !8, i64 276, !8, i64 280, !8, i64 284, !8, i64 288, !8, i64 292, !8, i64 296, !8, i64 300, !8, i64 304, !8, i64 308, !8, i64 312, !8, i64 316, !8, i64 320, !8, i64 324, !8, i64 328, !8, i64 332, !8, i64 336, !8, i64 340, !8, i64 344, !8, i64 348, !8, i64 352, !8, i64 356, !8, i64 360, !8, i64 364, !8, i64 368, !8, i64 372, !8, i64 376, !8, i64 380, !8, i64 384, !8, i64 388, !8, i64 392, !8, i64 396, !8, i64 400, !8, i64 404, !8, i64 408, !8, i64 412, !8, i64 416, !8, i64 420, !8, i64 424, !8, i64 428, !8, i64 432, !8, i64 436, !8, i64 440, !8, i64 444, !8, i64 448, !8, i64 452, !8, i64 456, !8, i64 460, !8, i64 464, !8, i64 468, !8, i64 472, !8, i64 476, !8, i64 480, !8, i64 484, !8, i64 488, !8, i64 492, !8, i64 496, !8, i64 500, !8, i64 504, !8, i64 508, !8, i64 512, !8, i64 516, !8, i64 520, !8, i64 524, !8, i64 528, !8, i64 532, !8, i64 536, !8, i64 540, !8, i64 544, !8, i64 548, !8, i64 552, !8, i64 556, !8, i64 560, !8, i64 564, !8, i64 568, !8, i64 572, !8, i64 576, !8, i64 580, !8, i64 584, !8, i64 588, !8, i64 592, !8, i64 596, !8, i64 600, !8, i64 604, !8, i64 608, !8, i64 612, !8, i64 616, !8, i64 620, !8, i64 624, !8, i64 628, !8, i64 632, !8, i64 636, !8, i64 640, !8, i64 644, !8, i64 648, !8, i64 652, !8, i64 656, !8, i64 660, !8, i64 664, !8, i64 668}
!21 = !{!"dd_function_table", !8, i64 0, !8, i64 4, !8, i64 8, !8, i64 12, !8, i64 16, !8, i64 20, !8, i64 24, !8, i64 28, !8, i64 32, !8, i64 36, !8, i64 40, !8, i64 44, !8, i64 48, !8, i64 52, !8, i64 56, !8, i64 60, !8, i64 64, !8, i64 68, !8, i64 72, !8, i64 76, !8, i64 80, !8, i64 84, !8, i64 88, !8, i64 92, !8, i64 96, !8, i64 100, !8, i64 104, !8, i64 108, !8, i64 112, !8, i64 116, !8, i64 120, !8, i64 124, !8, i64 128, !8, i64 132, !8, i64 136, !8, i64 140, !8, i64 144, !8, i64 148, !8, i64 152, !8, i64 156, !8, i64 160, !8, i64 164, !8, i64 168, !8, i64 172, !8, i64 176}
!22 = !{!"gl_accum_attrib", !3, i64 0}
!23 = !{!"gl_colorbuffer_attrib", !2, i64 0, !3, i64 4, !2, i64 20, !2, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !16, i64 44, !3, i64 48, !3, i64 49, !3, i64 52, !3, i64 56, !3, i64 60, !3, i64 64, !3, i64 80, !3, i64 84, !3, i64 85, !3, i64 86, !3, i64 87}
!24 = !{!"gl_current_attrib", !3, i64 0, !2, i64 16, !3, i64 20, !3, i64 32, !3, i64 48, !16, i64 64, !3, i64 68, !2, i64 84, !3, i64 88, !3, i64 104, !3, i64 105}
!25 = !{!"gl_depthbuffer_attrib", !3, i64 0, !16, i64 4, !3, i64 8, !3, i64 9}
!26 = !{!"gl_eval_attrib", !3, i64 0, !3, i64 1, !3, i64 2, !3, i64 3, !3, i64 4, !3, i64 5, !3, i64 6, !3, i64 7, !3, i64 8, !3, i64 9, !3, i64 10, !3, i64 11, !3, i64 12, !3, i64 13, !3, i64 14, !3, i64 15, !3, i64 16, !3, i64 17, !3, i64 18, !2, i64 20, !16, i64 24, !16, i64 28, !2, i64 32, !2, i64 36, !16, i64 40, !16, i64 44, !16, i64 48, !16, i64 52}
!27 = !{!"gl_fog_attrib", !3, i64 0, !3, i64 4, !16, i64 20, !16, i64 24, !16, i64 28, !16, i64 32, !3, i64 36}
!28 = !{!"gl_hint_attrib", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16}
!29 = !{!"gl_light_attrib", !3, i64 0, !30, i64 34304, !3, i64 34324, !3, i64 36084, !3, i64 36088, !3, i64 36092, !3, i64 36096, !2, i64 36100, !3, i64 36104, !8, i64 36108, !3, i64 36112, !3, i64 36116}
!30 = !{!"gl_lightmodel", !3, i64 0, !3, i64 16, !3, i64 17}
!31 = !{!"gl_line_attrib", !3, i64 0, !3, i64 1, !32, i64 2, !2, i64 4, !16, i64 8}
!32 = !{!"short", !3, i64 0}
!33 = !{!"gl_list_attrib", !2, i64 0}
!34 = !{!"gl_pixel_attrib", !3, i64 0, !16, i64 4, !16, i64 8, !16, i64 12, !16, i64 16, !16, i64 20, !16, i64 24, !16, i64 28, !16, i64 32, !16, i64 36, !16, i64 40, !2, i64 44, !2, i64 48, !3, i64 52, !3, i64 53, !16, i64 56, !16, i64 60, !2, i64 64, !2, i64 68, !2, i64 72, !2, i64 76, !2, i64 80, !2, i64 84, !2, i64 88, !2, i64 92, !2, i64 96, !2, i64 100, !3, i64 104, !3, i64 1128, !3, i64 2152, !3, i64 3176, !3, i64 4200, !3, i64 5224, !3, i64 6248, !3, i64 7272, !3, i64 8296, !3, i64 9320}
!35 = !{!"gl_point_attrib", !3, i64 0, !16, i64 4}
!36 = !{!"gl_polygon_attrib", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 13, !3, i64 16, !2, i64 20, !3, i64 24, !3, i64 25, !16, i64 28, !16, i64 32, !3, i64 36, !3, i64 37, !3, i64 38, !3, i64 39}
!37 = !{!"gl_scissor_attrib", !3, i64 0, !2, i64 4, !2, i64 8, !2, i64 12, !2, i64 16}
!38 = !{!"gl_stencil_attrib", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 21, !3, i64 22, !3, i64 23}
!39 = !{!"gl_texture_attrib", !2, i64 0, !3, i64 4, !3, i64 8, !2, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !3, i64 44, !3, i64 60, !3, i64 76, !3, i64 92, !3, i64 108, !3, i64 124, !3, i64 140, !3, i64 156, !8, i64 172, !8, i64 176, !8, i64 180, !8, i64 184, !8, i64 188, !8, i64 192}
!40 = !{!"gl_transform_attrib", !3, i64 0, !3, i64 4, !3, i64 100, !3, i64 106, !3, i64 107}
!41 = !{!"gl_viewport_attrib", !2, i64 0, !2, i64 4, !2, i64 8, !2, i64 12, !16, i64 16, !16, i64 20, !16, i64 24, !16, i64 28, !16, i64 32, !16, i64 36, !16, i64 40, !16, i64 44}
!42 = !{!"gl_array_attrib", !2, i64 0, !3, i64 4, !2, i64 8, !2, i64 12, !8, i64 16, !3, i64 20, !3, i64 24, !2, i64 28, !2, i64 32, !8, i64 36, !3, i64 40, !2, i64 44, !3, i64 48, !2, i64 52, !2, i64 56, !8, i64 60, !3, i64 64, !3, i64 68, !2, i64 72, !2, i64 76, !8, i64 80, !3, i64 84, !2, i64 88, !3, i64 92, !2, i64 96, !2, i64 100, !8, i64 104, !3, i64 108, !2, i64 112, !2, i64 116, !8, i64 120, !3, i64 124}
!43 = !{!"gl_pixelstore_attrib", !2, i64 0, !2, i64 4, !2, i64 8, !2, i64 12, !2, i64 16, !2, i64 20, !3, i64 24, !3, i64 25}
!44 = !{!"gl_evaluators", !45, i64 0, !45, i64 20, !45, i64 40, !45, i64 60, !45, i64 80, !45, i64 100, !45, i64 120, !45, i64 140, !45, i64 160, !46, i64 180, !46, i64 212, !46, i64 244, !46, i64 276, !46, i64 308, !46, i64 340, !46, i64 372, !46, i64 404, !46, i64 436}
!45 = !{!"gl_1d_map", !2, i64 0, !16, i64 4, !16, i64 8, !8, i64 12, !3, i64 16}
!46 = !{!"gl_2d_map", !2, i64 0, !2, i64 4, !16, i64 8, !16, i64 12, !16, i64 16, !16, i64 20, !8, i64 24, !3, i64 28}
!47 = !{!"gl_feedback", !3, i64 0, !2, i64 4, !8, i64 8, !2, i64 12, !2, i64 16}
!48 = !{!"gl_selection", !8, i64 0, !2, i64 4, !2, i64 8, !2, i64 12, !2, i64 16, !3, i64 20, !3, i64 276, !16, i64 280, !16, i64 284}
!49 = !{i64 5242875}
