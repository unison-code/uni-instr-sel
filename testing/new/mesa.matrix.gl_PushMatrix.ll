; ModuleID = 'mesa.matrix.gl_PushMatrix.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%struct.gl_context.287 = type { %struct.gl_shared_state.288*, %struct.api_function_table.289, %struct.api_function_table.289, %struct.api_function_table.289, %struct.gl_visual.290*, %struct.gl_frame_buffer.291*, %struct.dd_function_table.292, i8*, [16 x float], [16 x float], i8, i32, [32 x [16 x float]], [16 x float], i32, [32 x [16 x float]], [16 x float], i8, i32, [10 x [16 x float]], i32, i8, i8, i32, [16 x %struct.gl_attrib_node.293*], %struct.gl_accum_attrib.294, %struct.gl_colorbuffer_attrib.295, %struct.gl_current_attrib.296, %struct.gl_depthbuffer_attrib.297, %struct.gl_eval_attrib.298, %struct.gl_fog_attrib.299, %struct.gl_hint_attrib.300, %struct.gl_light_attrib.301, %struct.gl_line_attrib.302, %struct.gl_list_attrib.303, %struct.gl_pixel_attrib.304, %struct.gl_point_attrib.305, %struct.gl_polygon_attrib.306, [32 x i32], %struct.gl_scissor_attrib.307, %struct.gl_stencil_attrib.308, %struct.gl_texture_attrib.309, %struct.gl_transform_attrib.310, %struct.gl_viewport_attrib.311, i32, [16 x %struct.gl_attrib_node.293*], %struct.gl_array_attrib.312, %struct.gl_pixelstore_attrib.313, %struct.gl_pixelstore_attrib.313, %struct.gl_evaluators.314, %struct.gl_feedback.315, %struct.gl_selection.316, i32, i32, i32, i32, i32, i32, i32, i32, i8, float, float, float, i8, i8, i8, i8, i32, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32*, i32)*, void (%struct.gl_context.287*, i32, i32*, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32)*, %struct.vertex_buffer.317*, %struct.pixel_buffer.318*, i8 }
%struct.gl_shared_state.288 = type { i32, [7000 x %union.node.319*], %struct.gl_texture_object.320* }
%union.node.319 = type opaque
%struct.gl_texture_object.320 = type { i32, i32, i32, float, [4 x i32], i32, i32, i32, i32, i32, [11 x %struct.gl_texture_image.321*], i8, %struct.gl_texture_object.320* }
%struct.gl_texture_image.321 = type { i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i8* }
%struct.api_function_table.289 = type { void (%struct.gl_context.287*, i32, float)*, void (%struct.gl_context.287*, i32, float)*, i8 (%struct.gl_context.287*, i32, i32*, i8*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, i32, float, float, float, float, %struct.gl_image.322*)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i8*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, double)*, void (%struct.gl_context.287*, float)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, float*)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, i8, i8, i8, i8)*, void (%struct.gl_context.287*, i8, i8, i8, i8)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, i32*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i8)*, void (%struct.gl_context.287*, double, double)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i8)*, void (%struct.gl_context.287*, i32, i8*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32)*, {}*, {}*, void (%struct.gl_context.287*, float)*, void (%struct.gl_context.287*, float, float)*, void (%struct.gl_context.287*, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, i32, float*)*, {}*, {}*, void (%struct.gl_context.287*, i32, float*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, double, double, double, double, double, double)*, i32 (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32*)*, void (%struct.gl_context.287*, i32, i8*)*, void (%struct.gl_context.287*, i32, double*)*, void (%struct.gl_context.287*, i32, double*)*, i32 (%struct.gl_context.287*)*, void (%struct.gl_context.287*, i32, float*)*, void (%struct.gl_context.287*, i32, i32*)*, i8* (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32, double*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, float*)*, void (%struct.gl_context.287*, i32, i32*)*, void (%struct.gl_context.287*, i32, i16*)*, void (%struct.gl_context.287*, i32, i8**)*, void (%struct.gl_context.287*, i8*)*, void (%struct.gl_context.287*, i32, i32*, float*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32, double*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, float)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i8*)*, {}*, void (%struct.gl_context.287*, i32, i32, i8*)*, i8 (%struct.gl_context.287*, i32)*, i8 (%struct.gl_context.287*, i32)*, i8 (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, float*)*, void (%struct.gl_context.287*, i32, i32, float*, i32)*, void (%struct.gl_context.287*, i32, i16)*, void (%struct.gl_context.287*, float)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, float*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, float, float, i32, i32, float*, i8)*, void (%struct.gl_context.287*, i32, float, float, i32, i32, float, float, i32, i32, float*, i8)*, void (%struct.gl_context.287*, i32, float, float)*, void (%struct.gl_context.287*, i32, float, float, i32, float, float)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, float*)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, float, float, float)*, void (%struct.gl_context.287*, float*)*, void (%struct.gl_context.287*, i32, i32, i8*)*, void (%struct.gl_context.287*, float)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, float)*, void (%struct.gl_context.287*, float, float)*, void (%struct.gl_context.287*, float)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, float, float)*, void (%struct.gl_context.287*, i8*)*, {}*, {}*, {}*, {}*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32)*, {}*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, float, float, float, float)*, i32 (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, float, float, float)*, void (%struct.gl_context.287*, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32*)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32, i32, i32)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32, i32, float*)*, void (%struct.gl_context.287*, float, float, float)*, void (%struct.gl_context.287*, float, float, float, float)*, void (%struct.gl_context.287*, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i32)*, void (%struct.gl_context.287*, float, float, float, float)*, {}* }
%struct.gl_image.322 = type { i32, i32, i32, i32, i32, i32, i8*, i8, i32 }
%struct.gl_visual.290 = type { i8, i8, float, float, float, float, i8, float, float, float, float, i32, i32, i32, i32, i8, i8 }
%struct.gl_frame_buffer.291 = type { %struct.gl_visual.290*, i32, i32, i16*, i8*, i16*, i8*, i8*, i8*, i32, i32, i32, i32 }
%struct.dd_function_table.292 = type { {}*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i8, i8, i8, i8)*, void (%struct.gl_context.287*, i8, i32, i32, i32, i32)*, void (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i8, i8, i8, i8)*, i8 (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i32*, i32*)*, void (%struct.gl_context.287*, i32, i32, i32, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i8*, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i32*, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i32*, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, i32*)*, void (%struct.gl_context.287*, i32, i32, i32, i8*, i8*, i8*, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i32*, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i8*, i8*, i8*, i8*, i8*)*, {}*, {}*, i8 (%struct.gl_context.287*, i32)*, i8 (%struct.gl_context.287*, i8, i8, i8, i8)*, i8 (%struct.gl_context.287*, i32)*, void (%struct.gl_context.287*, i8)*, {}*, {}*, {}*, i32 (%struct.gl_context.287*, i32, i32, i32, i16*, i8*)*, void (%struct.gl_context.287*, i32, i32*, i32*, i16*, i8*)*, void (%struct.gl_context.287*, i32, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32, i16*)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32)*, void (%struct.gl_context.287*, i32, i32, i32, i32)*, i8 (%struct.gl_context.287*, i32, i32, i32, i32, i32, i32, i8, i8*)*, i8 (%struct.gl_context.287*, i32, i32, float, float, float, float, %struct.gl_image.322*)*, void (%struct.gl_context.287*, i32)*, {}*, void (%struct.gl_context.287*, i32, float*)*, void (%struct.gl_context.287*, i32, i32, i32, i32, %struct.gl_texture_image.321*)*, void (%struct.gl_context.287*, i32, i32, i32, float*)*, void (%struct.gl_context.287*, i32, i32)*, void (%struct.gl_context.287*, i32)* }
%struct.gl_accum_attrib.294 = type { [4 x float] }
%struct.gl_colorbuffer_attrib.295 = type { i32, [4 x float], i32, i32, i8, i32, i8, i32, float, i8, i8, i32, i32, i32, [4 x float], i32, i8, i8, i8, i8 }
%struct.gl_current_attrib.296 = type { [4 x i32], i32, [3 x float], [4 x float], [4 x float], float, [4 x float], i32, [4 x float], i8, i8 }
%struct.gl_depthbuffer_attrib.297 = type { i32, float, i8, i8 }
%struct.gl_eval_attrib.298 = type { i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i32, float, float, i32, i32, float, float, float, float }
%struct.gl_fog_attrib.299 = type { i8, [4 x float], float, float, float, float, i32 }
%struct.gl_hint_attrib.300 = type { i32, i32, i32, i32, i32 }
%struct.gl_light_attrib.301 = type { [8 x %struct.gl_light.323], %struct.gl_lightmodel.324, [2 x %struct.gl_material.325], i8, i32, i32, i32, i32, i8, %struct.gl_light.323*, i8, [4 x float] }
%struct.gl_light.323 = type { [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], float, float, float, float, float, float, i8, %struct.gl_light.323*, [3 x float], [3 x float], [3 x float], [512 x [2 x float]], [3 x float], [3 x float], [3 x float], float, float }
%struct.gl_lightmodel.324 = type { [4 x float], i8, i8 }
%struct.gl_material.325 = type { [4 x float], [4 x float], [4 x float], [4 x float], float, float, float, float, [200 x float] }
%struct.gl_line_attrib.302 = type { i8, i8, i16, i32, float }
%struct.gl_list_attrib.303 = type { i32 }
%struct.gl_pixel_attrib.304 = type { i32, float, float, float, float, float, float, float, float, float, float, i32, i32, i8, i8, float, float, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, [256 x i32], [256 x i32], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float], [256 x float] }
%struct.gl_point_attrib.305 = type { i8, float }
%struct.gl_polygon_attrib.306 = type { i32, i32, i32, i8, i8, i32, i32, i8, i8, float, float, i8, i8, i8, i8 }
%struct.gl_scissor_attrib.307 = type { i8, i32, i32, i32, i32 }
%struct.gl_stencil_attrib.308 = type { i8, i32, i32, i32, i32, i8, i8, i8, i8 }
%struct.gl_texture_attrib.309 = type { i32, i32, [4 x float], i32, i32, i32, i32, i32, [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], [4 x float], %struct.gl_texture_object.320*, %struct.gl_texture_object.320*, %struct.gl_texture_object.320*, %struct.gl_texture_object.320*, %struct.gl_texture_object.320*, %struct.gl_texture_object.320* }
%struct.gl_transform_attrib.310 = type { i32, [6 x [4 x float]], [6 x i8], i8, i8 }
%struct.gl_viewport_attrib.311 = type { i32, i32, i32, i32, float, float, float, float, float, float, float, float }
%struct.gl_attrib_node.293 = type { i32, i8*, %struct.gl_attrib_node.293* }
%struct.gl_array_attrib.312 = type { i32, i32, i32, i32, i8*, i8, i32, i32, i32, i8*, i8, i32, i32, i32, i32, i8*, i8, i32, i32, i32, i8*, i8, i32, i32, i32, i32, i8*, i8, i32, i32, i8*, i8 }
%struct.gl_pixelstore_attrib.313 = type { i32, i32, i32, i32, i32, i32, i8, i8 }
%struct.gl_evaluators.314 = type { %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_1d_map.326, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327, %struct.gl_2d_map.327 }
%struct.gl_1d_map.326 = type { i32, float, float, float*, i8 }
%struct.gl_2d_map.327 = type { i32, i32, float, float, float, float, float*, i8 }
%struct.gl_feedback.315 = type { i32, i32, float*, i32, i32 }
%struct.gl_selection.316 = type { i32*, i32, i32, i32, i32, [64 x i32], i8, float, float }
%struct.vertex_buffer.317 = type opaque
%struct.pixel_buffer.318 = type opaque

@.str.2 = external hidden unnamed_addr constant [13 x i8], align 1

declare void @gl_error(%struct.gl_context.287*, i32 signext, i8*) #0

; Function Attrs: nounwind
define void @gl_PushMatrix(%struct.gl_context.287* %ctx) #1 {
  %gep_int = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep = add i32 %gep_int, 55744
  %1 = inttoptr i32 %gep to i32*
  %2 = load i32, i32* %1, align 4, !tbaa !1
  %3 = icmp eq i32 %2, 6656
  br i1 %3, label %5, label %4, !exec_freq !37

; <label>:4                                       ; preds = %0
  tail call void @gl_error(%struct.gl_context.287* nonnull %ctx, i32 signext 1282, i8* nonnull getelementptr inbounds ([13 x i8], [13 x i8]* @.str.2, i32 0, i32 0)) #4
  br label %45, !exec_freq !38

; <label>:5                                       ; preds = %0
  %gep_int5 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep6 = add i32 %gep_int5, 54548
  %6 = inttoptr i32 %gep6 to i32*
  %7 = load i32, i32* %6, align 4, !tbaa !39
  br label %NodeBlock3, !exec_freq !38

NodeBlock3:                                       ; preds = %5
  %Pivot4 = icmp slt i32 %7, 5889
  br i1 %Pivot4, label %LeafBlock, label %NodeBlock, !exec_freq !38

NodeBlock:                                        ; preds = %NodeBlock3
  %Pivot = icmp slt i32 %7, 5890
  br i1 %Pivot, label %20, label %LeafBlock1, !exec_freq !40

LeafBlock1:                                       ; preds = %NodeBlock
  %SwitchLeaf2 = icmp eq i32 %7, 5890
  br i1 %SwitchLeaf2, label %32, label %NewDefault, !exec_freq !41

LeafBlock:                                        ; preds = %NodeBlock3
  %SwitchLeaf = icmp eq i32 %7, 5888
  br i1 %SwitchLeaf, label %8, label %NewDefault, !exec_freq !40

; <label>:8                                       ; preds = %LeafBlock
  %gep_int7 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep8 = add i32 %gep_int7, 2344
  %9 = inttoptr i32 %gep8 to i32*
  %10 = load i32, i32* %9, align 4, !tbaa !42
  %11 = icmp ugt i32 %10, 30
  br i1 %11, label %12, label %13, !exec_freq !43

; <label>:12                                      ; preds = %8
  tail call void @gl_error(%struct.gl_context.287* nonnull %ctx, i32 signext 1283, i8* nonnull getelementptr inbounds ([13 x i8], [13 x i8]* @.str.2, i32 0, i32 0)) #4
  br label %45, !exec_freq !44

; <label>:13                                      ; preds = %8
  %gep_int9 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep10 = add i32 %gep_int9, 2348
  %gep_array = mul i32 %10, 64
  %gep11 = add i32 %gep10, %gep_array
  %14 = inttoptr i32 %gep11 to float*
  %15 = bitcast float* %14 to i8*
  %gep_int12 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep13 = add i32 %gep_int12, 2212
  %16 = inttoptr i32 %gep13 to float*
  %17 = bitcast float* %16 to i8*
  tail call void @llvm.memcpy.p0i8.p0i8.i32(i8* %15, i8* %17, i32 64, i32 1, i1 false)
  %sunkaddr = ptrtoint %struct.gl_context.287* %ctx to i32
  %sunkaddr1 = add i32 %sunkaddr, 2344
  %sunkaddr2 = inttoptr i32 %sunkaddr1 to i32*
  %18 = load i32, i32* %sunkaddr2, align 4, !tbaa !42
  %19 = add i32 %18, 1
  store i32 %19, i32* %sunkaddr2, align 4, !tbaa !42
  br label %45, !exec_freq !44

; <label>:20                                      ; preds = %NodeBlock
  %gep_int14 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep15 = add i32 %gep_int14, 4460
  %21 = inttoptr i32 %gep15 to i32*
  %22 = load i32, i32* %21, align 4, !tbaa !45
  %23 = icmp ugt i32 %22, 31
  br i1 %23, label %24, label %25, !exec_freq !41

; <label>:24                                      ; preds = %20
  tail call void @gl_error(%struct.gl_context.287* nonnull %ctx, i32 signext 1283, i8* nonnull getelementptr inbounds ([13 x i8], [13 x i8]* @.str.2, i32 0, i32 0)) #4
  br label %45, !exec_freq !46

; <label>:25                                      ; preds = %20
  %gep_int16 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep17 = add i32 %gep_int16, 4464
  %gep_array18 = mul i32 %22, 64
  %gep19 = add i32 %gep17, %gep_array18
  %26 = inttoptr i32 %gep19 to float*
  %27 = bitcast float* %26 to i8*
  %gep_int20 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep21 = add i32 %gep_int20, 4396
  %28 = inttoptr i32 %gep21 to float*
  %29 = bitcast float* %28 to i8*
  tail call void @llvm.memcpy.p0i8.p0i8.i32(i8* %27, i8* %29, i32 64, i32 1, i1 false)
  %sunkaddr3 = ptrtoint %struct.gl_context.287* %ctx to i32
  %sunkaddr4 = add i32 %sunkaddr3, 4460
  %sunkaddr5 = inttoptr i32 %sunkaddr4 to i32*
  %30 = load i32, i32* %sunkaddr5, align 4, !tbaa !45
  %31 = add i32 %30, 1
  store i32 %31, i32* %sunkaddr5, align 4, !tbaa !45
  br label %45, !exec_freq !46

; <label>:32                                      ; preds = %LeafBlock1
  %gep_int22 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep23 = add i32 %gep_int22, 6580
  %33 = inttoptr i32 %gep23 to i32*
  %34 = load i32, i32* %33, align 4, !tbaa !47
  %35 = icmp ugt i32 %34, 9
  br i1 %35, label %36, label %37, !exec_freq !44

; <label>:36                                      ; preds = %32
  tail call void @gl_error(%struct.gl_context.287* nonnull %ctx, i32 signext 1283, i8* nonnull getelementptr inbounds ([13 x i8], [13 x i8]* @.str.2, i32 0, i32 0)) #4
  br label %45, !exec_freq !48

; <label>:37                                      ; preds = %32
  %gep_int24 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep25 = add i32 %gep_int24, 6584
  %gep_array26 = mul i32 %34, 64
  %gep27 = add i32 %gep25, %gep_array26
  %38 = inttoptr i32 %gep27 to float*
  %39 = bitcast float* %38 to i8*
  %gep_int28 = ptrtoint %struct.gl_context.287* %ctx to i32
  %gep29 = add i32 %gep_int28, 6512
  %40 = inttoptr i32 %gep29 to float*
  %41 = bitcast float* %40 to i8*
  tail call void @llvm.memcpy.p0i8.p0i8.i32(i8* %39, i8* %41, i32 64, i32 1, i1 false)
  %sunkaddr6 = ptrtoint %struct.gl_context.287* %ctx to i32
  %sunkaddr7 = add i32 %sunkaddr6, 6580
  %sunkaddr8 = inttoptr i32 %sunkaddr7 to i32*
  %42 = load i32, i32* %sunkaddr8, align 4, !tbaa !47
  %43 = add i32 %42, 1
  store i32 %43, i32* %sunkaddr8, align 4, !tbaa !47
  br label %45, !exec_freq !48

NewDefault:                                       ; preds = %LeafBlock, %LeafBlock1
  br label %44, !exec_freq !49

; <label>:44                                      ; preds = %NewDefault
  tail call void @abort() #5
  unreachable, !exec_freq !49

; <label>:45                                      ; preds = %37, %36, %25, %24, %13, %12, %4
  ret void, !exec_freq !50
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture, i8* nocapture readonly, i32, i32, i1) #2

; Function Attrs: noreturn nounwind
declare void @abort() #3

attributes #0 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { argmemonly nounwind }
attributes #3 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind }
attributes #5 = { noreturn nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !4, i64 55744}
!2 = !{!"gl_context", !3, i64 0, !6, i64 4, !6, i64 676, !6, i64 1348, !3, i64 2020, !3, i64 2024, !7, i64 2028, !3, i64 2208, !4, i64 2212, !4, i64 2276, !4, i64 2340, !8, i64 2344, !4, i64 2348, !4, i64 4396, !8, i64 4460, !4, i64 4464, !4, i64 6512, !4, i64 6576, !8, i64 6580, !4, i64 6584, !8, i64 7224, !4, i64 7228, !4, i64 7229, !8, i64 7232, !4, i64 7236, !9, i64 7300, !10, i64 7316, !12, i64 7404, !13, i64 7512, !14, i64 7524, !15, i64 7580, !16, i64 7620, !17, i64 7640, !19, i64 43772, !21, i64 43784, !22, i64 43788, !23, i64 54132, !24, i64 54140, !4, i64 54180, !25, i64 54308, !26, i64 54328, !27, i64 54352, !28, i64 54548, !29, i64 54656, !8, i64 54704, !4, i64 54708, !30, i64 54772, !31, i64 54900, !31, i64 54928, !32, i64 54956, !35, i64 55424, !36, i64 55444, !4, i64 55732, !8, i64 55736, !4, i64 55740, !4, i64 55744, !8, i64 55748, !8, i64 55752, !8, i64 55756, !8, i64 55760, !4, i64 55764, !11, i64 55768, !11, i64 55772, !11, i64 55776, !4, i64 55780, !4, i64 55781, !4, i64 55782, !4, i64 55783, !8, i64 55784, !3, i64 55788, !3, i64 55792, !3, i64 55796, !3, i64 55800, !3, i64 55804, !3, i64 55808, !3, i64 55812, !4, i64 55816}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!"api_function_table", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !3, i64 44, !3, i64 48, !3, i64 52, !3, i64 56, !3, i64 60, !3, i64 64, !3, i64 68, !3, i64 72, !3, i64 76, !3, i64 80, !3, i64 84, !3, i64 88, !3, i64 92, !3, i64 96, !3, i64 100, !3, i64 104, !3, i64 108, !3, i64 112, !3, i64 116, !3, i64 120, !3, i64 124, !3, i64 128, !3, i64 132, !3, i64 136, !3, i64 140, !3, i64 144, !3, i64 148, !3, i64 152, !3, i64 156, !3, i64 160, !3, i64 164, !3, i64 168, !3, i64 172, !3, i64 176, !3, i64 180, !3, i64 184, !3, i64 188, !3, i64 192, !3, i64 196, !3, i64 200, !3, i64 204, !3, i64 208, !3, i64 212, !3, i64 216, !3, i64 220, !3, i64 224, !3, i64 228, !3, i64 232, !3, i64 236, !3, i64 240, !3, i64 244, !3, i64 248, !3, i64 252, !3, i64 256, !3, i64 260, !3, i64 264, !3, i64 268, !3, i64 272, !3, i64 276, !3, i64 280, !3, i64 284, !3, i64 288, !3, i64 292, !3, i64 296, !3, i64 300, !3, i64 304, !3, i64 308, !3, i64 312, !3, i64 316, !3, i64 320, !3, i64 324, !3, i64 328, !3, i64 332, !3, i64 336, !3, i64 340, !3, i64 344, !3, i64 348, !3, i64 352, !3, i64 356, !3, i64 360, !3, i64 364, !3, i64 368, !3, i64 372, !3, i64 376, !3, i64 380, !3, i64 384, !3, i64 388, !3, i64 392, !3, i64 396, !3, i64 400, !3, i64 404, !3, i64 408, !3, i64 412, !3, i64 416, !3, i64 420, !3, i64 424, !3, i64 428, !3, i64 432, !3, i64 436, !3, i64 440, !3, i64 444, !3, i64 448, !3, i64 452, !3, i64 456, !3, i64 460, !3, i64 464, !3, i64 468, !3, i64 472, !3, i64 476, !3, i64 480, !3, i64 484, !3, i64 488, !3, i64 492, !3, i64 496, !3, i64 500, !3, i64 504, !3, i64 508, !3, i64 512, !3, i64 516, !3, i64 520, !3, i64 524, !3, i64 528, !3, i64 532, !3, i64 536, !3, i64 540, !3, i64 544, !3, i64 548, !3, i64 552, !3, i64 556, !3, i64 560, !3, i64 564, !3, i64 568, !3, i64 572, !3, i64 576, !3, i64 580, !3, i64 584, !3, i64 588, !3, i64 592, !3, i64 596, !3, i64 600, !3, i64 604, !3, i64 608, !3, i64 612, !3, i64 616, !3, i64 620, !3, i64 624, !3, i64 628, !3, i64 632, !3, i64 636, !3, i64 640, !3, i64 644, !3, i64 648, !3, i64 652, !3, i64 656, !3, i64 660, !3, i64 664, !3, i64 668}
!7 = !{!"dd_function_table", !3, i64 0, !3, i64 4, !3, i64 8, !3, i64 12, !3, i64 16, !3, i64 20, !3, i64 24, !3, i64 28, !3, i64 32, !3, i64 36, !3, i64 40, !3, i64 44, !3, i64 48, !3, i64 52, !3, i64 56, !3, i64 60, !3, i64 64, !3, i64 68, !3, i64 72, !3, i64 76, !3, i64 80, !3, i64 84, !3, i64 88, !3, i64 92, !3, i64 96, !3, i64 100, !3, i64 104, !3, i64 108, !3, i64 112, !3, i64 116, !3, i64 120, !3, i64 124, !3, i64 128, !3, i64 132, !3, i64 136, !3, i64 140, !3, i64 144, !3, i64 148, !3, i64 152, !3, i64 156, !3, i64 160, !3, i64 164, !3, i64 168, !3, i64 172, !3, i64 176}
!8 = !{!"int", !4, i64 0}
!9 = !{!"gl_accum_attrib", !4, i64 0}
!10 = !{!"gl_colorbuffer_attrib", !8, i64 0, !4, i64 4, !8, i64 20, !8, i64 24, !4, i64 28, !4, i64 32, !4, i64 36, !4, i64 40, !11, i64 44, !4, i64 48, !4, i64 49, !4, i64 52, !4, i64 56, !4, i64 60, !4, i64 64, !4, i64 80, !4, i64 84, !4, i64 85, !4, i64 86, !4, i64 87}
!11 = !{!"float", !4, i64 0}
!12 = !{!"gl_current_attrib", !4, i64 0, !8, i64 16, !4, i64 20, !4, i64 32, !4, i64 48, !11, i64 64, !4, i64 68, !8, i64 84, !4, i64 88, !4, i64 104, !4, i64 105}
!13 = !{!"gl_depthbuffer_attrib", !4, i64 0, !11, i64 4, !4, i64 8, !4, i64 9}
!14 = !{!"gl_eval_attrib", !4, i64 0, !4, i64 1, !4, i64 2, !4, i64 3, !4, i64 4, !4, i64 5, !4, i64 6, !4, i64 7, !4, i64 8, !4, i64 9, !4, i64 10, !4, i64 11, !4, i64 12, !4, i64 13, !4, i64 14, !4, i64 15, !4, i64 16, !4, i64 17, !4, i64 18, !8, i64 20, !11, i64 24, !11, i64 28, !8, i64 32, !8, i64 36, !11, i64 40, !11, i64 44, !11, i64 48, !11, i64 52}
!15 = !{!"gl_fog_attrib", !4, i64 0, !4, i64 4, !11, i64 20, !11, i64 24, !11, i64 28, !11, i64 32, !4, i64 36}
!16 = !{!"gl_hint_attrib", !4, i64 0, !4, i64 4, !4, i64 8, !4, i64 12, !4, i64 16}
!17 = !{!"gl_light_attrib", !4, i64 0, !18, i64 34304, !4, i64 34324, !4, i64 36084, !4, i64 36088, !4, i64 36092, !4, i64 36096, !8, i64 36100, !4, i64 36104, !3, i64 36108, !4, i64 36112, !4, i64 36116}
!18 = !{!"gl_lightmodel", !4, i64 0, !4, i64 16, !4, i64 17}
!19 = !{!"gl_line_attrib", !4, i64 0, !4, i64 1, !20, i64 2, !8, i64 4, !11, i64 8}
!20 = !{!"short", !4, i64 0}
!21 = !{!"gl_list_attrib", !8, i64 0}
!22 = !{!"gl_pixel_attrib", !4, i64 0, !11, i64 4, !11, i64 8, !11, i64 12, !11, i64 16, !11, i64 20, !11, i64 24, !11, i64 28, !11, i64 32, !11, i64 36, !11, i64 40, !8, i64 44, !8, i64 48, !4, i64 52, !4, i64 53, !11, i64 56, !11, i64 60, !8, i64 64, !8, i64 68, !8, i64 72, !8, i64 76, !8, i64 80, !8, i64 84, !8, i64 88, !8, i64 92, !8, i64 96, !8, i64 100, !4, i64 104, !4, i64 1128, !4, i64 2152, !4, i64 3176, !4, i64 4200, !4, i64 5224, !4, i64 6248, !4, i64 7272, !4, i64 8296, !4, i64 9320}
!23 = !{!"gl_point_attrib", !4, i64 0, !11, i64 4}
!24 = !{!"gl_polygon_attrib", !4, i64 0, !4, i64 4, !4, i64 8, !4, i64 12, !4, i64 13, !4, i64 16, !8, i64 20, !4, i64 24, !4, i64 25, !11, i64 28, !11, i64 32, !4, i64 36, !4, i64 37, !4, i64 38, !4, i64 39}
!25 = !{!"gl_scissor_attrib", !4, i64 0, !8, i64 4, !8, i64 8, !8, i64 12, !8, i64 16}
!26 = !{!"gl_stencil_attrib", !4, i64 0, !4, i64 4, !4, i64 8, !4, i64 12, !4, i64 16, !4, i64 20, !4, i64 21, !4, i64 22, !4, i64 23}
!27 = !{!"gl_texture_attrib", !8, i64 0, !4, i64 4, !4, i64 8, !8, i64 24, !4, i64 28, !4, i64 32, !4, i64 36, !4, i64 40, !4, i64 44, !4, i64 60, !4, i64 76, !4, i64 92, !4, i64 108, !4, i64 124, !4, i64 140, !4, i64 156, !3, i64 172, !3, i64 176, !3, i64 180, !3, i64 184, !3, i64 188, !3, i64 192}
!28 = !{!"gl_transform_attrib", !4, i64 0, !4, i64 4, !4, i64 100, !4, i64 106, !4, i64 107}
!29 = !{!"gl_viewport_attrib", !8, i64 0, !8, i64 4, !8, i64 8, !8, i64 12, !11, i64 16, !11, i64 20, !11, i64 24, !11, i64 28, !11, i64 32, !11, i64 36, !11, i64 40, !11, i64 44}
!30 = !{!"gl_array_attrib", !8, i64 0, !4, i64 4, !8, i64 8, !8, i64 12, !3, i64 16, !4, i64 20, !4, i64 24, !8, i64 28, !8, i64 32, !3, i64 36, !4, i64 40, !8, i64 44, !4, i64 48, !8, i64 52, !8, i64 56, !3, i64 60, !4, i64 64, !4, i64 68, !8, i64 72, !8, i64 76, !3, i64 80, !4, i64 84, !8, i64 88, !4, i64 92, !8, i64 96, !8, i64 100, !3, i64 104, !4, i64 108, !8, i64 112, !8, i64 116, !3, i64 120, !4, i64 124}
!31 = !{!"gl_pixelstore_attrib", !8, i64 0, !8, i64 4, !8, i64 8, !8, i64 12, !8, i64 16, !8, i64 20, !4, i64 24, !4, i64 25}
!32 = !{!"gl_evaluators", !33, i64 0, !33, i64 20, !33, i64 40, !33, i64 60, !33, i64 80, !33, i64 100, !33, i64 120, !33, i64 140, !33, i64 160, !34, i64 180, !34, i64 212, !34, i64 244, !34, i64 276, !34, i64 308, !34, i64 340, !34, i64 372, !34, i64 404, !34, i64 436}
!33 = !{!"gl_1d_map", !8, i64 0, !11, i64 4, !11, i64 8, !3, i64 12, !4, i64 16}
!34 = !{!"gl_2d_map", !8, i64 0, !8, i64 4, !11, i64 8, !11, i64 12, !11, i64 16, !11, i64 20, !3, i64 24, !4, i64 28}
!35 = !{!"gl_feedback", !4, i64 0, !8, i64 4, !3, i64 8, !8, i64 12, !8, i64 16}
!36 = !{!"gl_selection", !3, i64 0, !8, i64 4, !8, i64 8, !8, i64 12, !8, i64 16, !4, i64 20, !4, i64 276, !11, i64 280, !11, i64 284}
!37 = !{i64 22369621}
!38 = !{i64 11184810}
!39 = !{!2, !4, i64 54548}
!40 = !{i64 5592405}
!41 = !{i64 2796202}
!42 = !{!2, !8, i64 2344}
!43 = !{i64 5592399}
!44 = !{i64 2796199}
!45 = !{!2, !8, i64 4460}
!46 = !{i64 1398101}
!47 = !{!2, !8, i64 6580}
!48 = !{i64 1398099}
!49 = !{i64 8}
!50 = !{i64 22369613}
