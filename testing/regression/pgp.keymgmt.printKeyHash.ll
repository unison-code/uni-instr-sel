; ModuleID = 'pgp.keymgmt.printKeyHash.low.ll'
target datalayout = "e-m:e-p:32:32:32-i64:64:64-i32:32:32-i16:16:16-i1:8:8-f64:64:64-f32:32:32-v64:64:64-v32:32:32-a:0-n16:32"
target triple = "hexagon"

%struct._IO_FILE.348 = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker.349*, %struct._IO_FILE.348*, i32, i32, i32, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i32, i32, [40 x i8] }
%struct._IO_marker.349 = type { %struct._IO_marker.349*, %struct._IO_FILE.348*, i32 }

@pgpout = external global %struct._IO_FILE.348*, align 4
@.str.21 = external hidden unnamed_addr constant [6 x i8], align 1
@.str.22 = external hidden unnamed_addr constant [18 x i8], align 1
@.str.23 = external hidden unnamed_addr constant [6 x i8], align 1

declare i32 @_IO_putc(i32, %struct._IO_FILE.348*) #0

; Function Attrs: nounwind
declare i32 @fprintf(%struct._IO_FILE.348* nocapture, i8* nocapture readonly, ...) #1

declare i8* @LANG(i8*) #0

; Function Attrs: nounwind
define void @printKeyHash(i8* nocapture readonly %hash, i8 zeroext %indent) #1 {
  %1 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %2 = icmp ne i8 %indent, 0
  br i1 %2, label %.selecttrue, label %.selectcont, !exec_freq !5

.selecttrue:                                      ; preds = %0
  br label %.selectcont, !exec_freq !6

.selectcont:                                      ; preds = %.selecttrue, %0
  %3 = phi i32 [ 27, %.selecttrue ], [ 1, %0 ]
  %4 = tail call i8* @LANG(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.22, i32 0, i32 0)) #2
  %5 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %1, i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.21, i32 0, i32 0), i32 %3, i8* %4)
  %6 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %7 = load i8, i8* %hash, align 1, !tbaa !7
  %8 = zext i8 %7 to i32
  %9 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %6, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %8)
  %10 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int = ptrtoint i8* %hash to i32
  %gep = add i32 %gep_int, 1
  %cgep = inttoptr i32 %gep to i8*
  %11 = load i8, i8* %cgep, align 1, !tbaa !7
  %12 = zext i8 %11 to i32
  %13 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %10, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %12)
  %14 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int1 = ptrtoint i8* %hash to i32
  %gep2 = add i32 %gep_int1, 2
  %cgep1 = inttoptr i32 %gep2 to i8*
  %15 = load i8, i8* %cgep1, align 1, !tbaa !7
  %16 = zext i8 %15 to i32
  %17 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %14, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %16)
  %18 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int3 = ptrtoint i8* %hash to i32
  %gep4 = add i32 %gep_int3, 3
  %cgep2 = inttoptr i32 %gep4 to i8*
  %19 = load i8, i8* %cgep2, align 1, !tbaa !7
  %20 = zext i8 %19 to i32
  %21 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %18, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %20)
  %22 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int5 = ptrtoint i8* %hash to i32
  %gep6 = add i32 %gep_int5, 4
  %cgep3 = inttoptr i32 %gep6 to i8*
  %23 = load i8, i8* %cgep3, align 1, !tbaa !7
  %24 = zext i8 %23 to i32
  %25 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %22, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %24)
  %26 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int7 = ptrtoint i8* %hash to i32
  %gep8 = add i32 %gep_int7, 5
  %cgep4 = inttoptr i32 %gep8 to i8*
  %27 = load i8, i8* %cgep4, align 1, !tbaa !7
  %28 = zext i8 %27 to i32
  %29 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %26, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %28)
  %30 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int9 = ptrtoint i8* %hash to i32
  %gep10 = add i32 %gep_int9, 6
  %cgep5 = inttoptr i32 %gep10 to i8*
  %31 = load i8, i8* %cgep5, align 1, !tbaa !7
  %32 = zext i8 %31 to i32
  %33 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %30, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %32)
  %34 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int11 = ptrtoint i8* %hash to i32
  %gep12 = add i32 %gep_int11, 7
  %cgep6 = inttoptr i32 %gep12 to i8*
  %35 = load i8, i8* %cgep6, align 1, !tbaa !7
  %36 = zext i8 %35 to i32
  %37 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %34, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %36)
  %38 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %39 = tail call i32 @_IO_putc(i32 32, %struct._IO_FILE.348* %38) #2
  %40 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int13 = ptrtoint i8* %hash to i32
  %gep14 = add i32 %gep_int13, 8
  %cgep7 = inttoptr i32 %gep14 to i8*
  %41 = load i8, i8* %cgep7, align 1, !tbaa !7
  %42 = zext i8 %41 to i32
  %43 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %40, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %42)
  %44 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int15 = ptrtoint i8* %hash to i32
  %gep16 = add i32 %gep_int15, 9
  %cgep8 = inttoptr i32 %gep16 to i8*
  %45 = load i8, i8* %cgep8, align 1, !tbaa !7
  %46 = zext i8 %45 to i32
  %47 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %44, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %46)
  %48 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int17 = ptrtoint i8* %hash to i32
  %gep18 = add i32 %gep_int17, 10
  %cgep9 = inttoptr i32 %gep18 to i8*
  %49 = load i8, i8* %cgep9, align 1, !tbaa !7
  %50 = zext i8 %49 to i32
  %51 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %48, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %50)
  %52 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int19 = ptrtoint i8* %hash to i32
  %gep20 = add i32 %gep_int19, 11
  %cgep10 = inttoptr i32 %gep20 to i8*
  %53 = load i8, i8* %cgep10, align 1, !tbaa !7
  %54 = zext i8 %53 to i32
  %55 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %52, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %54)
  %56 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int21 = ptrtoint i8* %hash to i32
  %gep22 = add i32 %gep_int21, 12
  %cgep11 = inttoptr i32 %gep22 to i8*
  %57 = load i8, i8* %cgep11, align 1, !tbaa !7
  %58 = zext i8 %57 to i32
  %59 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %56, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %58)
  %60 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int23 = ptrtoint i8* %hash to i32
  %gep24 = add i32 %gep_int23, 13
  %cgep12 = inttoptr i32 %gep24 to i8*
  %61 = load i8, i8* %cgep12, align 1, !tbaa !7
  %62 = zext i8 %61 to i32
  %63 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %60, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %62)
  %64 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int25 = ptrtoint i8* %hash to i32
  %gep26 = add i32 %gep_int25, 14
  %cgep13 = inttoptr i32 %gep26 to i8*
  %65 = load i8, i8* %cgep13, align 1, !tbaa !7
  %66 = zext i8 %65 to i32
  %67 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %64, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %66)
  %68 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %gep_int27 = ptrtoint i8* %hash to i32
  %gep28 = add i32 %gep_int27, 15
  %cgep14 = inttoptr i32 %gep28 to i8*
  %69 = load i8, i8* %cgep14, align 1, !tbaa !7
  %70 = zext i8 %69 to i32
  %71 = tail call i32 (%struct._IO_FILE.348*, i8*, ...) @fprintf(%struct._IO_FILE.348* %68, i8* nonnull getelementptr inbounds ([6 x i8], [6 x i8]* @.str.23, i32 0, i32 0), i32 %70)
  %72 = load %struct._IO_FILE.348*, %struct._IO_FILE.348** @pgpout, align 4, !tbaa !1
  %73 = tail call i32 @_IO_putc(i32 10, %struct._IO_FILE.348* %72) #2
  ret void, !exec_freq !5
}

attributes #0 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="hexagonv5" "target-features"="-hvx,-hvx-double" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{!2, !2, i64 0}
!2 = !{!"any pointer", !3, i64 0}
!3 = !{!"omnipotent char", !4, i64 0}
!4 = !{!"Simple C/C++ TBAA"}
!5 = !{i64 12}
!6 = !{i64 8}
!7 = !{!3, !3, i64 0}
