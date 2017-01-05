; ModuleID = 'mesa.dlist.gl_init_lists.low.ll'
target datalayout = "E-m:m-p:32:32-i8:8:32-i16:16:32-i64:64-n32-S64"
target triple = "mips--linux-gnu"

%union.node.0 = type { i32 }

@gl_init_lists.init_flag = external hidden unnamed_addr global i1
@CurrentBlock = external hidden unnamed_addr global %union.node.0*, align 4
@CurrentListPtr = external hidden unnamed_addr global %union.node.0*, align 4
@CurrentListNum = external hidden unnamed_addr global i32, align 4
@InstSize = external hidden unnamed_addr global [106 x i32], align 4

; Function Attrs: norecurse nounwind
define void @gl_init_lists() #0 {
  %.b = load i1, i1* @gl_init_lists.init_flag, align 1
  br i1 %.b, label %2, label %1, !exec_freq !1

; <label>:1                                       ; preds = %0
  store %union.node.0* null, %union.node.0** @CurrentBlock, align 4, !tbaa !2
  store %union.node.0* null, %union.node.0** @CurrentListPtr, align 4, !tbaa !2
  store i32 0, i32* @CurrentListNum, align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 0), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 1), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 2), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 3), align 4, !tbaa !6
  store i32 8, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 4), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 5), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 6), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 7), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 8), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 9), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 10), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 11), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 12), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 13), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 14), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 15), align 4, !tbaa !6
  store i32 6, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 16), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 17), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 18), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 19), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 20), align 4, !tbaa !6
  store i32 6, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 21), align 4, !tbaa !6
  store i32 8, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 22), align 4, !tbaa !6
  store i32 9, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 23), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 25), align 4, !tbaa !6
  store i32 9, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 26), align 4, !tbaa !6
  store i32 10, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 27), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 28), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 29), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 30), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 31), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 32), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 33), align 4, !tbaa !6
  store i32 6, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 34), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 36), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 35), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 37), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 38), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 39), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 40), align 4, !tbaa !6
  store i32 6, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 41), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 42), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 43), align 4, !tbaa !6
  store i32 6, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 44), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 45), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 46), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 47), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 48), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 49), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 50), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 51), align 4, !tbaa !6
  store i32 6, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 52), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 53), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 54), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 55), align 4, !tbaa !6
  store i32 17, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 56), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 57), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 58), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 59), align 4, !tbaa !6
  store i32 11, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 60), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 61), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 62), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 63), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 64), align 4, !tbaa !6
  store i32 17, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 65), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 66), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 67), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 68), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 69), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 70), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 71), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 72), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 73), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 74), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 75), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 76), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 77), align 4, !tbaa !6
  store i32 3, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 78), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 79), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 80), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 81), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 82), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 83), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 84), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 85), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 87), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 88), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 89), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 86), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 90), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 91), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 92), align 4, !tbaa !6
  store i32 7, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 93), align 4, !tbaa !6
  store i32 9, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 94), align 4, !tbaa !6
  store i32 10, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 95), align 4, !tbaa !6
  store i32 11, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 96), align 4, !tbaa !6
  store i32 8, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 97), align 4, !tbaa !6
  store i32 10, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 98), align 4, !tbaa !6
  store i32 12, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 99), align 4, !tbaa !6
  store i32 4, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 100), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 101), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 102), align 4, !tbaa !6
  store i32 5, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 103), align 4, !tbaa !6
  store i32 2, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 104), align 4, !tbaa !6
  store i32 1, i32* getelementptr inbounds ([106 x i32], [106 x i32]* @InstSize, i32 0, i32 105), align 4, !tbaa !6
  br label %2, !exec_freq !8

; <label>:2                                       ; preds = %1, %0
  store i1 true, i1* @gl_init_lists.init_flag, align 1
  ret void, !exec_freq !1
}

attributes #0 = { norecurse nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="mips32r2" "target-features"="+mips32r2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"}
!1 = !{i64 16}
!2 = !{!3, !3, i64 0}
!3 = !{!"any pointer", !4, i64 0}
!4 = !{!"omnipotent char", !5, i64 0}
!5 = !{!"Simple C/C++ TBAA"}
!6 = !{!7, !7, i64 0}
!7 = !{!"int", !4, i64 0}
!8 = !{i64 8}
