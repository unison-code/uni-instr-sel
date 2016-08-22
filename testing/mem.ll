; ModuleID = 'mem.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @func(i32* %a) #0 {
entry:
  %a.addr = alloca i32*, align 8
  %x = alloca i32, align 4
  store i32* %a, i32** %a.addr, align 8
  %0 = load i32** %a.addr, align 8
  %1 = load i32* %0, align 4
  %add = add nsw i32 %1, 1
  store i32 %add, i32* %x, align 4
  %2 = load i32* %x, align 4
  %3 = load i32** %a.addr, align 8
  store i32 %2, i32* %3, align 4
  %4 = load i32* %x, align 4
  ret i32 %4
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4 (tags/RELEASE_34/final)"}
