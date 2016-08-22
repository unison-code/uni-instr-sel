; ModuleID = 'fact.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @fact(i32 %n) #0 {
entry:
  %n.addr = alloca i32, align 4
  %res = alloca i32, align 4
  store i32 %n, i32* %n.addr, align 4
  %0 = load i32* %n.addr, align 4
  store i32 %0, i32* %res, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %1 = load i32* %n.addr, align 4
  %cmp = icmp sgt i32 %1, 1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %2 = load i32* %n.addr, align 4
  %3 = load i32* %res, align 4
  %mul = mul nsw i32 %3, %2
  store i32 %mul, i32* %res, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %4 = load i32* %n.addr, align 4
  %dec = add nsw i32 %4, -1
  store i32 %dec, i32* %n.addr, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %5 = load i32* %res, align 4
  ret i32 %5
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"clang version 3.4 (tags/RELEASE_34/final)"}
