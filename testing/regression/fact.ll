; ModuleID = 'fact.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @fact(i32 %n) #0 {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %res.0 = phi i32 [ %n, %entry ], [ %mul, %for.inc ]
  %n.addr.0 = phi i32 [ %n, %entry ], [ %dec, %for.inc ]
  %cmp = icmp sgt i32 %n.addr.0, 1
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %mul = mul nsw i32 %res.0, %n.addr.0
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %dec = add nsw i32 %n.addr.0, -1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 %res.0
}

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.1 (http://llvm.org/git/clang.git 07a6361e0f32f699d47c124106e7911b584974d4) (http://llvm.org/git/llvm.git 051e787f26dbfdc26cf61a57bc82ca00dcb812e8)"}
