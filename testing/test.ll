; ModuleID = 'fact.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind readnone uwtable
define i32 @fact(i32 %n) #0 {
entry:
  %cmp4 = icmp sgt i32 %n, 0
  br i1 %cmp4, label %for.body, label %for.end

for.body:                                         ; preds = %entry, %for.body
  %i.06 = phi i32 [ %dec, %for.body ], [ %n, %entry ]
  %res.05 = phi i32 [ %mul, %for.body ], [ 1, %entry ]
  %mul = mul nsw i32 %i.06, %res.05
  %dec = add nsw i32 %i.06, -1
  %cmp = icmp sgt i32 %dec, 0
  br i1 %cmp, label %for.body, label %for.end

for.end:                                          ; preds = %for.body, %entry
  %res.0.lcssa = phi i32 [ 1, %entry ], [ %mul, %for.body ]
  ret i32 %res.0.lcssa
}

attributes #0 = { nounwind readnone uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "unsafe-fp-math"="false" "use-soft-float"="false" }
