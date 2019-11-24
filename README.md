# Jack

Jack is a simple pure-functional programming language inspired by ML.

This repository hosts a 3-step compiler written in Rust that does parsing, semantic analysis (including type checking) and generates LLVM IR.
It then runs the IR through the LLVM JIT.

Here's an example of a Jack program you can run using `cargo run fib.ml`
```ml
fun fib(n: int): int =
    if n == 0 then
        0
    else if n == 1 then
        1
    else
        fib(n-1) + fib(n-2)

fun main(): bool = let fib9 = fib(9) in fib9 == 34
```

which compiles down to:
```ll
; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define i64 @fib(i64) {
  %2 = icmp eq i64 %0, 0
  br i1 %2, label %then, label %else

then:                                             ; preds = %1
  br label %fi

else:                                             ; preds = %1
  %3 = icmp eq i64 %0, 1
  br i1 %3, label %then1, label %else2

then1:                                            ; preds = %else
  br label %fi3

else2:                                            ; preds = %else
  %4 = sub i64 %0, 1
  %5 = call i64 @fib(i64 %4)
  %6 = sub i64 %0, 2
  %7 = call i64 @fib(i64 %6)
  %8 = add i64 %5, %7
  br label %fi3

fi3:                                              ; preds = %else2, %then1
  %phi = phi i64 [ 1, %then1 ], [ %8, %else2 ]
  br label %fi

fi:                                               ; preds = %fi3, %then
  %phi4 = phi i64 [ 0, %then ], [ %phi, %fi3 ]
  ret i64 %phi4
}

define i1 @main() {
  %1 = call i64 @fib(i64 9)
  %2 = icmp eq i64 %1, 34
  ret i1 %2
}
```
