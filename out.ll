; ModuleID = 'sum'
source_filename = "sum"

@fmt_str = private unnamed_addr constant [10 x i8] c"debug: %d\00", align 1

declare i32 @printf(ptr, ...)

define void @print_int(i32 %0) {
entry:
  %printf = call i32 (ptr, ...) @printf(ptr @fmt_str, i32 %0)
  ret void
}

define void @main() {
entry:
  call void @print_int(i32 42)
  ret void
}
