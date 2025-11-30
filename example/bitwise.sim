.orig x3000
  LEA R4, globals
  LD R6, stack_base
  ADD R5, R6, #0
  JSR fn_main
  HALT
stack_base  .fill xF000
globals
  .fill 0
  .fill 0
  .fill 0
  .fill 0
fn_main
  ADD R6, R6, #-1
  ADD R6, R6, #-1
  STR R7, R6, #0
  ADD R6, R6, #-1
  STR R5, R6, #0
  ADD R5, R6, #-1
  ADD R6, R6, #-2
  ;; Push(9)
  AND R0, R0, #0
  ADD R0, R0, #9
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; SetLocal(0)
  LDR R0, R6, #0
  STR R0, R5, #0
  ;; Push(7)
  AND R0, R0, #0
  ADD R0, R0, #7
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; SetLocal(1)
  LDR R0, R6, #0
  STR R0, R5, #-1
  ;; GetLocal(0)
  LDR R0, R5, #0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; GetLocal(1)
  LDR R0, R5, #-1
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; BitwiseOr
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ADD R6, R6, #1
  LDR R1, R6, #-1
  NOT R0, R0
  NOT R1, R1
  AND R0, R0, R1
  NOT R0, R0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; SetGlobal(0)
  LDR R0, R6, #0
  STR R0, R4, #0
  ;; Pop
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ;; GetLocal(0)
  LDR R0, R5, #0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; GetLocal(1)
  LDR R0, R5, #-1
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; BitwiseXor
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ADD R6, R6, #1
  LDR R1, R6, #-1
  NOT R2, R0
  AND R2, R2, R1
  NOT R3, R1
  AND R3, R3, R0
  ADD R0, R2, R3
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; SetGlobal(1)
  LDR R0, R6, #0
  STR R0, R4, #1
  ;; Pop
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ;; GetLocal(0)
  LDR R0, R5, #0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; GetLocal(1)
  LDR R0, R5, #-1
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; BitwiseAnd
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ADD R6, R6, #1
  LDR R1, R6, #-1
  AND R0, R0, R1
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; SetGlobal(2)
  LDR R0, R6, #0
  STR R0, R4, #2
  ;; Pop
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ;; GetLocal(0)
  LDR R0, R5, #0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; BitwiseNot
  ADD R6, R6, #1
  LDR R0, R6, #-1
  NOT R0, R0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; SetGlobal(3)
  LDR R0, R6, #0
  STR R0, R4, #3
  ;; Pop
  ADD R6, R6, #1
  LDR R0, R6, #-1
fn_main_teardown
  ADD R6, R5, #1
  LDR R5, R6, #0
  ADD R6, R6, #1
  LDR R7, R6, #0
  ADD R6, R6, #1
  RET
.end
