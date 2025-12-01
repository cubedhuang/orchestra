.orig x3000
  LEA R4, globals
  LD R6, stack_base
  ADD R5, R6, #0
  JSR fn_main
  HALT
stack_base  .fill xF000
globals
  .fill 6
  .fill 7
fn_main
  ADD R6, R6, #-1
  ADD R6, R6, #-1
  STR R7, R6, #0
  ADD R6, R6, #-1
  STR R5, R6, #0
  ADD R5, R6, #-1
  ADD R6, R6, #-1
  ;; GetGlobalAddress(0)
  ADD R0, R4, #0
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; Call(spooky [1])
  JSR fn_spooky
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ADD R6, R6, #1
  ADD R6, R6, #-1
  STR R0, R6, #0
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
fn_spooky
  ADD R6, R6, #-1
  ADD R6, R6, #-1
  STR R7, R6, #0
  ADD R6, R6, #-1
  STR R5, R6, #0
  ADD R5, R6, #-1
  ADD R6, R6, #-1
  ;; GetParameter(0)
  LDR R0, R5, #4
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; Push(1)
  AND R0, R0, #0
  ADD R0, R0, #1
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; Add
  ADD R6, R6, #1
  LDR R0, R6, #-1
  ADD R6, R6, #1
  LDR R1, R6, #-1
  ADD R0, R0, R1
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; Push(9)
  AND R0, R0, #0
  ADD R0, R0, #9
  ADD R6, R6, #-1
  STR R0, R6, #0
  ;; StoreIndirect
  ADD R6, R6, #1
  LDR R1, R6, #-1
  ADD R6, R6, #1
  LDR R0, R6, #-1
  STR R1, R0, #0
  ADD R6, R6, #-1
  STR R1, R6, #0
  ;; Pop
  ADD R6, R6, #1
  LDR R0, R6, #-1
fn_spooky_teardown
  ADD R6, R5, #1
  LDR R5, R6, #0
  ADD R6, R6, #1
  LDR R7, R6, #0
  ADD R6, R6, #1
  RET
.end
