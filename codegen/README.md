```bash
$ clang -target armv4t-none-eabi -march=armv4t -O0 -S -o return-exp.s return-exp.c
```

`returne-xp.c`
```c
int main(void) {
  return 3+5;
}
```

The part of `return-exp.s`
```armasm
  .text
  .globl  main                            @ -- Begin function main
  .type main,%function

main:
  .fnstart
@ %bb.0:
  .pad  #4
  sub sp, sp, #4
  mov r0, #0
  str r0, [sp]
  mov r0, #8
  add sp, sp, #4
  bx  lr
```

Current AST
```
AST: #<AST-TRANSLATION-UNIT-DECL {70079BC133}>
AST: #<AST-FUNCTION-DECL {70079B7C03}>
return: #S(DECLSPEC
           :TYPE-SPEC DST-INT
           :TYPE-QUALIFIER NIL
           :STORAGE-SPEC NIL
           :FUNC-SPEC NIL)
name: main
AST: #<AST-COMPOUND-STMT {70079B07E3}>
AST: #<AST-RETURN-STMT {700791C1E3}>
AST: #<AST-BINARY-OPERATOR {700788C963}>
  OP: OP-PLUS
  LHS: #<AST-INTEGER-LITERAL {700780C3A3}>
    AST: #<AST-INTEGER-LITERAL {700780C3A3}>
      VALUE: 3
  RHS: #<AST-INTEGER-LITERAL {700780C673}>
    AST: #<AST-INTEGER-LITERAL {700780C673}>
      VALUE: 4
```