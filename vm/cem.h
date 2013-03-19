//Boolean
typedef unsigned char byte;
typedef byte bool;
//Opcodes
#define OPAPP 0
#define OPLAM 1
#define OPVAR 2

/* Types */
typedef struct Code {byte opcode; union {struct Code* m; int fVar; int var;} u;} Code;
#ifdef DEBUG
typedef struct Closure {Code* code; struct Environment* env; unsigned long count;} Closure;
#else
typedef struct Closure {Code* code; struct Environment* env;} Closure;
#endif
typedef enum StackMarker {
  arg,
  upd
} StackMarker;
typedef struct StackObject {StackMarker update; union {Closure clos; struct Environment* updLoc;};} StackObject;
typedef struct Stack {struct StackObject* head; struct StackObject* tail;} Stack;
typedef struct Environment {Closure clos; struct Environment* next;} Environment;

void trace(Closure clos, Stack stack);
void traceCode(Code* code);

Code* runCEM();
