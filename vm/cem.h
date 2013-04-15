//Boolean
typedef unsigned char byte;
typedef byte bool;
//Opcodes
#define OPPUSH 0
#define OPTAKE 1
#define OPENTER 2
#define OPCONST 3
#define OPOP  4
#define OPLIT 5
/* Types */
typedef struct Code {byte opcode; union {struct Code* m; int fVar; int var; int l; int op;} u;} Code;
#ifdef DEBUG
typedef struct Closure {Code* code; union {struct Environment* env; int i;}; unsigned long count;} Closure;
#else
typedef struct Closure {Code* code; union {struct Environment* env; int i;};} Closure;
#endif

typedef struct Stack {Closure* head; Closure* tail;} Stack;
typedef struct Environment {Closure clos; struct Environment* next;} Environment;

void trace(Closure clos, Stack stack);
void traceCode(Closure clos);

Closure* runCEM();
