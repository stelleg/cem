//Boolean
typedef unsigned char byte;
typedef byte bool;
//Opcodes
#define OPPUSH 0
#define OPTAKE 1
#define OPENTER 2

/* Types */
typedef struct Code {byte opcode; union {struct Code* m; int fVar; int var;} u;} Code;
#ifdef DEBUG
typedef struct Closure {Code* code; struct Environment* env; unsigned long count;} Closure;
#else
typedef struct Closure {Code* code; struct Environment* env;} Closure;
#endif

typedef struct Stack {Closure* head; Closure* tail;} Stack;
typedef struct Environment {Closure clos; struct Environment* next;} Environment;

void trace(Closure clos, Stack stack);
void traceCode(Code* code);

Code* runCEM();
