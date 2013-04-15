#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cem.h"

#define jump(ct, opcode) steps++; goto *ct[opcode];

/* Global Variables */
#ifdef DEBUG
static unsigned long long stackSize = ((unsigned long long) 2) << 24;
static unsigned long long envSize = ((unsigned long long) 2) << 26;
#else
static unsigned long long stackSize = ((unsigned long long) 2) << 26;
static unsigned long long envSize = ((unsigned long long) 2) << 28;
#endif

Closure* runCEM(Code* code){
  long long steps = 0, maxStack = 0, numVarIndir=0, numVarStraight=0, hotClos=0;
  bool varIndir;
  static const void *codetable[] = {&&PUSH, &&TAKE, &&ENTER, &&CONST, &&OP, &&LIT};
  /// INITIALIZATION ///
  Stack stack = (Stack) { NULL, malloc(stackSize * sizeof(Closure)) };
  stack.head = stack.tail - 1;
  if(!stack.tail){printf("Error: stack allocation failed\n"); exit(-1);}
  Environment* env = malloc(envSize * sizeof(Environment)); 
  Environment* freeEnv = env;
  if(!env){printf("Error: env allocation failed\n"); exit(-1);}
  Closure* closure;
  #ifdef DEBUG
  Closure clos = (Closure) {code, NULL, 0};
  #else
  Closure clos = (Closure) {code, NULL};
  #endif
  //tmp variables
  int var, i, i1, i2; Closure tmpclos; Environment* tmpenv; Code lit = (Code) {OPLIT, 0};
  jump(codetable, clos.code->opcode);

PUSH:
  #ifdef TRACE
  printf("PUSH: "); trace(clos, stack);
  #endif
  #ifdef DEBUG
  maxStack = stack.head - stack.tail > maxStack ? stack.head - stack.tail : maxStack;
  #endif
  if(stack.head == stack.tail + stackSize){
    printf("Error: stack overflow\n");
    exit(-1);
  }
  *(++stack.head) = (Closure) {clos.code->u.m, clos.env };
  jump(codetable,(++clos.code)->opcode);

DONE: 
  #ifdef DEBUG
  printf("Result: "); traceCode(clos); printf("\n");
  printf("Took %llu steps \n", steps);
  printf("Hottest closure: %llu\n", hotClos);
  printf("Max stacksize = %llu \n", maxStack);
  printf("Max envsize = %lu \n", freeEnv - env);
  printf("%llu straight var lookups, %llu indirect var lookups\n",
         numVarStraight, numVarIndir);
  #endif
  closure = malloc(sizeof(Closure));
  *closure = clos;
  return closure;

TAKE: 
  if(stack.head < stack.tail) goto DONE;
  if(!stack.head->code){
    (stack.head--)->env->clos = clos;
    goto TAKE;
  }
  
  #ifdef TRACE
  printf("TAKE: "); trace(clos,stack);
  #endif 
  
  *freeEnv = (Environment) {*stack.head--, clos.env};
  clos.env = freeEnv++;
  if(freeEnv == env + envSize){
    printf("Error: environment overflow\n");
    exit(-1);
  }
  jump(codetable,(++clos.code)->opcode);

ENTER:
  #ifdef TRACE
  printf("ENTER: ");trace(clos,stack);
  #endif

  #ifdef DEBUG
  varIndir = 0;
  for(var=clos.code->u.var; var > 0; var--){
    if(clos.env - clos.env -> next > 1){
      varIndir=1;
    }
    clos.env = clos.env->next;
  }
  if(varIndir) numVarIndir ++;
  else numVarStraight ++;
  clos.env->clos.count ++;
  #else
  for(var=clos.code->u.var; var > 0; var--)
    clos.env = clos.env->next;
  #endif 

  tmpclos = clos;
  clos = clos.env->clos;

#ifdef DEBUG
  hotClos = ++tmpclos.env->clos.count > hotClos ? tmpclos.env->clos.count : hotClos;
#endif
  jump(codetable,clos.code->opcode);
 
CONST: 
  clos = (Closure) {&lit, .i=clos.code->u.l};
  goto LIT;

LIT:
  #ifdef TRACE
  printf("LIT: "); trace(clos,stack);
  #endif
  if(stack.head < stack.tail) goto DONE;
  if(!stack.head->code){
    (stack.head--)->env->clos = clos;
    goto LIT;
  }
 
  tmpclos = clos;
  clos = *stack.head;
  *stack.head = tmpclos;
  jump(codetable, clos.code->opcode);

OP: 
  #ifdef TRACE
  printf("OP: "); trace(clos,stack);
  #endif
  i2 = (*stack.head--).i;
  i1 = (*stack.head--).i;
  switch(clos.code->u.op){
    case 0: i = i1 +  i2; break;
    case 1: i = i1 -  i2; break;
    case 2: i = i1 *  i2; break;
    case 3: i = i1 /  i2; break;
    case 4: i = i1 == i2; break;
  }
  clos = (Closure) {&lit, .i=i};
  goto LIT;

  //Shouldn't reach here
  return NULL;
}
 
