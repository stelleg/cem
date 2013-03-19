#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cem.h"

#define jump(ct, opcode) steps++; goto *ct[opcode];

/* Global Variables */
static unsigned long long stackSize = ((unsigned long long) 2) << 30;
static unsigned long long envSize = ((unsigned long long) 2) << 32;

Code* runCEM(Code* code){
  long long steps = 0, maxStack = 0, numVarIndir=0, numVarStraight=0, i=0, hotClos=0;
  bool varIndir;
  Environment *env, *freeEnv;
  static const void *codetable[] = {&&APP, &&LAM, &&VAR};
  /// INITIALIZATION ///
  Stack stack = (Stack) { NULL, malloc(stackSize) };
  stack.head = stack.tail - 1;
  env = malloc(envSize); 
  freeEnv = env;
  Closure clos = (Closure) { code, NULL };
  //tmp variables
  int var; Environment* tmp;
  
  jump(codetable, clos.code->opcode);

APP:
  #ifdef TRACE
  printf("APP: "); trace(clos, stack);
  #endif
  #ifdef DEBUG
  maxStack = stack.head - stack.tail > maxStack ? stack.head - stack.tail : maxStack;
  #endif
  if(stack.head == stack.tail + stackSize){
    printf("Error: stack overflow\n");
    exit(-1);
  }
  *(++stack.head) = (StackObject) { arg, .clos={ clos.code->u.m, clos.env } };
  jump(codetable,(++clos.code)->opcode);

DONE: 
  printf("Result: "); traceCode(clos.code); printf("\n");
  printf("Took %llu steps \n", steps);
  #ifdef DEBUG
  printf("Hottest closure: %llu\n", hotClos);
  printf("Max stacksize = %llu \n", maxStack);
  printf("Max envsize = %lu \n", freeEnv - env);
  printf("%llu straight var lookups, %llu indirect var lookups\n",
         numVarStraight, numVarIndir);
  #endif
  return clos.code;

LAM: 
  if(stack.head < stack.tail) goto DONE;
  if(stack.head->update) goto UPDATE;
  
  #ifdef TRACE
  printf("LAM: "); trace(clos,stack);
  #endif 
  
  tmp = clos.env;
  clos.env = freeEnv++;
  if(freeEnv == env + envSize){
    printf("Error: environment overflow\n");
    exit(-1);
  }
  *clos.env = (Environment) {(stack.head--)->clos, tmp};
  jump(codetable,(++clos.code)->opcode);

UPDATE:
  #ifdef TRACE
  printf("UPD: "); trace(clos,stack);
  #endif
  (stack.head--)->updLoc->clos = clos;
  jump(codetable,clos.code->opcode);

VAR:
  #ifdef TRACE
  printf("VAR: ");trace(clos,stack);
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

  //Shortcut, if closure is a value, dont bother pushing update, along with
  //collapsed marker update, both from Improving Lazy Kriving Machine
  if(clos.env->clos.code->opcode != OPLAM && stack.head[0].update == arg) 
    *(++stack.head) = (StackObject){upd, .updLoc=clos.env};

#ifdef DEBUG
  hotClos = ++clos.env->clos.count > hotClos ? clos.env->clos.count : hotClos;
#endif
  clos = clos.env->clos;
  jump(codetable,clos.code->opcode);
  
  //Shouldn't reach here
  return NULL;
}
 
