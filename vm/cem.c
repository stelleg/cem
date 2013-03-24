#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cem.h"

#define OPGOTO 3
#define jump(ct, opcode) steps++; goto *ct[opcode];

/* Global Variables */
static unsigned long long stackSize = ((unsigned long long) 2) << 26;
static unsigned long long envSize = ((unsigned long long) 2) << 28;

Code* runCEM(Code* code){
  long long steps = 0, maxStack = 0, numVarIndir=0, numVarStraight=0, hotClos=0;
  bool varIndir;
  static const void *codetable[] = {&&PUSH, &&TAKE, &&ENTER, &&GOTO};
  /// INITIALIZATION ///
  Stack stack = (Stack) { NULL, malloc(stackSize * sizeof(Closure)) };
  if(!stack.tail){printf("Error: stack allocation failed\n"); exit(-1);}
  stack.head = stack.tail - 1;
  Environment* env = malloc(envSize * sizeof(Environment)); 
  if(!env){printf("Error: env allocation failed\n"); exit(-1);}
  Environment* freeEnv = env;
  Closure clos = (Closure) { code, NULL };
  //tmp variables
  int var; Environment* tmpenv; Closure tmpclos;
  Code go = (Code) {OPGOTO, .u=0}; //used for collapsed marker
  jump(codetable, clos.code->opcode);

GOTO:
  clos = clos.env->clos;
  jump(codetable,clos.code->opcode);

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
  printf("Result: "); traceCode(clos.code); printf("\n");
  printf("Took %llu steps \n", steps);
  printf("Hottest closure: %llu\n", hotClos);
  printf("Max stacksize = %llu \n", maxStack);
  printf("Max envsize = %lu \n", freeEnv - env);
  printf("%llu straight var lookups, %llu indirect var lookups\n",
         numVarStraight, numVarIndir);
  #endif
  return clos.code;

TAKE: 
  if(stack.head < stack.tail) goto DONE;
  if(!stack.head->code){
    (stack.head--)->env->clos = clos;
    goto TAKE;
  }
  
  
  #ifdef TRACE
  printf("TAKE: "); trace(clos,stack);
  #endif 
  
  tmpenv = clos.env;
  clos.env = freeEnv++;
  if(freeEnv == env + envSize){
    printf("Error: environment overflow\n");
    exit(-1);
  }
  *clos.env = (Environment) {*stack.head--, tmpenv};
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
  if(tmpclos.env->clos.code->opcode != OPTAKE){
  #ifdef COLLAPSED
  //Collapsed marker optimiziation
    if(stack.head >= stack.tail && !stack.head->code){
      tmpclos.env->clos.env = stack.head->env;
      tmpclos.env->clos.code = &go;
    }else 
    #endif
      *(++stack.head) = (Closure){ NULL, tmpclos.env };
  }

#ifdef DEBUG
  hotClos = ++tmpclos.env->clos.count > hotClos ? tmpclos.env->clos.count : hotClos;
#endif
  jump(codetable,clos.code->opcode);
  
  //Shouldn't reach here
  return NULL;
}
 
