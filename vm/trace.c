/* Tracing functions for debugging */
#include <stdio.h>
#include "cem.h"

//Debugging trace functions
void traceCode(Code* code){
  switch(code->opcode){
    case OPPUSH:
      printf("("); traceCode(code+1); printf(" "); traceCode(code->u.m); printf(")");
      break;
    case OPTAKE:
      printf("\\"); traceCode(code+1);
      break;
    case OPENTER: 
      printf("%d", code->u.var); 
      break;
  }
}

void traceEnv(Environment* env){
  if(env){
      printf("\t[");
        traceCode(env->clos.code); 
        printf(", ");
        printf("%p", env->clos.env);
      printf("]\n");
      traceEnv(env->next);
  }
}

void traceStack(Stack stack){
  Closure* stackptr;
  for(stackptr = stack.head; stackptr >= stack.tail; stackptr--){
    printf("\t{");
      if(!stackptr->code) 
        printf("update %p", stackptr->env);
      else{
        printf("arg "); 
        traceCode(stackptr->code); 
        printf(", ");
        printf("%p", stackptr->env);
      }
    printf("}\n");
  }
}

//Full state trace function
void trace(Closure clos, Stack stack){
  printf("Stacksize: %ld, ", stack.head + 1 - stack.tail); 
  traceCode(clos.code); printf("\n");
  traceEnv(clos.env);
  traceStack(stack);
}

