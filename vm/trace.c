/* Tracing functions for debugging */
#include <stdio.h>
#include "cem.h"

//Debugging trace functions
void traceCode(Closure clos){
  switch(clos.code->opcode){
    case OPPUSH:
      printf("("); traceCode((Closure){clos.code+1,clos.env}); 
      printf(" "); 
      traceCode((Closure){clos.code->u.m, clos.env}); printf(")");
      break;
    case OPTAKE:
      printf("\\"); traceCode((Closure){clos.code+1, clos.env});
      break;
    case OPENTER: 
      printf("e:%d", clos.code->u.var); 
      break;
    case OPLIT:
      printf("l:%d", clos.i);
      break;
    case OPCONST:
      printf("c:%d", clos.code->u.l);
      break;
    case OPOP:
      printf("op:%d", clos.code->u.op);
      break;
  }
}

void traceEnv(Environment* env){
  if(env){
    printf("\t[");
      traceCode((Closure){env->clos.code, env->clos.env}); 
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
        traceCode(*stackptr); 
        printf(", ");
        printf("%p", stackptr->env);
      }
    printf("}\n");
  }
}

//Full state trace function
void trace(Closure clos, Stack stack){
  traceCode(clos); printf("\n");
  traceStack(stack);
  if(clos.code->opcode == OPLIT || clos.code->opcode == OPCONST)
    printf("\t[%d]\n", clos.i);
  else
    traceEnv(clos.env);
}

