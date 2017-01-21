#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "cbard.h"

BoxedBardValue* bard_box_undefined() {
  BoxedBardValue* box;
  int* outval;

  box = malloc(sizeof(BoxedBardValue));
  outval = malloc(sizeof(int));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  (*outval)=0;
  box->type=BARD_UNDEFINED;
  box->value=outval;

  return box;
}

BoxedBardValue* bard_box_null() {
  BoxedBardValue* box;
  int* outval;

  box = malloc(sizeof(BoxedBardValue));
  outval = malloc(sizeof(int));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  (*outval)=0;
  box->type=BARD_NULL;
  box->value=outval;

  return box;
}

BoxedBardValue* bard_box_character(char inch) {
  BoxedBardValue* box;
  char* outch;

  box = malloc(sizeof(BoxedBardValue));
  outch = malloc(sizeof(char));

  if (box==NULL){return NULL;}
  if (outch==NULL){return NULL;}

  (*outch)=inch;
  box->type=BARD_CHARACTER;
  box->value=outch;

  return box;
}


BoxedBardValue* bard_box_boolean(bool inval) {
  BoxedBardValue* box;
  bool* outval;

  box = malloc(sizeof(BoxedBardValue));
  outval = malloc(sizeof(bool));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  (*outval)=inval;
  box->type=BARD_BOOLEAN;
  box->value=outval;

  return box;
}


BoxedBardValue* bard_box_integer(int inval) {
  BoxedBardValue* box;
  int* outval;

  box = malloc(sizeof(BoxedBardValue));
  outval = malloc(sizeof(int));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  (*outval)=inval;
  box->type=BARD_INTEGER;
  box->value=outval;

  return box;
}

BoxedBardValue* bard_box_float(float inval) {
  BoxedBardValue* box;
  float* outval;

  box = malloc(sizeof(BoxedBardValue));
  outval = malloc(sizeof(float));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  (*outval)=inval;
  box->type=BARD_FLOAT;
  box->value=outval;

  return box;
}


BoxedBardValue* bard_box_ratio(int num, int denom) {
  BoxedBardValue* box;
  float* outval;

  box = malloc(sizeof(BoxedBardValue));
  outval = malloc(sizeof(float));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  float fnum=(float)num;
  float fdenom=(float)denom;
  (*outval)=(fnum/fdenom);
  box->type=BARD_FLOAT;
  box->value=outval;

  return box;
}

BoxedBardValue* bard_box_symbol(const char* inval) {
  BoxedBardValue* box;
  char* outval;

  box = malloc(sizeof(BoxedBardValue));
  int len = strlen(inval);
  outval = malloc(1+len*sizeof(char));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  strcpy(outval,inval);
  box->type=BARD_SYMBOL;
  box->value=outval;

  return box;
}

BoxedBardValue* bard_box_keyword(const char* inval) {
  BoxedBardValue* box;
  char* outval;

  box = malloc(sizeof(BoxedBardValue));
  int len = strlen(inval);
  outval = malloc(1+len*sizeof(char));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  strcpy(outval,inval);
  box->type=BARD_KEYWORD;
  box->value=outval;

  return box;
}

BoxedBardValue* bard_box_text(const char* inval) {
  BoxedBardValue* box;
  char* outval;

  box = malloc(sizeof(BoxedBardValue));
  int len = strlen(inval);
  outval = malloc(1+len*sizeof(char));

  if (box==NULL){return NULL;}
  if (outval==NULL){return NULL;}

  strcpy(outval,inval);
  box->type=BARD_TEXT;
  box->value=outval;

  return box;
}

