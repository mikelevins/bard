#include <stdbool.h>
#include <stdlib.h>
#include "cbard.h"

int main () {
  BoxedBardValue* box;

  //------------------------------------------------------------

  printf("\n\ntesting BARD_UNDEFINED");
  box = bard_box_undefined();
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    int* valptr = (int*)box->value;
    int val = *valptr;

    if(tag==BARD_UNDEFINED) {
      printf("\nSUCCESS: tag is BARD_UNDEFINED\n\n");
    } else {
      printf("\nFAILURE: tag (%d) is not BARD_UNDEFINED\n\n",tag);
    }
  }

  //------------------------------------------------------------

  printf("testing BARD_NULL");
  box = bard_box_null();
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    int* valptr = (int*)box->value;
    int val = *valptr;

    if(tag==BARD_NULL) {
      printf("\nSUCCESS: tag is BARD_NULL\n\n");
    } else {
      printf("\nFAILURE: tag (%d) is not BARD_NULL\n\n",tag);
    }
  }

  //------------------------------------------------------------

  printf("testing BARD_CHARACTER");
  box = bard_box_character('B');
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    char* valptr = (char*)box->value;
    char val = *valptr;

    if(tag==BARD_CHARACTER) {
      printf("\nSUCCESS: tag is BARD_CHARACTER");
    } else {
      printf("\nFAILURE: tag is not BARD_CHARACTER\n\n");
    }

    if(val=='B') {
      printf("\nSUCESS: value is correct  (%c)\n\n",val);
    } else {
      printf("\nFAILURE: value is incorrect! (%c)\n\n",val);
    }

  }

  //------------------------------------------------------------

  printf("testing BARD_BOOLEAN");
  box = bard_box_boolean(false);
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    bool* valptr = (bool*)box->value;
    bool val = *valptr;

    if(tag==BARD_BOOLEAN) {
      printf("\nSUCCESS: tag is BARD_BOOLEAN");
    } else {
      printf("\nFAILURE: tag is not BARD_BOOLEAN\n\n");
    }

    if(val==false) {
      printf("\nSUCESS: value is correct  (%d)\n\n",val);
    } else {
      printf("\nFAILURE: value is incorrect! (%d)\n\n",val);
    }

  }

  //------------------------------------------------------------

  printf("testing BARD_INTEGER");
  box = bard_box_integer(12345);
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    int* valptr = (int*)box->value;
    int val = *valptr;

    if(tag==BARD_INTEGER) {
      printf("\nSUCCESS: tag is BARD_INTEGER");
    } else {
      printf("\nFAILURE: tag is not BARD_INTEGER\n\n");
    }

    if(val==12345) {
      printf("\nSUCESS: value is correct  (%d)\n\n",val);
    } else {
      printf("\nFAILURE: value is incorrect! (%d)\n\n",val);
    }
  }

  //------------------------------------------------------------

  printf("testing BARD_FLOAT");
  box = bard_box_float(123.45);
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    float* valptr = (float*)box->value;
    float val = *valptr;

    if(tag==BARD_FLOAT) {
      printf("\nSUCCESS: tag is BARD_FLOAT");
    } else {
      printf("\nFAILURE: tag is not BARD_FLOAT\n\n");
    }

    if((123.44<val)&&(val<123.46)) {
      printf("\nSUCESS: value is correct  (%f)\n\n",val);
    } else {
      printf("\nFAILURE: value is incorrect! (%f)\n\n",val);
    }

  }

  //------------------------------------------------------------


  printf("testing BARD_RATIO");
  box = bard_box_ratio(2,3);
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    float* valptr = (float*)box->value;
    float val = *valptr;

    if(tag==BARD_FLOAT) {
      printf("\nSUCCESS: tag is BARD_FLOAT");
    } else {
      printf("\nFAILURE: tag is not BARD_FLOAT\n\n");
    }

    if((0.65<val)&&(val<0.67)) {
      printf("\nSUCESS: value is correct  (%f)\n\n",val);
    } else {
      printf("\nFAILURE: value is incorrect! (%f)\n\n",val);
    }
  }

  //------------------------------------------------------------

  printf("testing BARD_SYMBOL");
  box = bard_box_symbol("Foobar");
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    const char* valptr = (const char*)box->value;

    if(tag==BARD_SYMBOL) {
      printf("\nSUCCESS: tag is BARD_SYMBOL");
    } else {
      printf("\nFAILURE: tag is not BARD_SYMBOL\n\n");
    }

    if(strcmp(valptr,"Foobar")==0) {
      printf("\nSUCESS: value is correct  (%s)\n\n",valptr);
    } else {
      printf("\nFAILURE: value is incorrect! (%s)\n\n",valptr);
    }
  }

  //------------------------------------------------------------

  printf("testing BARD_KEYWORD");
  box = bard_box_keyword("Barney");
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    const char* valptr = (const char*)box->value;

    if(tag==BARD_KEYWORD) {
      printf("\nSUCCESS: tag is BARD_KEYWORD");
    } else {
      printf("\nFAILURE: tag is not BARD_KEYWORD\n\n");
    }

    if(strcmp(valptr,"Barney")==0) {
      printf("\nSUCESS: value is correct  (%s)\n\n",valptr);
    } else {
      printf("\nFAILURE: value is incorrect! (%s)\n\n",valptr);
    }
  }

  //------------------------------------------------------------

  printf("testing BARD_TEXT");
  box = bard_box_text("Frobnicate!");
  if (box==NULL) {
    printf("\nERROR: box was NULL. \n");
  } else {
    int tag = box->type;
    const char* valptr = (const char*)box->value;

    if(tag==BARD_TEXT) {
      printf("\nSUCCESS: tag is BARD_TEXT");
    } else {
      printf("\nFAILURE: tag is not BARD_TEXT\n\n");
    }

    if(strcmp(valptr,"Frobnicate!")==0) {
      printf("\nSUCESS: value is correct  (%s)\n\n",valptr);
    } else {
      printf("\nFAILURE: value is incorrect! (%s)\n\n",valptr);
    }
  }

  //------------------------------------------------------------


}
