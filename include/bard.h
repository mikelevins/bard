#define ___VERSION 406001
#include "gambit.h"
#import <Foundation/Foundation.h>

#define BARD_NULL 0
#define BARD_BOOLEAN 1
#define BARD_NUMBER 2
#define BARD_SYMBOL 3
#define BARD_KEYWORD 4
#define BARD_TEXT 5
#define BARD_LIST 6
#define BARD_FRAME 7
#define BARD_UNRECOGNIZED 255

extern bool bard_error_status();
extern char* bard_error ();
extern void clear_bard_error ();
extern NSString* bard_version ();
extern bool init_bard();
extern int bard_type(___SCMOBJ expr);
extern ___SCMOBJ bard_read(NSString* text);
extern ___SCMOBJ bard_eval(___SCMOBJ expr);
extern bool bard_load(NSString* text);
extern NSString* bard_show(___SCMOBJ expr);
extern NSMutableArray* as_array(___SCMOBJ expr);
