#define ___VERSION 406006
#include "gambit.h"

#define BardValue ___SCMOBJ

#define BARD_UNDEFINED -1
#define BARD_NULL 0
#define BARD_BOOLEAN 1
#define BARD_NUMBER 2
#define BARD_SYMBOL 3
#define BARD_KEYWORD 4
#define BARD_TEXT 5
#define BARD_LIST 6
#define BARD_FRAME 7
#define BARD_UNRECOGNIZED 127

extern bool bard_error_status();
extern char* bard_error ();
extern void clear_bard_error ();
extern const char* bard_version ();
extern bool init_bard();
extern int bard_type(BardValue expr);
extern const char* bard_typename(BardValue expr);
extern BardValue bard_read(const char* str);
extern BardValue bard_eval(BardValue obj);
extern const char* bard_print(BardValue expr);
extern char as_char(BardValue expr);
extern bool as_bool(BardValue expr);
extern int as_int(BardValue expr);
extern float as_float(BardValue expr);
extern const char* as_string(BardValue expr);
