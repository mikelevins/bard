#define ___VERSION 406006
#include "gambit.h"

#define BardValue ___SCMOBJ

// type tags
#define BARD_UNDEFINED -1
#define BARD_NULL 0
#define BARD_CHARACTER 1
#define BARD_BOOLEAN 2
#define BARD_NUMBER 3
#define BARD_SYMBOL 4
#define BARD_KEYWORD 5
#define BARD_TEXT 6
#define BARD_LIST 7
#define BARD_FRAME 8
#define BARD_PRIMITIVE 9
#define BARD_FUNCTION 10
#define BARD_METHOD 11
#define BARD_TYPE 12
#define BARD_UNRECOGNIZED 127

// error codes
#define BARD_ERR_NO_ERROR 0
#define BARD_ERR_GENERAL_EXCEPTION 1
#define BARD_ERR_HEAP_OVERFLOW 2
#define BARD_ERR_STACK_OVERFLOW 3
#define BARD_ERR_OS_EXCEPTION 4
#define BARD_ERR_UNDEFINED_ENVIRONMENT_VARIABLE 5
#define BARD_ERR_NO_SUCH_FILE 6
#define BARD_ERR_SCHEDULER_EXCEPTION 7
#define BARD_ERR_DEADLOCK_DETECTED 8
#define BARD_ERR_ABANDONED_MUTEX 9
#define BARD_ERR_JOIN_TIMEOUT 10
#define BARD_ERR_THREAD_ALREADY_STARTED 11
#define BARD_ERR_THREAD_TERMINATED 12
#define BARD_ERR_UNCAUGHT_EXEPTION 13
#define BARD_ERR_ERROR_CONVERTING_TO_BARD 14
#define BARD_ERR_ERROR_CONVERTING_TO_C 15
#define BARD_ERR_ERROR_RETURNING_FROM_C 16
#define BARD_ERR_BAD_DATA_FORMAT 17
#define BARD_ERR_EXPRESSION_SYNTAX_ERROR 18
#define BARD_ERR_UNDEFINED_GLOBAL 19
#define BARD_ERR_WRONG_TYPE 20
#define BARD_ERR_OUT_OF_RANGE 21
#define BARD_ERR_DIVIDE_BY_ZERO 22
#define BARD_ERR_IMPROPER_LIST_LENGTH 23
#define BARD_ERR_WRONG_NUMBER_OF_ARGUMENTS 24
#define BARD_ERR_TOO_MANY_ARGUMENTS 25
#define BARD_ERR_OPERATOR_NOT_A_PROCEDURE 26
#define BARD_ERR_UNKNOWN_KEYWORD_ARGUMENT 27
#define BARD_ERR_KEYWORD_EXPECTED 28
#define BARD_ERR_NONCONTINUABLE_EXCEPTION 29
#define BARD_ERR_UNBOUND_TABLE_KEY 30
#define BARD_ERR_UNIDENTIFIED_ERROR 127


// API functions
extern bool bard_error_status();
extern int bard_last_error_code ();
extern const char* bard_last_error_message ();
extern void clear_last_bard_error ();
extern void clear_all_bard_errors ();
extern const char* bard_version ();
extern bool init_bard();
extern int bard_type(BardValue expr);
extern const char* bard_typename(BardValue expr);

extern BardValue bard_read(const char* str);
extern BardValue bard_read_lines(const char* str);
extern BardValue bard_read_nonempty_lines(const char* str);

extern BardValue bard_eval(BardValue obj);
extern int bard_object_to_id(BardValue obj);
extern BardValue bard_id_to_object(int oid);


extern BardValue bard_load_from_string(const char* str);
extern BardValue bard_get(BardValue obj, BardValue key);
extern BardValue bard_keys(BardValue obj);

extern const char* bard_print(BardValue expr);

extern bool bard_is_nothing(BardValue obj);
extern bool bard_is_empty(BardValue obj);
extern char as_char(BardValue expr);
extern bool as_bool(BardValue expr);
extern int as_int(BardValue expr);
extern float as_float(BardValue expr);
extern const char* as_string(BardValue expr);

