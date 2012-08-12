#define ___VERSION 406006
#include "gambit.h"

#define BardValue ___SCMOBJ

// type tags
#define BARD_UNDEFINED -1
#define BARD_NULL 0
#define BARD_CHARACTER 1
#define BARD_BOOLEAN 2
#define BARD_INTEGER 3
#define BARD_FLOAT 4
#define BARD_RATIO 5
#define BARD_SYMBOL 6
#define BARD_KEYWORD 7
#define BARD_TEXT 8
#define BARD_LIST 9
#define BARD_FRAME 10
#define BARD_PRIMITIVE 11
#define BARD_FUNCTION 12
#define BARD_METHOD 13
#define BARD_TYPE 14
#define BARD_UNRECOGNIZED 127

// type names
#define BARD_UNDEFINED_NAME "Undefined"
#define BARD_NULL_NAME "Null"
#define BARD_CHARACTER_NAME "Character"
#define BARD_BOOLEAN_NAME "Boolean"
#define BARD_INTEGER_NAME "Integer"
#define BARD_FLOAT_NAME "Float"
#define BARD_RATIO_NAME "Ratio"
#define BARD_SYMBOL_NAME "Symbol"
#define BARD_KEYWORD_NAME "Keyword"
#define BARD_TEXT_NAME "Text"
#define BARD_LIST_NAME "List"
#define BARD_FRAME_NAME "Frame"
#define BARD_PRIMITIVE_NAME "Primitive"
#define BARD_FUNCTION_NAME "Function"
#define BARD_METHOD_NAME "Method"
#define BARD_TYPE_NAME "Type"
#define BARD_UNRECOGNIZED_NAME "Unrecognized type"

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

// ---------------------------------------------------------------------
// API functions
// ---------------------------------------------------------------------

// errors

extern bool bard_error_status();
extern int bard_last_error_code ();
extern const char* bard_last_error_message ();
extern void clear_last_bard_error ();
extern void clear_all_bard_errors ();

// initialization

extern const char* bard_version ();
extern bool init_bard();

// types and conversions

extern int bard_type(BardValue expr);
extern const char* bard_typename(BardValue expr);
extern bool bard_is_nothing(BardValue obj);
extern bool bard_is_empty(BardValue obj);
extern char as_char(BardValue expr);
extern bool as_bool(BardValue expr);
extern int as_int(BardValue expr);
extern float as_float(BardValue expr);
extern const char* as_string(BardValue expr);

extern BardValue make_integer(int i);
extern BardValue make_character(char ch);
extern BardValue make_float(float f);
extern BardValue make_boolean(bool b);
extern BardValue make_string(const char* str);
extern BardValue make_symbol(const char* str);
extern BardValue make_keyword(const char* str);


// object ids

#define BARD_NO_OBJECT_ID 0
extern int bard_object_to_id(BardValue obj);
extern BardValue bard_id_to_object(int oid);

// reading and loading

extern BardValue bard_read(const char* str);
extern BardValue bard_read_lines(const char* str);
extern BardValue bard_read_nonempty_lines(const char* str);
extern BardValue bard_load_from_string(const char* str);
extern BardValue bard_eval(BardValue obj);

// -------------------
// operations on values
// --------------------

// frames

extern BardValue bard_get(BardValue obj, BardValue key);
extern BardValue bard_put(BardValue obj, BardValue key, BardValue val);

extern BardValue bard_get_char_key(BardValue obj, char key);
extern BardValue bard_get_bool_key(BardValue obj, bool key);
extern BardValue bard_get_int_key(BardValue obj, int key);
extern BardValue bard_get_float_key(BardValue obj, float key);
extern BardValue bard_get_symbol_key(BardValue obj, const char* key);
extern BardValue bard_get_keyword_key(BardValue obj, const char* key);
extern BardValue bard_get_string_key(BardValue obj, const char* key);

extern BardValue bard_get_keyword_symbol_or_string_key(BardValue obj, const char* key);
extern BardValue bard_get_path(BardValue obj, const char* pathname);

extern BardValue bard_keys(BardValue obj);

// lists

extern int bard_length(BardValue obj);
extern BardValue bard_element(BardValue obj,int index);
extern BardValue bard_cons(BardValue obj1,BardValue obj2);

// utils
extern void bard_display(BardValue obj);
extern void bard_newline();






