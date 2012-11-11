#ifdef ___LINKER_INFO
; File: "macro.c", produced by Gambit-C v4.6.6
(
406006
" macro"
(" macro")
(
"and"
"if"
"let"
"or"
)
(
"test"
)
(
" macro"
"$bard-macro-functions"
"%define-macro-function"
"%macroexpand"
)
(
"%eval-macro-form"
"%macro-form?"
)
(
"%cadr"
"%car"
"%cddr"
"%cdr"
"%eval"
"%false"
"%funcall"
"%list?"
"%null?"
"%true"
"eq?"
"error"
"gensym"
"make-table"
"not"
"table-ref"
"table-set!"
)
 #f
)
#else
#define ___VERSION 406006
#define ___MODULE_NAME " macro"
#define ___LINKER_ID ____20_macro
#define ___MH_PROC ___H__20_macro
#define ___SCRIPT_LINE 0
#define ___SYM_COUNT 4
#define ___KEY_COUNT 1
#define ___GLO_COUNT 23
#define ___SUP_COUNT 6
#define ___SUB_COUNT 1
#define ___LBL_COUNT 65
#include "gambit.h"

___NEED_SYM(___S_and)
___NEED_SYM(___S_if)
___NEED_SYM(___S_let)
___NEED_SYM(___S_or)

___NEED_KEY(___K_test)

___NEED_GLO(___G__20_macro)
___NEED_GLO(___G__24_bard_2d_macro_2d_functions)
___NEED_GLO(___G__25_cadr)
___NEED_GLO(___G__25_car)
___NEED_GLO(___G__25_cddr)
___NEED_GLO(___G__25_cdr)
___NEED_GLO(___G__25_define_2d_macro_2d_function)
___NEED_GLO(___G__25_eval)
___NEED_GLO(___G__25_eval_2d_macro_2d_form)
___NEED_GLO(___G__25_false)
___NEED_GLO(___G__25_funcall)
___NEED_GLO(___G__25_list_3f_)
___NEED_GLO(___G__25_macro_2d_form_3f_)
___NEED_GLO(___G__25_macroexpand)
___NEED_GLO(___G__25_null_3f_)
___NEED_GLO(___G__25_true)
___NEED_GLO(___G_eq_3f_)
___NEED_GLO(___G_error)
___NEED_GLO(___G_gensym)
___NEED_GLO(___G_make_2d_table)
___NEED_GLO(___G_not)
___NEED_GLO(___G_table_2d_ref)
___NEED_GLO(___G_table_2d_set_21_)

___BEGIN_SYM1
___DEF_SYM1(0,___S_and,"and")
___DEF_SYM1(1,___S_if,"if")
___DEF_SYM1(2,___S_let,"let")
___DEF_SYM1(3,___S_or,"or")
___END_SYM1

___BEGIN_KEY1
___DEF_KEY1(0,___K_test,"test")
___END_KEY1

___BEGIN_GLO
___DEF_GLO(0," macro")
___DEF_GLO(1,"$bard-macro-functions")
___DEF_GLO(2,"%define-macro-function")
___DEF_GLO(3,"%eval-macro-form")
___DEF_GLO(4,"%macro-form?")
___DEF_GLO(5,"%macroexpand")
___DEF_GLO(6,"%cadr")
___DEF_GLO(7,"%car")
___DEF_GLO(8,"%cddr")
___DEF_GLO(9,"%cdr")
___DEF_GLO(10,"%eval")
___DEF_GLO(11,"%false")
___DEF_GLO(12,"%funcall")
___DEF_GLO(13,"%list?")
___DEF_GLO(14,"%null?")
___DEF_GLO(15,"%true")
___DEF_GLO(16,"eq?")
___DEF_GLO(17,"error")
___DEF_GLO(18,"gensym")
___DEF_GLO(19,"make-table")
___DEF_GLO(20,"not")
___DEF_GLO(21,"table-ref")
___DEF_GLO(22,"table-set!")
___END_GLO

___DEF_SUB_STR(___X0,29)
               ___STR8(117,110,100,101,102,105,110,101)
               ___STR8(100,32,109,97,99,114,111,32)
               ___STR8(105,110,32,101,120,112,114,101)
               ___STR5(115,115,105,111,110)

___BEGIN_SUB
 ___DEF_SUB(___X0)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_macro)
___DEF_M_HLBL(___L1__20_macro)
___DEF_M_HLBL(___L2__20_macro)
___DEF_M_HLBL(___L3__20_macro)
___DEF_M_HLBL(___L4__20_macro)
___DEF_M_HLBL(___L5__20_macro)
___DEF_M_HLBL(___L6__20_macro)
___DEF_M_HLBL(___L7__20_macro)
___DEF_M_HLBL(___L8__20_macro)
___DEF_M_HLBL(___L9__20_macro)
___DEF_M_HLBL(___L10__20_macro)
___DEF_M_HLBL(___L11__20_macro)
___DEF_M_HLBL(___L12__20_macro)
___DEF_M_HLBL(___L13__20_macro)
___DEF_M_HLBL(___L14__20_macro)
___DEF_M_HLBL(___L15__20_macro)
___DEF_M_HLBL(___L16__20_macro)
___DEF_M_HLBL(___L17__20_macro)
___DEF_M_HLBL(___L18__20_macro)
___DEF_M_HLBL(___L19__20_macro)
___DEF_M_HLBL(___L20__20_macro)
___DEF_M_HLBL(___L21__20_macro)
___DEF_M_HLBL(___L22__20_macro)
___DEF_M_HLBL(___L23__20_macro)
___DEF_M_HLBL(___L24__20_macro)
___DEF_M_HLBL(___L25__20_macro)
___DEF_M_HLBL(___L26__20_macro)
___DEF_M_HLBL(___L27__20_macro)
___DEF_M_HLBL(___L28__20_macro)
___DEF_M_HLBL(___L29__20_macro)
___DEF_M_HLBL(___L30__20_macro)
___DEF_M_HLBL(___L31__20_macro)
___DEF_M_HLBL(___L32__20_macro)
___DEF_M_HLBL(___L33__20_macro)
___DEF_M_HLBL(___L34__20_macro)
___DEF_M_HLBL(___L35__20_macro)
___DEF_M_HLBL(___L36__20_macro)
___DEF_M_HLBL(___L37__20_macro)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_define_2d_macro_2d_function)
___DEF_M_HLBL(___L1__25_define_2d_macro_2d_function)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L1__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L2__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L3__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L4__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L5__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L6__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L7__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L8__25_macro_2d_form_3f_)
___DEF_M_HLBL(___L9__25_macro_2d_form_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_macroexpand)
___DEF_M_HLBL(___L1__25_macroexpand)
___DEF_M_HLBL(___L2__25_macroexpand)
___DEF_M_HLBL(___L3__25_macroexpand)
___DEF_M_HLBL(___L4__25_macroexpand)
___DEF_M_HLBL(___L5__25_macroexpand)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_eval_2d_macro_2d_form)
___DEF_M_HLBL(___L1__25_eval_2d_macro_2d_form)
___DEF_M_HLBL(___L2__25_eval_2d_macro_2d_form)
___DEF_M_HLBL(___L3__25_eval_2d_macro_2d_form)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_macro
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_macro)
___DEF_P_HLBL(___L1__20_macro)
___DEF_P_HLBL(___L2__20_macro)
___DEF_P_HLBL(___L3__20_macro)
___DEF_P_HLBL(___L4__20_macro)
___DEF_P_HLBL(___L5__20_macro)
___DEF_P_HLBL(___L6__20_macro)
___DEF_P_HLBL(___L7__20_macro)
___DEF_P_HLBL(___L8__20_macro)
___DEF_P_HLBL(___L9__20_macro)
___DEF_P_HLBL(___L10__20_macro)
___DEF_P_HLBL(___L11__20_macro)
___DEF_P_HLBL(___L12__20_macro)
___DEF_P_HLBL(___L13__20_macro)
___DEF_P_HLBL(___L14__20_macro)
___DEF_P_HLBL(___L15__20_macro)
___DEF_P_HLBL(___L16__20_macro)
___DEF_P_HLBL(___L17__20_macro)
___DEF_P_HLBL(___L18__20_macro)
___DEF_P_HLBL(___L19__20_macro)
___DEF_P_HLBL(___L20__20_macro)
___DEF_P_HLBL(___L21__20_macro)
___DEF_P_HLBL(___L22__20_macro)
___DEF_P_HLBL(___L23__20_macro)
___DEF_P_HLBL(___L24__20_macro)
___DEF_P_HLBL(___L25__20_macro)
___DEF_P_HLBL(___L26__20_macro)
___DEF_P_HLBL(___L27__20_macro)
___DEF_P_HLBL(___L28__20_macro)
___DEF_P_HLBL(___L29__20_macro)
___DEF_P_HLBL(___L30__20_macro)
___DEF_P_HLBL(___L31__20_macro)
___DEF_P_HLBL(___L32__20_macro)
___DEF_P_HLBL(___L33__20_macro)
___DEF_P_HLBL(___L34__20_macro)
___DEF_P_HLBL(___L35__20_macro)
___DEF_P_HLBL(___L36__20_macro)
___DEF_P_HLBL(___L37__20_macro)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_macro)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_macro)
   ___SET_STK(1,___R0)
   ___SET_R2(___GLO(16,___G_eq_3f_))
   ___SET_R1(___KEY(0,___K_test))
   ___SET_R0(___LBL(2))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1__20_macro)
   ___JUMPGLOSAFE(___SET_NARGS(2),19,___G_make_2d_table)
___DEF_SLBL(2,___L2__20_macro)
   ___SET_GLO(1,___G__24_bard_2d_macro_2d_functions,___R1)
   ___SET_GLO(2,___G__25_define_2d_macro_2d_function,___PRC(40))
   ___SET_GLO(4,___G__25_macro_2d_form_3f_,___PRC(43))
   ___SET_GLO(5,___G__25_macroexpand,___PRC(54))
   ___SET_GLO(3,___G__25_eval_2d_macro_2d_form,___PRC(61))
   ___SET_R2(___LBL(7))
   ___SET_R1(___SYM(0,___S_and))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(2),2,___G__25_define_2d_macro_2d_function)
___DEF_SLBL(3,___L3__20_macro)
   ___SET_R2(___LBL(5))
   ___SET_R1(___SYM(3,___S_or))
   ___SET_R0(___STK(-3))
   ___POLL(4)
___DEF_SLBL(4,___L4__20_macro)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),2,___G__25_define_2d_macro_2d_function)
___DEF_SLBL(5,___L5__20_macro)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(5,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(33))
   ___ADJFP(8)
   ___POLL(6)
___DEF_SLBL(6,___L6__20_macro)
   ___GOTO(___L38__20_macro)
___DEF_SLBL(7,___L7__20_macro)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(7,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(9))
   ___ADJFP(8)
   ___POLL(8)
___DEF_SLBL(8,___L8__20_macro)
___DEF_GLBL(___L38__20_macro)
   ___JUMPGLOSAFE(___SET_NARGS(0),18,___G_gensym)
___DEF_SLBL(9,___L9__20_macro)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_cdr)
___DEF_SLBL(10,___L10__20_macro)
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G__25_null_3f_)
___DEF_SLBL(11,___L11__20_macro)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L39__20_macro)
   ___END_IF
   ___SET_R0(___STK(-7))
   ___POLL(12)
___DEF_SLBL(12,___L12__20_macro)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(0),15,___G__25_true)
___DEF_GLBL(___L39__20_macro)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_cddr)
___DEF_SLBL(13,___L13__20_macro)
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G__25_null_3f_)
___DEF_SLBL(14,___L14__20_macro)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L41__20_macro)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G__25_cadr)
___DEF_SLBL(15,___L15__20_macro)
   ___BEGIN_ALLOC_LIST(2,___R1)
   ___ADD_LIST_ELEM(1,___STK(-5))
   ___END_ALLOC_LIST(2)
   ___SET_R1(___GET_LIST(2))
   ___SET_R1(___CONS(___R1,___NUL))
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(17))
   ___CHECK_HEAP(16,4096)
___DEF_SLBL(16,___L16__20_macro)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_cddr)
___DEF_SLBL(17,___L17__20_macro)
   ___SET_R1(___CONS(___SYM(0,___S_and),___R1))
   ___SET_STK(-6,___R1)
   ___SET_R0(___LBL(19))
   ___CHECK_HEAP(18,4096)
___DEF_SLBL(18,___L18__20_macro)
   ___JUMPGLOSAFE(___SET_NARGS(0),11,___G__25_false)
___DEF_SLBL(19,___L19__20_macro)
   ___BEGIN_ALLOC_LIST(4,___R1)
   ___ADD_LIST_ELEM(1,___STK(-6))
   ___ADD_LIST_ELEM(2,___STK(-5))
   ___ADD_LIST_ELEM(3,___SYM(1,___S_if))
   ___END_ALLOC_LIST(4)
   ___SET_R1(___GET_LIST(4))
   ___BEGIN_ALLOC_LIST(3,___R1)
   ___ADD_LIST_ELEM(1,___STK(-4))
   ___ADD_LIST_ELEM(2,___SYM(2,___S_let))
   ___END_ALLOC_LIST(3)
   ___SET_R1(___GET_LIST(3))
   ___CHECK_HEAP(20,4096)
___DEF_SLBL(20,___L20__20_macro)
   ___POLL(21)
___DEF_SLBL(21,___L21__20_macro)
   ___GOTO(___L40__20_macro)
___DEF_SLBL(22,___L22__20_macro)
   ___SET_R1(___CONS(___SYM(3,___S_or),___R1))
   ___BEGIN_ALLOC_LIST(4,___R1)
   ___ADD_LIST_ELEM(1,___STK(-5))
   ___ADD_LIST_ELEM(2,___STK(-5))
   ___ADD_LIST_ELEM(3,___SYM(1,___S_if))
   ___END_ALLOC_LIST(4)
   ___SET_R1(___GET_LIST(4))
   ___BEGIN_ALLOC_LIST(3,___R1)
   ___ADD_LIST_ELEM(1,___STK(-4))
   ___ADD_LIST_ELEM(2,___SYM(2,___S_let))
   ___END_ALLOC_LIST(3)
   ___SET_R1(___GET_LIST(3))
   ___CHECK_HEAP(23,4096)
___DEF_SLBL(23,___L23__20_macro)
   ___POLL(24)
___DEF_SLBL(24,___L24__20_macro)
___DEF_GLBL(___L40__20_macro)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_SLBL(25,___L25__20_macro)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L42__20_macro)
   ___END_IF
___DEF_GLBL(___L41__20_macro)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(26))
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G__25_cadr)
___DEF_SLBL(26,___L26__20_macro)
   ___BEGIN_ALLOC_LIST(2,___R1)
   ___ADD_LIST_ELEM(1,___STK(-5))
   ___END_ALLOC_LIST(2)
   ___SET_R1(___GET_LIST(2))
   ___SET_R1(___CONS(___R1,___NUL))
   ___SET_STK(-6,___R1)
   ___SET_R0(___LBL(28))
   ___CHECK_HEAP(27,4096)
___DEF_SLBL(27,___L27__20_macro)
   ___JUMPGLOSAFE(___SET_NARGS(0),11,___G__25_false)
___DEF_SLBL(28,___L28__20_macro)
   ___BEGIN_ALLOC_LIST(4,___R1)
   ___ADD_LIST_ELEM(1,___STK(-5))
   ___ADD_LIST_ELEM(2,___STK(-5))
   ___ADD_LIST_ELEM(3,___SYM(1,___S_if))
   ___END_ALLOC_LIST(4)
   ___SET_R1(___GET_LIST(4))
   ___BEGIN_ALLOC_LIST(3,___R1)
   ___ADD_LIST_ELEM(1,___STK(-6))
   ___ADD_LIST_ELEM(2,___SYM(2,___S_let))
   ___END_ALLOC_LIST(3)
   ___SET_R1(___GET_LIST(3))
   ___CHECK_HEAP(29,4096)
___DEF_SLBL(29,___L29__20_macro)
   ___POLL(30)
___DEF_SLBL(30,___L30__20_macro)
   ___GOTO(___L40__20_macro)
___DEF_GLBL(___L42__20_macro)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(31))
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G__25_cadr)
___DEF_SLBL(31,___L31__20_macro)
   ___BEGIN_ALLOC_LIST(2,___R1)
   ___ADD_LIST_ELEM(1,___STK(-5))
   ___END_ALLOC_LIST(2)
   ___SET_R1(___GET_LIST(2))
   ___SET_R1(___CONS(___R1,___NUL))
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(22))
   ___CHECK_HEAP(32,4096)
___DEF_SLBL(32,___L32__20_macro)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_cddr)
___DEF_SLBL(33,___L33__20_macro)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(34))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_cdr)
___DEF_SLBL(34,___L34__20_macro)
   ___SET_R0(___LBL(35))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G__25_null_3f_)
___DEF_SLBL(35,___L35__20_macro)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L43__20_macro)
   ___END_IF
   ___SET_R0(___STK(-7))
   ___POLL(36)
___DEF_SLBL(36,___L36__20_macro)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(0),11,___G__25_false)
___DEF_GLBL(___L43__20_macro)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(37))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_cddr)
___DEF_SLBL(37,___L37__20_macro)
   ___SET_R0(___LBL(25))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G__25_null_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_define_2d_macro_2d_function
#undef ___PH_LBL0
#define ___PH_LBL0 40
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_define_2d_macro_2d_function)
___DEF_P_HLBL(___L1__25_define_2d_macro_2d_function)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_define_2d_macro_2d_function)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_define_2d_macro_2d_function)
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___GLO(1,___G__24_bard_2d_macro_2d_functions))
   ___POLL(1)
___DEF_SLBL(1,___L1__25_define_2d_macro_2d_function)
   ___JUMPGLOSAFE(___SET_NARGS(3),22,___G_table_2d_set_21_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_macro_2d_form_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 43
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L1__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L2__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L3__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L4__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L5__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L6__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L7__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L8__25_macro_2d_form_3f_)
___DEF_P_HLBL(___L9__25_macro_2d_form_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_macro_2d_form_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_macro_2d_form_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_macro_2d_form_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G__25_list_3f_)
___DEF_SLBL(2,___L2__25_macro_2d_form_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L10__25_macro_2d_form_3f_)
   ___END_IF
   ___GOTO(___L15__25_macro_2d_form_3f_)
___DEF_SLBL(3,___L3__25_macro_2d_form_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L11__25_macro_2d_form_3f_)
   ___END_IF
___DEF_GLBL(___L10__25_macro_2d_form_3f_)
   ___POLL(4)
___DEF_SLBL(4,___L4__25_macro_2d_form_3f_)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_SLBL(5,___L5__25_macro_2d_form_3f_)
   ___IF(___NOT(___EQP(___GLO(20,___G_not),___PRM(20,___G_not))))
   ___GOTO(___L14__25_macro_2d_form_3f_)
   ___END_IF
   ___SET_R1(___BOOLEAN(___FALSEP(___R1)))
   ___IF(___FALSEP(___R1))
   ___GOTO(___L10__25_macro_2d_form_3f_)
   ___END_IF
___DEF_GLBL(___L11__25_macro_2d_form_3f_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(6))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_car)
___DEF_SLBL(6,___L6__25_macro_2d_form_3f_)
   ___SET_R2(___R1)
   ___SET_R3(___FAL)
   ___SET_R1(___GLO(1,___G__24_bard_2d_macro_2d_functions))
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(3),21,___G_table_2d_ref)
___DEF_SLBL(7,___L7__25_macro_2d_form_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L12__25_macro_2d_form_3f_)
   ___END_IF
   ___POLL(8)
___DEF_SLBL(8,___L8__25_macro_2d_form_3f_)
   ___GOTO(___L13__25_macro_2d_form_3f_)
___DEF_GLBL(___L12__25_macro_2d_form_3f_)
   ___SET_R1(___TRU)
   ___POLL(9)
___DEF_SLBL(9,___L9__25_macro_2d_form_3f_)
___DEF_GLBL(___L13__25_macro_2d_form_3f_)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L14__25_macro_2d_form_3f_)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(1),20,___G_not)
___DEF_GLBL(___L15__25_macro_2d_form_3f_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G__25_null_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_macroexpand
#undef ___PH_LBL0
#define ___PH_LBL0 54
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_macroexpand)
___DEF_P_HLBL(___L1__25_macroexpand)
___DEF_P_HLBL(___L2__25_macroexpand)
___DEF_P_HLBL(___L3__25_macroexpand)
___DEF_P_HLBL(___L4__25_macroexpand)
___DEF_P_HLBL(___L5__25_macroexpand)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_macroexpand)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_macroexpand)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_macroexpand)
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_car)
___DEF_SLBL(2,___L2__25_macroexpand)
   ___SET_R2(___R1)
   ___SET_R3(___FAL)
   ___SET_R1(___GLO(1,___G__24_bard_2d_macro_2d_functions))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(3),21,___G_table_2d_ref)
___DEF_SLBL(3,___L3__25_macroexpand)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L6__25_macroexpand)
   ___END_IF
   ___SET_R2(___STK(-6))
   ___SET_R1(___SUB(0))
   ___SET_R0(___STK(-7))
   ___POLL(4)
___DEF_SLBL(4,___L4__25_macroexpand)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),17,___G_error)
___DEF_GLBL(___L6__25_macroexpand)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(5)
___DEF_SLBL(5,___L5__25_macroexpand)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G__25_funcall)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_eval_2d_macro_2d_form
#undef ___PH_LBL0
#define ___PH_LBL0 61
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_eval_2d_macro_2d_form)
___DEF_P_HLBL(___L1__25_eval_2d_macro_2d_form)
___DEF_P_HLBL(___L2__25_eval_2d_macro_2d_form)
___DEF_P_HLBL(___L3__25_eval_2d_macro_2d_form)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_eval_2d_macro_2d_form)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_eval_2d_macro_2d_form)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_eval_2d_macro_2d_form)
   ___JUMPGLOSAFE(___SET_NARGS(1),5,___G__25_macroexpand)
___DEF_SLBL(2,___L2__25_eval_2d_macro_2d_form)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(3)
___DEF_SLBL(3,___L3__25_eval_2d_macro_2d_form)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),10,___G__25_eval)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_macro," macro",___REF_FAL,38,0)
,___DEF_LBL_PROC(___H__20_macro,0,0)
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_PROC(___H__20_macro,1,0)
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_PROC(___H__20_macro,1,0)
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f0dL))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,0,0x3f0dL))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__20_macro,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_INTRO(___H__25_define_2d_macro_2d_function,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_define_2d_macro_2d_function,2,0)
,___DEF_LBL_RET(___H__25_define_2d_macro_2d_function,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H__25_macro_2d_form_3f_,0,___REF_FAL,10,0)
,___DEF_LBL_PROC(___H__25_macro_2d_form_3f_,1,0)
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__25_macro_2d_form_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_INTRO(___H__25_macroexpand,0,___REF_FAL,6,0)
,___DEF_LBL_PROC(___H__25_macroexpand,1,0)
,___DEF_LBL_RET(___H__25_macroexpand,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_macroexpand,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_macroexpand,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_macroexpand,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_macroexpand,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H__25_eval_2d_macro_2d_form,0,___REF_FAL,4,0)
,___DEF_LBL_PROC(___H__25_eval_2d_macro_2d_form,2,0)
,___DEF_LBL_RET(___H__25_eval_2d_macro_2d_form,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_eval_2d_macro_2d_form,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_eval_2d_macro_2d_form,___IFD(___RETI,8,8,0x3f00L))
___END_LBL

___BEGIN_MOD1
___DEF_PRM(0,___G__20_macro,1)
___END_MOD1

___BEGIN_MOD2
___DEF_SYM2(0,___S_and,"and")
___DEF_SYM2(1,___S_if,"if")
___DEF_SYM2(2,___S_let,"let")
___DEF_SYM2(3,___S_or,"or")
___DEF_KEY2(0,___K_test,"test")
___END_MOD2

#endif
