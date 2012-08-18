#ifdef ___LINKER_INFO
; File: "bard.c", produced by Gambit-C v4.6.6
(
406006
" bard"
(" bard")
(
)
(
"debug"
"q"
"quit"
)
(
" bard"
"*bard-prompt*"
)
(
"bard:repl"
)
(
"$bard-version-string"
"%as-string"
"%defined?"
"%eval"
"%init-bard"
"%null-environment"
"bard:read-from-string"
"display"
"display-error"
"eq?"
"gc-report-set!"
"newline"
"read-line"
"with-exception-catcher"
)
 #f
)
#else
#define ___VERSION 406006
#define ___MODULE_NAME " bard"
#define ___LINKER_ID ____20_bard
#define ___MH_PROC ___H__20_bard
#define ___SCRIPT_LINE 0
#define ___KEY_COUNT 3
#define ___GLO_COUNT 17
#define ___SUP_COUNT 3
#define ___SUB_COUNT 3
#define ___LBL_COUNT 39
#include "gambit.h"

___NEED_KEY(___K_debug)
___NEED_KEY(___K_q)
___NEED_KEY(___K_quit)

___NEED_GLO(___G__20_bard)
___NEED_GLO(___G__24_bard_2d_version_2d_string)
___NEED_GLO(___G__25_as_2d_string)
___NEED_GLO(___G__25_defined_3f_)
___NEED_GLO(___G__25_eval)
___NEED_GLO(___G__25_init_2d_bard)
___NEED_GLO(___G__25_null_2d_environment)
___NEED_GLO(___G__2a_bard_2d_prompt_2a_)
___NEED_GLO(___G_bard_3a_read_2d_from_2d_string)
___NEED_GLO(___G_bard_3a_repl)
___NEED_GLO(___G_display)
___NEED_GLO(___G_display_2d_error)
___NEED_GLO(___G_eq_3f_)
___NEED_GLO(___G_gc_2d_report_2d_set_21_)
___NEED_GLO(___G_newline)
___NEED_GLO(___G_read_2d_line)
___NEED_GLO(___G_with_2d_exception_2d_catcher)

___BEGIN_KEY1
___DEF_KEY1(0,___K_debug,"debug")
___DEF_KEY1(1,___K_q,"q")
___DEF_KEY1(2,___K_quit,"quit")
___END_KEY1

___BEGIN_GLO
___DEF_GLO(0," bard")
___DEF_GLO(1,"*bard-prompt*")
___DEF_GLO(2,"bard:repl")
___DEF_GLO(3,"$bard-version-string")
___DEF_GLO(4,"%as-string")
___DEF_GLO(5,"%defined?")
___DEF_GLO(6,"%eval")
___DEF_GLO(7,"%init-bard")
___DEF_GLO(8,"%null-environment")
___DEF_GLO(9,"bard:read-from-string")
___DEF_GLO(10,"display")
___DEF_GLO(11,"display-error")
___DEF_GLO(12,"eq?")
___DEF_GLO(13,"gc-report-set!")
___DEF_GLO(14,"newline")
___DEF_GLO(15,"read-line")
___DEF_GLO(16,"with-exception-catcher")
___END_GLO

___DEF_SUB_STR(___X0,6)
               ___STR6(98,97,114,100,62,32)
___DEF_SUB_VEC(___X1,2)
               ___VEC1(___REF_KEY(0,___K_debug))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STR(___X2,15)
               ___STR8(66,97,114,100,32,116,101,114)
               ___STR7(109,105,110,97,116,101,100)

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_bard)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_bard_3a_repl)
___DEF_M_HLBL(___L1_bard_3a_repl)
___DEF_M_HLBL(___L2_bard_3a_repl)
___DEF_M_HLBL(___L3_bard_3a_repl)
___DEF_M_HLBL(___L4_bard_3a_repl)
___DEF_M_HLBL(___L5_bard_3a_repl)
___DEF_M_HLBL(___L6_bard_3a_repl)
___DEF_M_HLBL(___L7_bard_3a_repl)
___DEF_M_HLBL(___L8_bard_3a_repl)
___DEF_M_HLBL(___L9_bard_3a_repl)
___DEF_M_HLBL(___L10_bard_3a_repl)
___DEF_M_HLBL(___L11_bard_3a_repl)
___DEF_M_HLBL(___L12_bard_3a_repl)
___DEF_M_HLBL(___L13_bard_3a_repl)
___DEF_M_HLBL(___L14_bard_3a_repl)
___DEF_M_HLBL(___L15_bard_3a_repl)
___DEF_M_HLBL(___L16_bard_3a_repl)
___DEF_M_HLBL(___L17_bard_3a_repl)
___DEF_M_HLBL(___L18_bard_3a_repl)
___DEF_M_HLBL(___L19_bard_3a_repl)
___DEF_M_HLBL(___L20_bard_3a_repl)
___DEF_M_HLBL(___L21_bard_3a_repl)
___DEF_M_HLBL(___L22_bard_3a_repl)
___DEF_M_HLBL(___L23_bard_3a_repl)
___DEF_M_HLBL(___L24_bard_3a_repl)
___DEF_M_HLBL(___L25_bard_3a_repl)
___DEF_M_HLBL(___L26_bard_3a_repl)
___DEF_M_HLBL(___L27_bard_3a_repl)
___DEF_M_HLBL(___L28_bard_3a_repl)
___DEF_M_HLBL(___L29_bard_3a_repl)
___DEF_M_HLBL(___L30_bard_3a_repl)
___DEF_M_HLBL(___L31_bard_3a_repl)
___DEF_M_HLBL(___L32_bard_3a_repl)
___DEF_M_HLBL(___L33_bard_3a_repl)
___DEF_M_HLBL(___L34_bard_3a_repl)
___DEF_M_HLBL(___L35_bard_3a_repl)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_bard
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_bard)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_bard)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_bard)
   ___SET_GLO(1,___G__2a_bard_2d_prompt_2a_,___SUB(0))
   ___SET_GLO(2,___G_bard_3a_repl,___PRC(3))
   ___SET_R1(___VOID)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_bard_3a_repl
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_bard_3a_repl)
___DEF_P_HLBL(___L1_bard_3a_repl)
___DEF_P_HLBL(___L2_bard_3a_repl)
___DEF_P_HLBL(___L3_bard_3a_repl)
___DEF_P_HLBL(___L4_bard_3a_repl)
___DEF_P_HLBL(___L5_bard_3a_repl)
___DEF_P_HLBL(___L6_bard_3a_repl)
___DEF_P_HLBL(___L7_bard_3a_repl)
___DEF_P_HLBL(___L8_bard_3a_repl)
___DEF_P_HLBL(___L9_bard_3a_repl)
___DEF_P_HLBL(___L10_bard_3a_repl)
___DEF_P_HLBL(___L11_bard_3a_repl)
___DEF_P_HLBL(___L12_bard_3a_repl)
___DEF_P_HLBL(___L13_bard_3a_repl)
___DEF_P_HLBL(___L14_bard_3a_repl)
___DEF_P_HLBL(___L15_bard_3a_repl)
___DEF_P_HLBL(___L16_bard_3a_repl)
___DEF_P_HLBL(___L17_bard_3a_repl)
___DEF_P_HLBL(___L18_bard_3a_repl)
___DEF_P_HLBL(___L19_bard_3a_repl)
___DEF_P_HLBL(___L20_bard_3a_repl)
___DEF_P_HLBL(___L21_bard_3a_repl)
___DEF_P_HLBL(___L22_bard_3a_repl)
___DEF_P_HLBL(___L23_bard_3a_repl)
___DEF_P_HLBL(___L24_bard_3a_repl)
___DEF_P_HLBL(___L25_bard_3a_repl)
___DEF_P_HLBL(___L26_bard_3a_repl)
___DEF_P_HLBL(___L27_bard_3a_repl)
___DEF_P_HLBL(___L28_bard_3a_repl)
___DEF_P_HLBL(___L29_bard_3a_repl)
___DEF_P_HLBL(___L30_bard_3a_repl)
___DEF_P_HLBL(___L31_bard_3a_repl)
___DEF_P_HLBL(___L32_bard_3a_repl)
___DEF_P_HLBL(___L33_bard_3a_repl)
___DEF_P_HLBL(___L34_bard_3a_repl)
___DEF_P_HLBL(___L35_bard_3a_repl)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_bard_3a_repl)
   ___IF_NARGS_EQ(0,___SET_R1(___FAL))
   ___GET_KEY(0,0,0,1,___SUB(1))
___DEF_GLBL(___L_bard_3a_repl)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R1(___FAL)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1_bard_3a_repl)
   ___JUMPGLOSAFE(___SET_NARGS(1),13,___G_gc_2d_report_2d_set_21_)
___DEF_SLBL(2,___L2_bard_3a_repl)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(0),14,___G_newline)
___DEF_SLBL(3,___L3_bard_3a_repl)
   ___SET_R1(___GLO(3,___G__24_bard_2d_version_2d_string))
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),10,___G_display)
___DEF_SLBL(4,___L4_bard_3a_repl)
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(0),14,___G_newline)
___DEF_SLBL(5,___L5_bard_3a_repl)
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(0),7,___G__25_init_2d_bard)
___DEF_SLBL(6,___L6_bard_3a_repl)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(7)
___DEF_SLBL(7,___L7_bard_3a_repl)
   ___GOTO(___L37_bard_3a_repl)
___DEF_SLBL(8,___L8_bard_3a_repl)
___DEF_GLBL(___L36_bard_3a_repl)
   ___SET_R1(___CLO(___STK(-6),1))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(9)
___DEF_SLBL(9,___L9_bard_3a_repl)
___DEF_GLBL(___L37_bard_3a_repl)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(11))
   ___ADJFP(8)
   ___POLL(10)
___DEF_SLBL(10,___L10_bard_3a_repl)
   ___JUMPGLOSAFE(___SET_NARGS(0),14,___G_newline)
___DEF_SLBL(11,___L11_bard_3a_repl)
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(0),14,___G_newline)
___DEF_SLBL(12,___L12_bard_3a_repl)
   ___SET_R1(___GLO(1,___G__2a_bard_2d_prompt_2a_))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),10,___G_display)
___DEF_SLBL(13,___L13_bard_3a_repl)
   ___SET_STK(-5,___ALLOC_CLO(1))
   ___BEGIN_SETUP_CLO(1,___STK(-5),22)
   ___ADD_CLO_ELEM(0,___STK(-6))
   ___END_SETUP_CLO(1)
   ___ADJFP(-5)
   ___CHECK_HEAP(14,4096)
___DEF_SLBL(14,___L14_bard_3a_repl)
   ___IF(___NOT(___FALSEP(___STK(-1))))
   ___GOTO(___L38_bard_3a_repl)
   ___END_IF
   ___SET_STK(1,___ALLOC_CLO(1))
   ___BEGIN_SETUP_CLO(1,___STK(1),17)
   ___ADD_CLO_ELEM(0,___STK(-1))
   ___END_SETUP_CLO(1)
   ___SET_R2(___STK(0))
   ___SET_R1(___STK(1))
   ___SET_R0(___STK(-2))
   ___ADJFP(1)
   ___CHECK_HEAP(15,4096)
___DEF_SLBL(15,___L15_bard_3a_repl)
   ___POLL(16)
___DEF_SLBL(16,___L16_bard_3a_repl)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),16,___G_with_2d_exception_2d_catcher)
___DEF_SLBL(17,___L17_bard_3a_repl)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(17,1,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R4)
   ___SET_R0(___LBL(19))
   ___ADJFP(8)
   ___POLL(18)
___DEF_SLBL(18,___L18_bard_3a_repl)
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G_display_2d_error)
___DEF_SLBL(19,___L19_bard_3a_repl)
   ___SET_R1(___CLO(___STK(-6),1))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(20)
___DEF_SLBL(20,___L20_bard_3a_repl)
   ___GOTO(___L37_bard_3a_repl)
___DEF_GLBL(___L38_bard_3a_repl)
   ___SET_R0(___STK(-2))
   ___POLL(21)
___DEF_SLBL(21,___L21_bard_3a_repl)
   ___ADJFP(-3)
   ___JUMPGENSAFE(___SET_NARGS(0),___STK(3))
___DEF_SLBL(22,___L22_bard_3a_repl)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(22,0,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R4)
   ___SET_R0(___LBL(24))
   ___ADJFP(8)
   ___POLL(23)
___DEF_SLBL(23,___L23_bard_3a_repl)
   ___JUMPGLOSAFE(___SET_NARGS(0),15,___G_read_2d_line)
___DEF_SLBL(24,___L24_bard_3a_repl)
   ___SET_R0(___LBL(25))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G_bard_3a_read_2d_from_2d_string)
___DEF_SLBL(25,___L25_bard_3a_repl)
   ___IF(___NOT(___EQP(___GLO(12,___G_eq_3f_),___PRM(12,___G_eq_3f_))))
   ___GOTO(___L45_bard_3a_repl)
   ___END_IF
   ___IF(___EQP(___R1,___KEY(2,___K_quit)))
   ___GOTO(___L43_bard_3a_repl)
   ___END_IF
   ___IF(___EQP(___GLO(12,___G_eq_3f_),___PRM(12,___G_eq_3f_)))
   ___GOTO(___L39_bard_3a_repl)
   ___END_IF
   ___GOTO(___L44_bard_3a_repl)
___DEF_SLBL(26,___L26_bard_3a_repl)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L43_bard_3a_repl)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___IF(___NOT(___EQP(___GLO(12,___G_eq_3f_),___PRM(12,___G_eq_3f_))))
   ___GOTO(___L44_bard_3a_repl)
   ___END_IF
___DEF_GLBL(___L39_bard_3a_repl)
   ___IF(___EQP(___R1,___KEY(1,___K_q)))
   ___GOTO(___L43_bard_3a_repl)
   ___END_IF
   ___IF(___NOT(___EQP(___GLO(12,___G_eq_3f_),___PRM(12,___G_eq_3f_))))
   ___GOTO(___L42_bard_3a_repl)
   ___END_IF
___DEF_GLBL(___L40_bard_3a_repl)
   ___IF(___EQP(___R1,___EOF))
   ___GOTO(___L36_bard_3a_repl)
   ___END_IF
___DEF_GLBL(___L41_bard_3a_repl)
   ___SET_STK(-5,___R1)
   ___SET_R0(___LBL(27))
   ___JUMPGLOSAFE(___SET_NARGS(0),8,___G__25_null_2d_environment)
___DEF_SLBL(27,___L27_bard_3a_repl)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(28))
   ___JUMPGLOSAFE(___SET_NARGS(2),6,___G__25_eval)
___DEF_SLBL(28,___L28_bard_3a_repl)
   ___SET_STK(-5,___R1)
   ___SET_R0(___LBL(29))
   ___JUMPGLOSAFE(___SET_NARGS(1),5,___G__25_defined_3f_)
___DEF_SLBL(29,___L29_bard_3a_repl)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L36_bard_3a_repl)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(30))
   ___JUMPGLOSAFE(___SET_NARGS(1),4,___G__25_as_2d_string)
___DEF_SLBL(30,___L30_bard_3a_repl)
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(1),10,___G_display)
___DEF_SLBL(31,___L31_bard_3a_repl)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L43_bard_3a_repl)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___IF(___EQP(___GLO(12,___G_eq_3f_),___PRM(12,___G_eq_3f_)))
   ___GOTO(___L40_bard_3a_repl)
   ___END_IF
___DEF_GLBL(___L42_bard_3a_repl)
   ___SET_STK(-5,___R1)
   ___SET_R2(___EOF)
   ___SET_R0(___LBL(32))
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G_eq_3f_)
___DEF_SLBL(32,___L32_bard_3a_repl)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L36_bard_3a_repl)
   ___END_IF
   ___SET_R1(___STK(-5))
   ___GOTO(___L41_bard_3a_repl)
___DEF_GLBL(___L43_bard_3a_repl)
   ___SET_R0(___LBL(33))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(0),14,___G_newline)
___DEF_SLBL(33,___L33_bard_3a_repl)
   ___SET_R1(___SUB(2))
   ___SET_R0(___LBL(34))
   ___JUMPGLOSAFE(___SET_NARGS(1),10,___G_display)
___DEF_SLBL(34,___L34_bard_3a_repl)
   ___SET_R0(___STK(-3))
   ___POLL(35)
___DEF_SLBL(35,___L35_bard_3a_repl)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(0),14,___G_newline)
___DEF_GLBL(___L44_bard_3a_repl)
   ___SET_STK(-5,___R1)
   ___SET_R2(___KEY(1,___K_q))
   ___SET_R0(___LBL(31))
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G_eq_3f_)
___DEF_GLBL(___L45_bard_3a_repl)
   ___SET_STK(-5,___R1)
   ___SET_R2(___KEY(2,___K_quit))
   ___SET_R0(___LBL(26))
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G_eq_3f_)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_bard," bard",___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__20_bard,0,0)
,___DEF_LBL_INTRO(___H_bard_3a_repl,0,___REF_FAL,36,0)
,___DEF_LBL_PROC(___H_bard_3a_repl,1,0)
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,3,0,0x3f7L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_PROC(___H_bard_3a_repl,1,1)
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,3,4,0x3f4L))
,___DEF_LBL_PROC(___H_bard_3a_repl,0,1)
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_bard_3a_repl,___IFD(___RETI,4,4,0x3f0L))
___END_LBL

___BEGIN_MOD1
___DEF_PRM(0,___G__20_bard,1)
___END_MOD1

___BEGIN_MOD2
___DEF_KEY2(0,___K_debug,"debug")
___DEF_KEY2(1,___K_q,"q")
___DEF_KEY2(2,___K_quit,"quit")
___END_MOD2

#endif
