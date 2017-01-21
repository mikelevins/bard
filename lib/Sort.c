#ifdef ___LINKER_INFO
; File: "Sort.c", produced by Gambit v4.8.5
(
408005
(C)
"Sort"
(("Sort"))
(
"Sort"
)
(
)
(
" Sort"
)
(
"sort"
)
(
"error"
"list->vector"
"vector->list"
)
 ()
)
#else
#define ___VERSION 408005
#define ___MODULE_NAME "Sort"
#define ___LINKER_ID ____20_Sort
#define ___MH_PROC ___H__20_Sort
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 1
#define ___GLOCOUNT 5
#define ___SUPCOUNT 2
#define ___SUBCOUNT 3
#define ___LBLCOUNT 30
#define ___OFDCOUNT 3
#define ___MODDESCR ___REF_SUB(2)
#include "gambit.h"

___NEED_SYM(___S_Sort)

___NEED_GLO(___G__20_Sort)
___NEED_GLO(___G_error)
___NEED_GLO(___G_list_2d__3e_vector)
___NEED_GLO(___G_sort)
___NEED_GLO(___G_vector_2d__3e_list)

___BEGIN_SYM
___DEF_SYM(0,___S_Sort,"Sort")
___END_SYM

#define ___SYM_Sort ___SYM(0,___S_Sort)

___BEGIN_GLO
___DEF_GLO(0," Sort")
___DEF_GLO(1,"sort")
___DEF_GLO(2,"error")
___DEF_GLO(3,"list->vector")
___DEF_GLO(4,"vector->list")
___END_GLO

#define ___GLO__20_Sort ___GLO(0,___G__20_Sort)
#define ___PRM__20_Sort ___PRM(0,___G__20_Sort)
#define ___GLO_sort ___GLO(1,___G_sort)
#define ___PRM_sort ___PRM(1,___G_sort)
#define ___GLO_error ___GLO(2,___G_error)
#define ___PRM_error ___PRM(2,___G_error)
#define ___GLO_list_2d__3e_vector ___GLO(3,___G_list_2d__3e_vector)
#define ___PRM_list_2d__3e_vector ___PRM(3,___G_list_2d__3e_vector)
#define ___GLO_vector_2d__3e_list ___GLO(4,___G_vector_2d__3e_list)
#define ___PRM_vector_2d__3e_list ___PRM(4,___G_vector_2d__3e_list)

___DEF_SUB_STR(___X0,23UL)
               ___STR8(118,101,99,116,111,114,32,111)
               ___STR8(114,32,108,105,115,116,32,101)
               ___STR7(120,112,101,99,116,101,100)
___DEF_SUB_STR(___X1,18UL)
               ___STR8(112,114,111,99,101,100,117,114)
               ___STR8(101,32,101,120,112,101,99,116)
               ___STR2(101,100)
___DEF_SUB_VEC(___X2,5UL)
               ___VEC1(___REF_SYM(0,___S_Sort))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FAL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
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
___DEF_M_HLBL(___L0__20_Sort)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_sort)
___DEF_M_HLBL(___L1_sort)
___DEF_M_HLBL(___L2_sort)
___DEF_M_HLBL(___L3_sort)
___DEF_M_HLBL(___L4_sort)
___DEF_M_HLBL(___L5_sort)
___DEF_M_HLBL(___L6_sort)
___DEF_M_HLBL(___L7_sort)
___DEF_M_HLBL(___L8_sort)
___DEF_M_HLBL(___L9_sort)
___DEF_M_HLBL(___L10_sort)
___DEF_M_HLBL(___L11_sort)
___DEF_M_HLBL(___L12_sort)
___DEF_M_HLBL(___L13_sort)
___DEF_M_HLBL(___L14_sort)
___DEF_M_HLBL(___L15_sort)
___DEF_M_HLBL(___L16_sort)
___DEF_M_HLBL(___L17_sort)
___DEF_M_HLBL(___L18_sort)
___DEF_M_HLBL(___L19_sort)
___DEF_M_HLBL(___L20_sort)
___DEF_M_HLBL(___L21_sort)
___DEF_M_HLBL(___L22_sort)
___DEF_M_HLBL(___L23_sort)
___DEF_M_HLBL(___L24_sort)
___DEF_M_HLBL(___L25_sort)
___DEF_M_HLBL(___L26_sort)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_Sort
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
___DEF_P_HLBL(___L0__20_Sort)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_Sort)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_Sort)
   ___SET_GLO(1,___G_sort,___PRC(3))
   ___SET_R1(___VOID)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_sort
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_sort)
___DEF_P_HLBL(___L1_sort)
___DEF_P_HLBL(___L2_sort)
___DEF_P_HLBL(___L3_sort)
___DEF_P_HLBL(___L4_sort)
___DEF_P_HLBL(___L5_sort)
___DEF_P_HLBL(___L6_sort)
___DEF_P_HLBL(___L7_sort)
___DEF_P_HLBL(___L8_sort)
___DEF_P_HLBL(___L9_sort)
___DEF_P_HLBL(___L10_sort)
___DEF_P_HLBL(___L11_sort)
___DEF_P_HLBL(___L12_sort)
___DEF_P_HLBL(___L13_sort)
___DEF_P_HLBL(___L14_sort)
___DEF_P_HLBL(___L15_sort)
___DEF_P_HLBL(___L16_sort)
___DEF_P_HLBL(___L17_sort)
___DEF_P_HLBL(___L18_sort)
___DEF_P_HLBL(___L19_sort)
___DEF_P_HLBL(___L20_sort)
___DEF_P_HLBL(___L21_sort)
___DEF_P_HLBL(___L22_sort)
___DEF_P_HLBL(___L23_sort)
___DEF_P_HLBL(___L24_sort)
___DEF_P_HLBL(___L25_sort)
___DEF_P_HLBL(___L26_sort)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_sort)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L_sort)
   ___IF(___NOT(___PROCEDUREP(___R2)))
   ___GOTO(___L40_sort)
   ___END_IF
   ___IF(___NULLP(___R1))
   ___GOTO(___L27_sort)
   ___END_IF
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L38_sort)
   ___END_IF
___DEF_GLBL(___L27_sort)
   ___POLL(1)
___DEF_SLBL(1,___L1_sort)
___DEF_GLBL(___L28_sort)
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___POLL(2)
___DEF_SLBL(2,___L2_sort)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L34_sort)
   ___END_IF
___DEF_GLBL(___L29_sort)
   ___SET_R3(___CDR(___R2))
   ___IF(___NOT(___PAIRP(___R3)))
   ___GOTO(___L34_sort)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___R2)
   ___SET_R0(___LBL(20))
   ___ADJFP(8)
   ___POLL(3)
___DEF_SLBL(3,___L3_sort)
___DEF_GLBL(___L30_sort)
   ___IF(___NOT(___PAIRP(___R1)))
   ___GOTO(___L32_sort)
   ___END_IF
___DEF_GLBL(___L31_sort)
   ___SET_R2(___CDR(___R1))
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L32_sort)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R1(___CDDR(___R1))
   ___SET_R0(___LBL(5))
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4_sort)
   ___GOTO(___L30_sort)
___DEF_SLBL(5,___L5_sort)
   ___SET_R2(___CAR(___STK(-6)))
   ___SET_R1(___CONS(___R2,___R1))
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6_sort)
   ___POLL(7)
___DEF_SLBL(7,___L7_sort)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_SLBL(8,___L8_sort)
   ___SET_STK(-4,___R1)
   ___SET_R1(___CDR(___STK(-5)))
   ___SET_R0(___LBL(9))
   ___IF(___PAIRP(___R1))
   ___GOTO(___L31_sort)
   ___END_IF
___DEF_GLBL(___L32_sort)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_SLBL(9,___L9_sort)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(10))
   ___IF(___PAIRP(___R2))
   ___GOTO(___L29_sort)
   ___END_IF
   ___GOTO(___L34_sort)
___DEF_SLBL(10,___L10_sort)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
   ___POLL(11)
___DEF_SLBL(11,___L11_sort)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L35_sort)
   ___END_IF
___DEF_GLBL(___L33_sort)
   ___IF(___PAIRP(___R3))
   ___GOTO(___L37_sort)
   ___END_IF
___DEF_GLBL(___L34_sort)
   ___SET_R1(___R2)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_SLBL(12,___L12_sort)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L36_sort)
   ___END_IF
   ___SET_R3(___CDR(___STK(-7)))
   ___SET_R2(___STK(-8))
   ___SET_R1(___STK(-9))
   ___SET_R0(___LBL(16))
   ___ADJFP(-4)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L33_sort)
   ___END_IF
___DEF_GLBL(___L35_sort)
   ___SET_R1(___R3)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_GLBL(___L36_sort)
   ___SET_R3(___STK(-7))
   ___SET_R2(___CDR(___STK(-8)))
   ___SET_R1(___STK(-9))
   ___SET_R0(___LBL(13))
   ___IF(___PAIRP(___R2))
   ___GOTO(___L33_sort)
   ___END_IF
   ___GOTO(___L35_sort)
___DEF_SLBL(13,___L13_sort)
   ___SET_R1(___CONS(___STK(-6),___R1))
   ___CHECK_HEAP(14,4096)
___DEF_SLBL(14,___L14_sort)
   ___POLL(15)
___DEF_SLBL(15,___L15_sort)
   ___ADJFP(-12)
   ___JUMPPRM(___NOTHING,___STK(2))
___DEF_SLBL(16,___L16_sort)
   ___SET_R1(___CONS(___STK(-7),___R1))
   ___CHECK_HEAP(17,4096)
___DEF_SLBL(17,___L17_sort)
   ___POLL(18)
___DEF_SLBL(18,___L18_sort)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(2))
___DEF_GLBL(___L37_sort)
   ___SET_R4(___CAR(___R2))
   ___SET_STK(1,___CAR(___R3))
   ___SET_STK(2,___R0)
   ___SET_STK(3,___R1)
   ___SET_STK(4,___R2)
   ___SET_STK(5,___R3)
   ___SET_STK(6,___R4)
   ___SET_R2(___STK(1))
   ___SET_R1(___R4)
   ___SET_R0(___LBL(12))
   ___ADJFP(12)
   ___POLL(19)
___DEF_SLBL(19,___L19_sort)
   ___JUMPGENNOTSAFE(___SET_NARGS(2),___STK(-9))
___DEF_SLBL(20,___L20_sort)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(8))
   ___IF(___PAIRP(___R2))
   ___GOTO(___L29_sort)
   ___END_IF
   ___GOTO(___L34_sort)
___DEF_GLBL(___L38_sort)
   ___IF(___NOT(___VECTORP(___R1)))
   ___GOTO(___L39_sort)
   ___END_IF
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R0(___LBL(22))
   ___ADJFP(8)
   ___POLL(21)
___DEF_SLBL(21,___L21_sort)
   ___JUMPPRM(___SET_NARGS(1),___PRM_vector_2d__3e_list)
___DEF_SLBL(22,___L22_sort)
   ___SET_R2(___STK(-6))
   ___SET_R0(___LBL(23))
   ___ADJFP(-4)
   ___GOTO(___L28_sort)
___DEF_SLBL(23,___L23_sort)
   ___SET_R0(___STK(-3))
   ___POLL(24)
___DEF_SLBL(24,___L24_sort)
   ___ADJFP(-4)
   ___JUMPPRM(___SET_NARGS(1),___PRM_list_2d__3e_vector)
___DEF_GLBL(___L39_sort)
   ___SET_R1(___SUB(0))
   ___POLL(25)
___DEF_SLBL(25,___L25_sort)
   ___GOTO(___L41_sort)
___DEF_GLBL(___L40_sort)
   ___SET_R1(___SUB(1))
   ___POLL(26)
___DEF_SLBL(26,___L26_sort)
___DEF_GLBL(___L41_sort)
   ___JUMPGLONOTSAFE(___SET_NARGS(1),2,___G_error)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_Sort," Sort",___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__20_Sort,0,-1)
,___DEF_LBL_INTRO(___H_sort,0,___REF_FAL,27,0)
,___DEF_LBL_PROC(___H_sort,2,-1)
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,0,0xbL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,0,0xbL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,9,1,0x3fL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,9,1,0x22L))
,___DEF_LBL_RET(___H_sort,___OFD(___RETI,12,1,0x3f002L))
,___DEF_LBL_RET(___H_sort,___OFD(___RETI,12,1,0x3f002L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,1,0x3L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,1,0x3f02L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,1,0x3f02L))
,___DEF_LBL_RET(___H_sort,___OFD(___RETI,12,1,0x3f03fL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H_sort,___IFD(___RETI,0,0,0x3fL))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f002L)
,___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f002L)
,___DEF_OFD(___RETI,12,1)
               ___GCMAP1(0x3f03fL)
___END_OFD

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G__20_Sort,1)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G__20_Sort,1)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_Sort,"Sort")
___END_MOD_SYM_KEY

#endif
