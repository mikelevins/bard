#ifdef ___LINKER_INFO
; File: "main.c", produced by Gambit-C v4.6.6
(
406006
" main"
(" main")
(
)
(
)
(
" main"
)
(
)
(
"bard:repl"
)
 #f
)
#else
#define ___VERSION 406006
#define ___MODULE_NAME " main"
#define ___LINKER_ID ____20_main
#define ___MH_PROC ___H__20_main
#define ___SCRIPT_LINE 0
#define ___GLO_COUNT 2
#define ___SUP_COUNT 1
#define ___LBL_COUNT 3
#include "gambit.h"

___NEED_GLO(___G__20_main)
___NEED_GLO(___G_bard_3a_repl)

___BEGIN_GLO
___DEF_GLO(0," main")
___DEF_GLO(1,"bard:repl")
___END_GLO


#undef ___MD_ALL
#define ___MD_ALL ___D_FP ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_FP ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_main)
___DEF_M_HLBL(___L1__20_main)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_main
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_main)
___DEF_P_HLBL(___L1__20_main)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_main)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_main)
   ___POLL(1)
___DEF_SLBL(1,___L1__20_main)
   ___JUMPGLOSAFE(___SET_NARGS(0),1,___G_bard_3a_repl)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_main," main",___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__20_main,0,0)
,___DEF_LBL_RET(___H__20_main,___IFD(___RETI,0,0,0x3fL))
___END_LBL

___BEGIN_MOD1
___DEF_PRM(0,___G__20_main,1)
___END_MOD1

___BEGIN_MOD2
___END_MOD2

#endif
