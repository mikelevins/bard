#ifdef ___LINKER_INFO
; File: "singleton-tree.c", produced by Gambit-C v4.6.6
(
406006
" singleton-tree"
(" singleton-tree")
(
"##type-1-3623F664-68ED-4642-BB14-B6FDCA33618C"
"##type-5"
"%singleton-tree"
"entries"
"fields"
"flags"
"id"
"name"
"super"
"type"
)
(
"test"
)
(
" singleton-tree"
"%singleton-tree"
"%singleton-tree-entries"
"%singleton-tree-enumerate"
"%singleton-tree-get"
"%singleton-tree-put!"
"%singleton-tree?"
"make-%singleton-tree"
)
(
"%singleton-tree-ref"
)
(
"##direct-structure-ref"
"##structure"
"%as-string"
"+"
"apply"
"car"
"cdr"
"display"
"eq?"
"error"
"make-string"
"make-table"
"newline"
"not"
"null?"
"str"
"table-for-each"
"table-ref"
"table-set!"
"table?"
)
 #f
)
#else
#define ___VERSION 406006
#define ___MODULE_NAME " singleton-tree"
#define ___LINKER_ID ____20_singleton_2d_tree
#define ___MH_PROC ___H__20_singleton_2d_tree
#define ___SCRIPT_LINE 0
#define ___SYM_COUNT 10
#define ___KEY_COUNT 1
#define ___GLO_COUNT 29
#define ___SUP_COUNT 9
#define ___SUB_COUNT 7
#define ___LBL_COUNT 77
#define ___OFD_COUNT 2
#include "gambit.h"

___NEED_SYM(___S__23__23_type_2d_1_2d_3623F664_2d_68ED_2d_4642_2d_BB14_2d_B6FDCA33618C)
___NEED_SYM(___S__23__23_type_2d_5)
___NEED_SYM(___S__25_singleton_2d_tree)
___NEED_SYM(___S_entries)
___NEED_SYM(___S_fields)
___NEED_SYM(___S_flags)
___NEED_SYM(___S_id)
___NEED_SYM(___S_name)
___NEED_SYM(___S_super)
___NEED_SYM(___S_type)

___NEED_KEY(___K_test)

___NEED_GLO(___G__20_singleton_2d_tree)
___NEED_GLO(___G__23__23_direct_2d_structure_2d_ref)
___NEED_GLO(___G__23__23_structure)
___NEED_GLO(___G__25_as_2d_string)
___NEED_GLO(___G__25_singleton_2d_tree)
___NEED_GLO(___G__25_singleton_2d_tree_2d_entries)
___NEED_GLO(___G__25_singleton_2d_tree_2d_enumerate)
___NEED_GLO(___G__25_singleton_2d_tree_2d_get)
___NEED_GLO(___G__25_singleton_2d_tree_2d_put_21_)
___NEED_GLO(___G__25_singleton_2d_tree_2d_ref)
___NEED_GLO(___G__25_singleton_2d_tree_3f_)
___NEED_GLO(___G__2b_)
___NEED_GLO(___G_apply)
___NEED_GLO(___G_car)
___NEED_GLO(___G_cdr)
___NEED_GLO(___G_display)
___NEED_GLO(___G_eq_3f_)
___NEED_GLO(___G_error)
___NEED_GLO(___G_make_2d__25_singleton_2d_tree)
___NEED_GLO(___G_make_2d_string)
___NEED_GLO(___G_make_2d_table)
___NEED_GLO(___G_newline)
___NEED_GLO(___G_not)
___NEED_GLO(___G_null_3f_)
___NEED_GLO(___G_str)
___NEED_GLO(___G_table_2d_for_2d_each)
___NEED_GLO(___G_table_2d_ref)
___NEED_GLO(___G_table_2d_set_21_)
___NEED_GLO(___G_table_3f_)

___BEGIN_SYM1
___DEF_SYM1(0,___S__23__23_type_2d_1_2d_3623F664_2d_68ED_2d_4642_2d_BB14_2d_B6FDCA33618C,"##type-1-3623F664-68ED-4642-BB14-B6FDCA33618C")

___DEF_SYM1(1,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM1(2,___S__25_singleton_2d_tree,"%singleton-tree")
___DEF_SYM1(3,___S_entries,"entries")
___DEF_SYM1(4,___S_fields,"fields")
___DEF_SYM1(5,___S_flags,"flags")
___DEF_SYM1(6,___S_id,"id")
___DEF_SYM1(7,___S_name,"name")
___DEF_SYM1(8,___S_super,"super")
___DEF_SYM1(9,___S_type,"type")
___END_SYM1

___BEGIN_KEY1
___DEF_KEY1(0,___K_test,"test")
___END_KEY1

___BEGIN_GLO
___DEF_GLO(0," singleton-tree")
___DEF_GLO(1,"%singleton-tree")
___DEF_GLO(2,"%singleton-tree-entries")
___DEF_GLO(3,"%singleton-tree-enumerate")
___DEF_GLO(4,"%singleton-tree-get")
___DEF_GLO(5,"%singleton-tree-put!")
___DEF_GLO(6,"%singleton-tree-ref")
___DEF_GLO(7,"%singleton-tree?")
___DEF_GLO(8,"make-%singleton-tree")
___DEF_GLO(9,"##direct-structure-ref")
___DEF_GLO(10,"##structure")
___DEF_GLO(11,"%as-string")
___DEF_GLO(12,"+")
___DEF_GLO(13,"apply")
___DEF_GLO(14,"car")
___DEF_GLO(15,"cdr")
___DEF_GLO(16,"display")
___DEF_GLO(17,"eq?")
___DEF_GLO(18,"error")
___DEF_GLO(19,"make-string")
___DEF_GLO(20,"make-table")
___DEF_GLO(21,"newline")
___DEF_GLO(22,"not")
___DEF_GLO(23,"null?")
___DEF_GLO(24,"str")
___DEF_GLO(25,"table-for-each")
___DEF_GLO(26,"table-ref")
___DEF_GLO(27,"table-set!")
___DEF_GLO(28,"table?")
___END_GLO

___DEF_SUB_STRUCTURE(___X0,6)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(0,___S__23__23_type_2d_1_2d_3623F664_2d_68ED_2d_4642_2d_BB14_2d_B6FDCA33618C))
               ___VEC1(___REF_SYM(2,___S__25_singleton_2d_tree))
               ___VEC1(___REF_FIX(24))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(3))
               ___VEC0
___DEF_SUB_STRUCTURE(___X1,6)
               ___VEC1(___REF_SUB(1))
               ___VEC1(___REF_SYM(1,___S__23__23_type_2d_5))
               ___VEC1(___REF_SYM(9,___S_type))
               ___VEC1(___REF_FIX(8))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(2))
               ___VEC0
___DEF_SUB_VEC(___X2,15)
               ___VEC1(___REF_SYM(6,___S_id))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(7,___S_name))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(5,___S_flags))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(8,___S_super))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(4,___S_fields))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X3,3)
               ___VEC1(___REF_SYM(3,___S_entries))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STR(___X4,41)
               ___STR8(73,110,118,97,108,105,100,32)
               ___STR8(115,101,97,114,99,104,32,112)
               ___STR8(97,116,104,32,102,111,114,32)
               ___STR8(37,115,105,110,103,108,101,116)
               ___STR8(111,110,45,116,114,101,101,58)
               ___STR1(32)
___DEF_SUB_STR(___X5,23)
               ___STR8(78,111,116,32,97,32,37,115)
               ___STR8(105,110,103,108,101,116,111,110)
               ___STR7(45,116,114,101,101,58,32)
___DEF_SUB_STR(___X6,2)
               ___STR2(58,32)

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
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
___DEF_M_HLBL(___L0__20_singleton_2d_tree)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_make_2d__25_singleton_2d_tree)
___DEF_M_HLBL(___L1_make_2d__25_singleton_2d_tree)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree_2d_entries)
___DEF_M_HLBL(___L1__25_singleton_2d_tree_2d_entries)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree)
___DEF_M_HLBL(___L1__25_singleton_2d_tree)
___DEF_M_HLBL(___L2__25_singleton_2d_tree)
___DEF_M_HLBL(___L3__25_singleton_2d_tree)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree_2d_ref)
___DEF_M_HLBL(___L1__25_singleton_2d_tree_2d_ref)
___DEF_M_HLBL(___L2__25_singleton_2d_tree_2d_ref)
___DEF_M_HLBL(___L3__25_singleton_2d_tree_2d_ref)
___DEF_M_HLBL(___L4__25_singleton_2d_tree_2d_ref)
___DEF_M_HLBL(___L5__25_singleton_2d_tree_2d_ref)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L1__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L2__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L3__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L4__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L5__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L6__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L7__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L8__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L9__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L10__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L11__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L12__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL(___L13__25_singleton_2d_tree_2d_get)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L1__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L2__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L3__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L4__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L5__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L6__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L7__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L8__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L9__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L10__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L11__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L12__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L13__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L14__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L15__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L16__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L17__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL(___L18__25_singleton_2d_tree_2d_put_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L1__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L2__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L3__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L4__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L5__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L6__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L7__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L8__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L9__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L10__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L11__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L12__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L13__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L14__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L15__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L16__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L17__25_singleton_2d_tree_2d_enumerate)
___DEF_M_HLBL(___L18__25_singleton_2d_tree_2d_enumerate)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_singleton_2d_tree
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
___DEF_P_HLBL(___L0__20_singleton_2d_tree)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_singleton_2d_tree)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_singleton_2d_tree)
   ___SET_GLO(8,___G_make_2d__25_singleton_2d_tree,___PRC(3))
   ___SET_GLO(7,___G__25_singleton_2d_tree_3f_,___PRC(6))
   ___SET_GLO(2,___G__25_singleton_2d_tree_2d_entries,___PRC(8))
   ___SET_GLO(1,___G__25_singleton_2d_tree,___PRC(11))
   ___SET_GLO(6,___G__25_singleton_2d_tree_2d_ref,___PRC(16))
   ___SET_GLO(4,___G__25_singleton_2d_tree_2d_get,___PRC(23))
   ___SET_GLO(5,___G__25_singleton_2d_tree_2d_put_21_,___PRC(38))
   ___SET_GLO(3,___G__25_singleton_2d_tree_2d_enumerate,___PRC(58))
   ___SET_R1(___VOID)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_make_2d__25_singleton_2d_tree
#undef ___PH_LBL0
#define ___PH_LBL0 3
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_make_2d__25_singleton_2d_tree)
___DEF_P_HLBL(___L1_make_2d__25_singleton_2d_tree)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_make_2d__25_singleton_2d_tree)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L_make_2d__25_singleton_2d_tree)
   ___SET_R2(___R1)
   ___SET_R1(___SUB(0))
   ___POLL(1)
___DEF_SLBL(1,___L1_make_2d__25_singleton_2d_tree)
   ___JUMPPRM(___SET_NARGS(2),___PRM(10,___G__23__23_structure))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 6
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_singleton_2d_tree_3f_)
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___SYM(0,___S__23__23_type_2d_1_2d_3623F664_2d_68ED_2d_4642_2d_BB14_2d_B6FDCA33618C))))
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree_2d_entries
#undef ___PH_LBL0
#define ___PH_LBL0 8
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree_2d_entries)
___DEF_P_HLBL(___L1__25_singleton_2d_tree_2d_entries)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree_2d_entries)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_singleton_2d_tree_2d_entries)
   ___SET_STK(1,___GLO(2,___G__25_singleton_2d_tree_2d_entries))
   ___SET_R2(___TYPEID(___SUB(0)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_singleton_2d_tree_2d_entries)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(0))
   ___SET_R1(___FIX(1L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_singleton_2d_tree_2d_entries)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(9,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_singleton_2d_tree_2d_entries)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(0),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree
#undef ___PH_LBL0
#define ___PH_LBL0 11
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree)
___DEF_P_HLBL(___L1__25_singleton_2d_tree)
___DEF_P_HLBL(___L2__25_singleton_2d_tree)
___DEF_P_HLBL(___L3__25_singleton_2d_tree)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__25_singleton_2d_tree)
   ___SET_STK(1,___R0)
   ___SET_R2(___GLO(17,___G_eq_3f_))
   ___SET_R1(___KEY(0,___K_test))
   ___SET_R0(___LBL(2))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_singleton_2d_tree)
   ___JUMPGLOSAFE(___SET_NARGS(2),20,___G_make_2d_table)
___DEF_SLBL(2,___L2__25_singleton_2d_tree)
   ___SET_R0(___STK(-3))
   ___POLL(3)
___DEF_SLBL(3,___L3__25_singleton_2d_tree)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G_make_2d__25_singleton_2d_tree)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree_2d_ref
#undef ___PH_LBL0
#define ___PH_LBL0 16
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree_2d_ref)
___DEF_P_HLBL(___L1__25_singleton_2d_tree_2d_ref)
___DEF_P_HLBL(___L2__25_singleton_2d_tree_2d_ref)
___DEF_P_HLBL(___L3__25_singleton_2d_tree_2d_ref)
___DEF_P_HLBL(___L4__25_singleton_2d_tree_2d_ref)
___DEF_P_HLBL(___L5__25_singleton_2d_tree_2d_ref)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree_2d_ref)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_singleton_2d_tree_2d_ref)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_singleton_2d_tree_2d_ref)
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_singleton_2d_tree_3f_)
___DEF_SLBL(2,___L2__25_singleton_2d_tree_2d_ref)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L6__25_singleton_2d_tree_2d_ref)
   ___END_IF
   ___SET_R1(___FAL)
   ___POLL(3)
___DEF_SLBL(3,___L3__25_singleton_2d_tree_2d_ref)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L6__25_singleton_2d_tree_2d_ref)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),2,___G__25_singleton_2d_tree_2d_entries)
___DEF_SLBL(4,___L4__25_singleton_2d_tree_2d_ref)
   ___SET_R2(___STK(-5))
   ___SET_R3(___FAL)
   ___SET_R0(___STK(-7))
   ___POLL(5)
___DEF_SLBL(5,___L5__25_singleton_2d_tree_2d_ref)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(3),26,___G_table_2d_ref)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree_2d_get
#undef ___PH_LBL0
#define ___PH_LBL0 23
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L1__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L2__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L3__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L4__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L5__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L6__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L7__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L8__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L9__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L10__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L11__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L12__25_singleton_2d_tree_2d_get)
___DEF_P_HLBL(___L13__25_singleton_2d_tree_2d_get)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree_2d_get)
   ___IF_NARGS_EQ(1,___SET_R2(___NUL))
   ___GET_REST(0,1,0,0)
___DEF_GLBL(___L__25_singleton_2d_tree_2d_get)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_singleton_2d_tree_2d_get)
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_singleton_2d_tree_3f_)
___DEF_SLBL(2,___L2__25_singleton_2d_tree_2d_get)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L14__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___GOTO(___L25__25_singleton_2d_tree_2d_get)
___DEF_SLBL(3,___L3__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L16__25_singleton_2d_tree_2d_get)
   ___END_IF
___DEF_GLBL(___L14__25_singleton_2d_tree_2d_get)
   ___SET_R1(___FAL)
   ___POLL(4)
___DEF_SLBL(4,___L4__25_singleton_2d_tree_2d_get)
___DEF_GLBL(___L15__25_singleton_2d_tree_2d_get)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L16__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___EQP(___GLO(15,___G_cdr),___PRM(15,___G_cdr))))
   ___GOTO(___L24__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L24__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___SET_R2(___CDR(___STK(-5)))
   ___IF(___EQP(___GLO(23,___G_null_3f_),___PRM(23,___G_null_3f_)))
   ___GOTO(___L17__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___GOTO(___L23__25_singleton_2d_tree_2d_get)
___DEF_SLBL(5,___L5__25_singleton_2d_tree_2d_get)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___IF(___NOT(___EQP(___GLO(23,___G_null_3f_),___PRM(23,___G_null_3f_))))
   ___GOTO(___L23__25_singleton_2d_tree_2d_get)
   ___END_IF
___DEF_GLBL(___L17__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___NULLP(___R2)))
   ___GOTO(___L18__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___POLL(6)
___DEF_SLBL(6,___L6__25_singleton_2d_tree_2d_get)
   ___GOTO(___L15__25_singleton_2d_tree_2d_get)
___DEF_GLBL(___L18__25_singleton_2d_tree_2d_get)
   ___IF(___EQP(___GLO(15,___G_cdr),___PRM(15,___G_cdr)))
   ___GOTO(___L19__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___GOTO(___L21__25_singleton_2d_tree_2d_get)
___DEF_SLBL(7,___L7__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L22__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___IF(___NOT(___EQP(___GLO(15,___G_cdr),___PRM(15,___G_cdr))))
   ___GOTO(___L21__25_singleton_2d_tree_2d_get)
   ___END_IF
___DEF_GLBL(___L19__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L21__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___SET_R2(___CDR(___STK(-5)))
___DEF_GLBL(___L20__25_singleton_2d_tree_2d_get)
   ___SET_R3(___R2)
   ___SET_R2(___R1)
   ___SET_R1(___GLO(4,___G__25_singleton_2d_tree_2d_get))
   ___SET_R0(___STK(-7))
   ___POLL(8)
___DEF_SLBL(8,___L8__25_singleton_2d_tree_2d_get)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(3),13,___G_apply)
___DEF_GLBL(___L21__25_singleton_2d_tree_2d_get)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(9))
   ___JUMPGLOSAFE(___SET_NARGS(1),15,___G_cdr)
___DEF_SLBL(9,___L9__25_singleton_2d_tree_2d_get)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___GOTO(___L20__25_singleton_2d_tree_2d_get)
___DEF_GLBL(___L22__25_singleton_2d_tree_2d_get)
   ___SET_R1(___STK(-6))
   ___POLL(10)
___DEF_SLBL(10,___L10__25_singleton_2d_tree_2d_get)
   ___GOTO(___L15__25_singleton_2d_tree_2d_get)
___DEF_GLBL(___L23__25_singleton_2d_tree_2d_get)
   ___SET_STK(-6,___R1)
   ___SET_R1(___R2)
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_null_3f_)
___DEF_GLBL(___L24__25_singleton_2d_tree_2d_get)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(1),15,___G_cdr)
___DEF_GLBL(___L25__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___EQP(___GLO(23,___G_null_3f_),___PRM(23,___G_null_3f_))))
   ___GOTO(___L29__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___IF(___NULLP(___STK(-5)))
   ___GOTO(___L14__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___GOTO(___L26__25_singleton_2d_tree_2d_get)
___DEF_SLBL(11,___L11__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L14__25_singleton_2d_tree_2d_get)
   ___END_IF
___DEF_GLBL(___L26__25_singleton_2d_tree_2d_get)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(1),2,___G__25_singleton_2d_tree_2d_entries)
___DEF_SLBL(12,___L12__25_singleton_2d_tree_2d_get)
   ___IF(___NOT(___EQP(___GLO(14,___G_car),___PRM(14,___G_car))))
   ___GOTO(___L28__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___IF(___NOT(___PAIRP(___STK(-5))))
   ___GOTO(___L28__25_singleton_2d_tree_2d_get)
   ___END_IF
   ___SET_R2(___CAR(___STK(-5)))
   ___GOTO(___L27__25_singleton_2d_tree_2d_get)
___DEF_SLBL(13,___L13__25_singleton_2d_tree_2d_get)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
___DEF_GLBL(___L27__25_singleton_2d_tree_2d_get)
   ___SET_R3(___FAL)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(3),26,___G_table_2d_ref)
___DEF_GLBL(___L28__25_singleton_2d_tree_2d_get)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G_car)
___DEF_GLBL(___L29__25_singleton_2d_tree_2d_get)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_null_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree_2d_put_21_
#undef ___PH_LBL0
#define ___PH_LBL0 38
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L1__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L2__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L3__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L4__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L5__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L6__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L7__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L8__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L9__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L10__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L11__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L12__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L13__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L14__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L15__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L16__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L17__25_singleton_2d_tree_2d_put_21_)
___DEF_P_HLBL(___L18__25_singleton_2d_tree_2d_put_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree_2d_put_21_)
   ___IF_NARGS_EQ(2,___SET_R3(___NUL))
   ___GET_REST(0,2,0,0)
___DEF_GLBL(___L__25_singleton_2d_tree_2d_put_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R1(___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_singleton_2d_tree_2d_put_21_)
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_singleton_2d_tree_3f_)
___DEF_SLBL(2,___L2__25_singleton_2d_tree_2d_put_21_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L36__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___IF(___NOT(___EQP(___GLO(23,___G_null_3f_),___PRM(23,___G_null_3f_))))
   ___GOTO(___L35__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___IF(___NULLP(___STK(-4)))
   ___GOTO(___L34__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___GOTO(___L19__25_singleton_2d_tree_2d_put_21_)
___DEF_SLBL(3,___L3__25_singleton_2d_tree_2d_put_21_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L34__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
___DEF_GLBL(___L19__25_singleton_2d_tree_2d_put_21_)
   ___IF(___NOT(___EQP(___GLO(14,___G_car),___PRM(14,___G_car))))
   ___GOTO(___L33__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___IF(___NOT(___PAIRP(___STK(-4))))
   ___GOTO(___L33__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___SET_R1(___CAR(___STK(-4)))
   ___IF(___NOT(___EQP(___GLO(15,___G_cdr),___PRM(15,___G_cdr))))
   ___GOTO(___L32__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
___DEF_GLBL(___L20__25_singleton_2d_tree_2d_put_21_)
   ___IF(___NOT(___PAIRP(___STK(-4))))
   ___GOTO(___L32__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___SET_R2(___CDR(___STK(-4)))
___DEF_GLBL(___L21__25_singleton_2d_tree_2d_put_21_)
   ___SET_STK(-3,___R1)
   ___SET_STK(-2,___R2)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(4))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(1),2,___G__25_singleton_2d_tree_2d_entries)
___DEF_SLBL(4,___L4__25_singleton_2d_tree_2d_put_21_)
   ___IF(___NOT(___EQP(___GLO(23,___G_null_3f_),___PRM(23,___G_null_3f_))))
   ___GOTO(___L31__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___IF(___NOT(___NULLP(___STK(-6))))
   ___GOTO(___L24__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___ADJFP(-4)
   ___GOTO(___L22__25_singleton_2d_tree_2d_put_21_)
___DEF_SLBL(5,___L5__25_singleton_2d_tree_2d_put_21_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L23__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___SET_R1(___STK(-5))
___DEF_GLBL(___L22__25_singleton_2d_tree_2d_put_21_)
   ___SET_R3(___STK(-6))
   ___SET_R2(___STK(-3))
   ___SET_R0(___STK(-7))
   ___POLL(6)
___DEF_SLBL(6,___L6__25_singleton_2d_tree_2d_put_21_)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(3),27,___G_table_2d_set_21_)
___DEF_GLBL(___L23__25_singleton_2d_tree_2d_put_21_)
   ___SET_R1(___STK(-5))
   ___GOTO(___L25__25_singleton_2d_tree_2d_put_21_)
___DEF_GLBL(___L24__25_singleton_2d_tree_2d_put_21_)
   ___ADJFP(-4)
___DEF_GLBL(___L25__25_singleton_2d_tree_2d_put_21_)
   ___SET_STK(-5,___R1)
   ___SET_R2(___STK(-3))
   ___SET_R3(___FAL)
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(3),26,___G_table_2d_ref)
___DEF_SLBL(7,___L7__25_singleton_2d_tree_2d_put_21_)
   ___SET_R1(___BOX(___R1))
   ___SET_STK(-2,___R1)
   ___SET_R1(___UNBOX(___R1))
   ___SET_R0(___LBL(9))
   ___ADJFP(4)
   ___CHECK_HEAP(8,4096)
___DEF_SLBL(8,___L8__25_singleton_2d_tree_2d_put_21_)
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_singleton_2d_tree_3f_)
___DEF_SLBL(9,___L9__25_singleton_2d_tree_2d_put_21_)
   ___IF(___NOT(___EQP(___GLO(22,___G_not),___PRM(22,___G_not))))
   ___GOTO(___L30__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___IF(___FALSEP(___R1))
   ___GOTO(___L29__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___GOTO(___L26__25_singleton_2d_tree_2d_put_21_)
___DEF_SLBL(10,___L10__25_singleton_2d_tree_2d_put_21_)
___DEF_GLBL(___L26__25_singleton_2d_tree_2d_put_21_)
   ___SET_STK(-9,___STK(-11))
   ___SET_STK(-11,___GLO(5,___G__25_singleton_2d_tree_2d_put_21_))
   ___IF(___NOT(___EQP(___GLO(15,___G_cdr),___PRM(15,___G_cdr))))
   ___GOTO(___L28__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___IF(___NOT(___PAIRP(___STK(-8))))
   ___GOTO(___L28__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
   ___SET_R1(___CDR(___STK(-8)))
___DEF_GLBL(___L27__25_singleton_2d_tree_2d_put_21_)
   ___SET_R3(___R1)
   ___SET_R2(___UNBOX(___STK(-6)))
   ___SET_R1(___STK(-10))
   ___SET_R0(___STK(-9))
   ___POLL(11)
___DEF_SLBL(11,___L11__25_singleton_2d_tree_2d_put_21_)
   ___ADJFP(-11)
   ___JUMPGLOSAFE(___SET_NARGS(4),13,___G_apply)
___DEF_GLBL(___L28__25_singleton_2d_tree_2d_put_21_)
   ___SET_R1(___STK(-8))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(1),15,___G_cdr)
___DEF_SLBL(12,___L12__25_singleton_2d_tree_2d_put_21_)
   ___GOTO(___L27__25_singleton_2d_tree_2d_put_21_)
___DEF_SLBL(13,___L13__25_singleton_2d_tree_2d_put_21_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L26__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
___DEF_GLBL(___L29__25_singleton_2d_tree_2d_put_21_)
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(0),1,___G__25_singleton_2d_tree)
___DEF_SLBL(14,___L14__25_singleton_2d_tree_2d_put_21_)
   ___SETBOX(___STK(-6),___R1)
   ___SET_R3(___UNBOX(___STK(-6)))
   ___SET_R2(___STK(-7))
   ___SET_R1(___STK(-9))
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(3),27,___G_table_2d_set_21_)
___DEF_GLBL(___L30__25_singleton_2d_tree_2d_put_21_)
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),22,___G_not)
___DEF_GLBL(___L31__25_singleton_2d_tree_2d_put_21_)
   ___SET_STK(-9,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(5))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_null_3f_)
___DEF_SLBL(15,___L15__25_singleton_2d_tree_2d_put_21_)
   ___IF(___EQP(___GLO(15,___G_cdr),___PRM(15,___G_cdr)))
   ___GOTO(___L20__25_singleton_2d_tree_2d_put_21_)
   ___END_IF
___DEF_GLBL(___L32__25_singleton_2d_tree_2d_put_21_)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(1),15,___G_cdr)
___DEF_SLBL(16,___L16__25_singleton_2d_tree_2d_put_21_)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-3))
   ___GOTO(___L21__25_singleton_2d_tree_2d_put_21_)
___DEF_GLBL(___L33__25_singleton_2d_tree_2d_put_21_)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G_car)
___DEF_GLBL(___L34__25_singleton_2d_tree_2d_put_21_)
   ___SET_R2(___STK(-4))
   ___SET_R1(___SUB(4))
   ___SET_R0(___LBL(17))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),24,___G_str)
___DEF_SLBL(17,___L17__25_singleton_2d_tree_2d_put_21_)
   ___SET_R0(___STK(-3))
   ___POLL(18)
___DEF_SLBL(18,___L18__25_singleton_2d_tree_2d_put_21_)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),18,___G_error)
___DEF_GLBL(___L35__25_singleton_2d_tree_2d_put_21_)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G_null_3f_)
___DEF_GLBL(___L36__25_singleton_2d_tree_2d_put_21_)
   ___SET_R2(___STK(-5))
   ___SET_R1(___SUB(5))
   ___SET_R0(___LBL(17))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),24,___G_str)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_singleton_2d_tree_2d_enumerate
#undef ___PH_LBL0
#define ___PH_LBL0 58
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L1__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L2__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L3__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L4__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L5__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L6__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L7__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L8__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L9__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L10__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L11__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L12__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L13__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L14__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L15__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L16__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L17__25_singleton_2d_tree_2d_enumerate)
___DEF_P_HLBL(___L18__25_singleton_2d_tree_2d_enumerate)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_singleton_2d_tree_2d_enumerate)
   ___IF_NARGS_EQ(1,___SET_R2(___FIX(0L)))
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,1,1,0)
___DEF_GLBL(___L__25_singleton_2d_tree_2d_enumerate)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_singleton_2d_tree_2d_enumerate)
   ___JUMPGLOSAFE(___SET_NARGS(1),2,___G__25_singleton_2d_tree_2d_entries)
___DEF_SLBL(2,___L2__25_singleton_2d_tree_2d_enumerate)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R2(___CHR(32))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(2),19,___G_make_2d_string)
___DEF_SLBL(3,___L3__25_singleton_2d_tree_2d_enumerate)
   ___IF(___FALSEP(___STK(-5)))
   ___GOTO(___L19__25_singleton_2d_tree_2d_enumerate)
   ___END_IF
   ___GOTO(___L24__25_singleton_2d_tree_2d_enumerate)
___DEF_SLBL(4,___L4__25_singleton_2d_tree_2d_enumerate)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L20__25_singleton_2d_tree_2d_enumerate)
   ___END_IF
___DEF_GLBL(___L19__25_singleton_2d_tree_2d_enumerate)
   ___SET_R1(___VOID)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_singleton_2d_tree_2d_enumerate)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L20__25_singleton_2d_tree_2d_enumerate)
   ___SET_STK(-3,___ALLOC_CLO(2))
   ___BEGIN_SETUP_CLO(2,___STK(-3),8)
   ___ADD_CLO_ELEM(0,___STK(-6))
   ___ADD_CLO_ELEM(1,___STK(-4))
   ___END_SETUP_CLO(2)
   ___SET_R1(___STK(-3))
   ___SET_R2(___STK(-5))
   ___SET_R0(___STK(-7))
   ___ADJFP(-3)
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6__25_singleton_2d_tree_2d_enumerate)
   ___POLL(7)
___DEF_SLBL(7,___L7__25_singleton_2d_tree_2d_enumerate)
   ___ADJFP(-5)
   ___JUMPGLOSAFE(___SET_NARGS(2),25,___G_table_2d_for_2d_each)
___DEF_SLBL(8,___L8__25_singleton_2d_tree_2d_enumerate)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(8,2,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R4)
   ___SET_R0(___LBL(10))
   ___ADJFP(8)
   ___POLL(9)
___DEF_SLBL(9,___L9__25_singleton_2d_tree_2d_enumerate)
   ___JUMPGLOSAFE(___SET_NARGS(0),21,___G_newline)
___DEF_SLBL(10,___L10__25_singleton_2d_tree_2d_enumerate)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G__25_as_2d_string)
___DEF_SLBL(11,___L11__25_singleton_2d_tree_2d_enumerate)
   ___SET_R2(___R1)
   ___SET_R1(___CLO(___STK(-4),2))
   ___SET_R3(___SUB(6))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(3),24,___G_str)
___DEF_SLBL(12,___L12__25_singleton_2d_tree_2d_enumerate)
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),16,___G_display)
___DEF_SLBL(13,___L13__25_singleton_2d_tree_2d_enumerate)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(1),7,___G__25_singleton_2d_tree_3f_)
___DEF_SLBL(14,___L14__25_singleton_2d_tree_2d_enumerate)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L23__25_singleton_2d_tree_2d_enumerate)
   ___END_IF
   ___IF(___NOT(___EQP(___GLO(12,___G__2b_),___PRM(12,___G__2b_))))
   ___GOTO(___L22__25_singleton_2d_tree_2d_enumerate)
   ___END_IF
   ___SET_R0(___CLO(___STK(-4),1))
   ___IF(___NOT(___FIXNUMP(___R0)))
   ___GOTO(___L22__25_singleton_2d_tree_2d_enumerate)
   ___END_IF
   ___SET_R0(___CLO(___STK(-4),1))
   ___SET_R1(___FIXADDP(___R0,___FIX(2L)))
   ___IF(___FALSEP(___R1))
   ___GOTO(___L22__25_singleton_2d_tree_2d_enumerate)
   ___END_IF
   ___GOTO(___L21__25_singleton_2d_tree_2d_enumerate)
___DEF_SLBL(15,___L15__25_singleton_2d_tree_2d_enumerate)
___DEF_GLBL(___L21__25_singleton_2d_tree_2d_enumerate)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-7))
   ___POLL(16)
___DEF_SLBL(16,___L16__25_singleton_2d_tree_2d_enumerate)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),3,___G__25_singleton_2d_tree_2d_enumerate)
___DEF_GLBL(___L22__25_singleton_2d_tree_2d_enumerate)
   ___SET_R1(___CLO(___STK(-4),1))
   ___SET_R2(___FIX(2L))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G__2b_)
___DEF_GLBL(___L23__25_singleton_2d_tree_2d_enumerate)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(17))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),11,___G__25_as_2d_string)
___DEF_SLBL(17,___L17__25_singleton_2d_tree_2d_enumerate)
   ___SET_R0(___STK(-3))
   ___POLL(18)
___DEF_SLBL(18,___L18__25_singleton_2d_tree_2d_enumerate)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),16,___G_display)
___DEF_GLBL(___L24__25_singleton_2d_tree_2d_enumerate)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),28,___G_table_3f_)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_singleton_2d_tree," singleton-tree",___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__20_singleton_2d_tree,0,0)
,___DEF_LBL_INTRO(___H_make_2d__25_singleton_2d_tree,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_make_2d__25_singleton_2d_tree,1,0)
,___DEF_LBL_RET(___H_make_2d__25_singleton_2d_tree,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree_3f_,0,___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_3f_,1,0)
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree_2d_entries,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_2d_entries,1,0)
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_entries,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree,0,___REF_FAL,4,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree,0,0)
,___DEF_LBL_RET(___H__25_singleton_2d_tree,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree_2d_ref,0,___REF_FAL,6,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_2d_ref,2,0)
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_ref,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_ref,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_ref,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_ref,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_ref,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree_2d_get,0,___REF_FAL,14,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_2d_get,2,0)
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_get,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree_2d_put_21_,0,___REF_FAL,19,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_2d_put_21_,3,0)
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,9,0,0x3bL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___OFD(___RETI,12,0,0x3f03fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,9,0,0x3fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,9,0,0x2bL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___OFD(___RETI,12,12,0x3f001L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,9,2,0x27L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,9,0,0x3fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,9,0,0x3fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_put_21_,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_INTRO(___H__25_singleton_2d_tree_2d_enumerate,0,___REF_FAL,19,0)
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_2d_enumerate,2,0)
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,5,8,0x3f00L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,5,8,0x3f00L))
,___DEF_LBL_PROC(___H__25_singleton_2d_tree_2d_enumerate,2,2)
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_singleton_2d_tree_2d_enumerate,___IFD(___RETI,4,4,0x3f0L))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,12,0)
               ___GCMAP1(0x3f03fL)
,___DEF_OFD(___RETI,12,12)
               ___GCMAP1(0x3f001L)
___END_OFD

___BEGIN_MOD1
___DEF_PRM(0,___G__20_singleton_2d_tree,1)
___END_MOD1

___BEGIN_MOD2
___DEF_SYM2(0,___S__23__23_type_2d_1_2d_3623F664_2d_68ED_2d_4642_2d_BB14_2d_B6FDCA33618C,"##type-1-3623F664-68ED-4642-BB14-B6FDCA33618C")

___DEF_SYM2(1,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM2(2,___S__25_singleton_2d_tree,"%singleton-tree")
___DEF_SYM2(3,___S_entries,"entries")
___DEF_SYM2(4,___S_fields,"fields")
___DEF_SYM2(5,___S_flags,"flags")
___DEF_SYM2(6,___S_id,"id")
___DEF_SYM2(7,___S_name,"name")
___DEF_SYM2(8,___S_super,"super")
___DEF_SYM2(9,___S_type,"type")
___DEF_KEY2(0,___K_test,"test")
___END_MOD2

#endif
