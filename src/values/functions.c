#ifdef ___LINKER_INFO
; File: "functions.c", produced by Gambit-C v4.6.6
(
406006
" functions"
(" functions")
(
"##type-4-0E7FE105-F4B7-4EE4-AA16-9475976B003D"
"##type-4-32EDAE4A-BA00-4313-BC4F-1F3A1F7AC8C0"
"##type-4-927A1AD2-762A-4DE6-9900-C22857D20E5A"
"##type-5"
"%function"
"%generator"
"%interpreted-method"
"%method"
"%primitive-method"
"<function>"
"<generator>"
"<interpreted-method>"
"<primitive-method>"
"bindings"
"body"
"environment"
"fields"
"flags"
"formals"
"function"
"id"
"methods"
"name"
"required-count"
"restarg"
"signatures"
"super"
"then-expression"
"type"
"yield-expression"
)
(
"environment"
"name"
"parameters"
"required-count"
"restarg"
)
(
" functions"
"##type-5-%primitive-method"
"##type-6-%interpreted-method"
"%function-max-method-index"
"%function-method-formals"
"%function-method-signatures"
"%function-methods"
"%function-name"
"%function-nth-method"
"%function-nth-method-signature"
"%generator-bindings"
"%generator-environment"
"%generator-then-expression"
"%generator-yield-expression"
"%make-function"
"%make-generator"
"%make-interpreted-method"
"%make-primitive-method"
"%method-body"
"%method-environment"
"%method-formals"
"%method-function"
"%method-name"
"%method-required-count"
"%method-restarg"
"%method-signature-matches?"
"%method-signature-more-specific?"
"%private-make-function"
"%private-make-interpreted-method"
"%private-make-primitive-method"
"%set-function-method-formals!"
"%set-function-method-signatures!"
"%set-function-methods!"
"%set-generator-bindings!"
"%set-generator-environment!"
"%type-more-specific?"
)
(
"%add-method!"
"%add-primitive-method!"
"%function-best-method"
"%function?"
"%generator?"
"%interpreted-method?"
"%method-name?"
"%method?"
"%next"
"%primitive-method?"
"<function>"
"<generator>"
"<interpreted-method>"
"<primitive-method>"
"make-%method"
)
(
"##direct-structure-ref"
"##direct-structure-set!"
"##make-uninterned-symbol"
"##structure"
"##structure-instance-of?"
"##structure-ref"
"##structure-type"
"##type-type"
"%append"
"%cadr"
"%car"
"%cdr"
"%cons"
"%define-standard-type"
"%eval"
"%eval-sequence"
"%every?"
"%instance-of?"
"%length"
"%list"
"%list-put"
"%list-ref"
"%nil"
"%null?"
"%object->bard-type"
"%position"
"%singleton-value"
"%singleton?"
"+"
"-"
"="
">"
">="
"Anything"
"eq?"
"equal?"
"list"
"map"
"string?"
"symbol?"
"values"
)
 #f
)
#else
#define ___VERSION 406006
#define ___MODULE_NAME " functions"
#define ___LINKER_ID ____20_functions
#define ___MH_PROC ___H__20_functions
#define ___SCRIPT_LINE 0
#define ___SYM_COUNT 30
#define ___KEY_COUNT 5
#define ___GLO_COUNT 92
#define ___SUP_COUNT 51
#define ___SUB_COUNT 16
#define ___LBL_COUNT 329
#define ___OFD_COUNT 4
#include "gambit.h"

___NEED_SYM(___S__23__23_type_2d_4_2d_0E7FE105_2d_F4B7_2d_4EE4_2d_AA16_2d_9475976B003D)
___NEED_SYM(___S__23__23_type_2d_4_2d_32EDAE4A_2d_BA00_2d_4313_2d_BC4F_2d_1F3A1F7AC8C0)
___NEED_SYM(___S__23__23_type_2d_4_2d_927A1AD2_2d_762A_2d_4DE6_2d_9900_2d_C22857D20E5A)
___NEED_SYM(___S__23__23_type_2d_5)
___NEED_SYM(___S__25_function)
___NEED_SYM(___S__25_generator)
___NEED_SYM(___S__25_interpreted_2d_method)
___NEED_SYM(___S__25_method)
___NEED_SYM(___S__25_primitive_2d_method)
___NEED_SYM(___S__3c_function_3e_)
___NEED_SYM(___S__3c_generator_3e_)
___NEED_SYM(___S__3c_interpreted_2d_method_3e_)
___NEED_SYM(___S__3c_primitive_2d_method_3e_)
___NEED_SYM(___S_bindings)
___NEED_SYM(___S_body)
___NEED_SYM(___S_environment)
___NEED_SYM(___S_fields)
___NEED_SYM(___S_flags)
___NEED_SYM(___S_formals)
___NEED_SYM(___S_function)
___NEED_SYM(___S_id)
___NEED_SYM(___S_methods)
___NEED_SYM(___S_name)
___NEED_SYM(___S_required_2d_count)
___NEED_SYM(___S_restarg)
___NEED_SYM(___S_signatures)
___NEED_SYM(___S_super)
___NEED_SYM(___S_then_2d_expression)
___NEED_SYM(___S_type)
___NEED_SYM(___S_yield_2d_expression)

___NEED_KEY(___K_environment)
___NEED_KEY(___K_name)
___NEED_KEY(___K_parameters)
___NEED_KEY(___K_required_2d_count)
___NEED_KEY(___K_restarg)

___NEED_GLO(___G__20_functions)
___NEED_GLO(___G__23__23_direct_2d_structure_2d_ref)
___NEED_GLO(___G__23__23_direct_2d_structure_2d_set_21_)
___NEED_GLO(___G__23__23_make_2d_uninterned_2d_symbol)
___NEED_GLO(___G__23__23_structure)
___NEED_GLO(___G__23__23_structure_2d_instance_2d_of_3f_)
___NEED_GLO(___G__23__23_structure_2d_ref)
___NEED_GLO(___G__23__23_structure_2d_type)
___NEED_GLO(___G__23__23_type_2d_5_2d__25_primitive_2d_method)
___NEED_GLO(___G__23__23_type_2d_6_2d__25_interpreted_2d_method)
___NEED_GLO(___G__23__23_type_2d_type)
___NEED_GLO(___G__25_add_2d_method_21_)
___NEED_GLO(___G__25_add_2d_primitive_2d_method_21_)
___NEED_GLO(___G__25_append)
___NEED_GLO(___G__25_cadr)
___NEED_GLO(___G__25_car)
___NEED_GLO(___G__25_cdr)
___NEED_GLO(___G__25_cons)
___NEED_GLO(___G__25_define_2d_standard_2d_type)
___NEED_GLO(___G__25_eval)
___NEED_GLO(___G__25_eval_2d_sequence)
___NEED_GLO(___G__25_every_3f_)
___NEED_GLO(___G__25_function_2d_best_2d_method)
___NEED_GLO(___G__25_function_2d_max_2d_method_2d_index)
___NEED_GLO(___G__25_function_2d_method_2d_formals)
___NEED_GLO(___G__25_function_2d_method_2d_signatures)
___NEED_GLO(___G__25_function_2d_methods)
___NEED_GLO(___G__25_function_2d_name)
___NEED_GLO(___G__25_function_2d_nth_2d_method)
___NEED_GLO(___G__25_function_2d_nth_2d_method_2d_signature)
___NEED_GLO(___G__25_function_3f_)
___NEED_GLO(___G__25_generator_2d_bindings)
___NEED_GLO(___G__25_generator_2d_environment)
___NEED_GLO(___G__25_generator_2d_then_2d_expression)
___NEED_GLO(___G__25_generator_2d_yield_2d_expression)
___NEED_GLO(___G__25_generator_3f_)
___NEED_GLO(___G__25_instance_2d_of_3f_)
___NEED_GLO(___G__25_interpreted_2d_method_3f_)
___NEED_GLO(___G__25_length)
___NEED_GLO(___G__25_list)
___NEED_GLO(___G__25_list_2d_put)
___NEED_GLO(___G__25_list_2d_ref)
___NEED_GLO(___G__25_make_2d_function)
___NEED_GLO(___G__25_make_2d_generator)
___NEED_GLO(___G__25_make_2d_interpreted_2d_method)
___NEED_GLO(___G__25_make_2d_primitive_2d_method)
___NEED_GLO(___G__25_method_2d_body)
___NEED_GLO(___G__25_method_2d_environment)
___NEED_GLO(___G__25_method_2d_formals)
___NEED_GLO(___G__25_method_2d_function)
___NEED_GLO(___G__25_method_2d_name)
___NEED_GLO(___G__25_method_2d_name_3f_)
___NEED_GLO(___G__25_method_2d_required_2d_count)
___NEED_GLO(___G__25_method_2d_restarg)
___NEED_GLO(___G__25_method_2d_signature_2d_matches_3f_)
___NEED_GLO(___G__25_method_2d_signature_2d_more_2d_specific_3f_)
___NEED_GLO(___G__25_method_3f_)
___NEED_GLO(___G__25_next)
___NEED_GLO(___G__25_nil)
___NEED_GLO(___G__25_null_3f_)
___NEED_GLO(___G__25_object_2d__3e_bard_2d_type)
___NEED_GLO(___G__25_position)
___NEED_GLO(___G__25_primitive_2d_method_3f_)
___NEED_GLO(___G__25_private_2d_make_2d_function)
___NEED_GLO(___G__25_private_2d_make_2d_interpreted_2d_method)
___NEED_GLO(___G__25_private_2d_make_2d_primitive_2d_method)
___NEED_GLO(___G__25_set_2d_function_2d_method_2d_formals_21_)
___NEED_GLO(___G__25_set_2d_function_2d_method_2d_signatures_21_)
___NEED_GLO(___G__25_set_2d_function_2d_methods_21_)
___NEED_GLO(___G__25_set_2d_generator_2d_bindings_21_)
___NEED_GLO(___G__25_set_2d_generator_2d_environment_21_)
___NEED_GLO(___G__25_singleton_2d_value)
___NEED_GLO(___G__25_singleton_3f_)
___NEED_GLO(___G__25_type_2d_more_2d_specific_3f_)
___NEED_GLO(___G__2b_)
___NEED_GLO(___G__2d_)
___NEED_GLO(___G__3c_function_3e_)
___NEED_GLO(___G__3c_generator_3e_)
___NEED_GLO(___G__3c_interpreted_2d_method_3e_)
___NEED_GLO(___G__3c_primitive_2d_method_3e_)
___NEED_GLO(___G__3d_)
___NEED_GLO(___G__3e_)
___NEED_GLO(___G__3e__3d_)
___NEED_GLO(___G_Anything)
___NEED_GLO(___G_eq_3f_)
___NEED_GLO(___G_equal_3f_)
___NEED_GLO(___G_list)
___NEED_GLO(___G_make_2d__25_method)
___NEED_GLO(___G_map)
___NEED_GLO(___G_string_3f_)
___NEED_GLO(___G_symbol_3f_)
___NEED_GLO(___G_values)

___BEGIN_SYM1
___DEF_SYM1(0,___S__23__23_type_2d_4_2d_0E7FE105_2d_F4B7_2d_4EE4_2d_AA16_2d_9475976B003D,"##type-4-0E7FE105-F4B7-4EE4-AA16-9475976B003D")

___DEF_SYM1(1,___S__23__23_type_2d_4_2d_32EDAE4A_2d_BA00_2d_4313_2d_BC4F_2d_1F3A1F7AC8C0,"##type-4-32EDAE4A-BA00-4313-BC4F-1F3A1F7AC8C0")

___DEF_SYM1(2,___S__23__23_type_2d_4_2d_927A1AD2_2d_762A_2d_4DE6_2d_9900_2d_C22857D20E5A,"##type-4-927A1AD2-762A-4DE6-9900-C22857D20E5A")

___DEF_SYM1(3,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM1(4,___S__25_function,"%function")
___DEF_SYM1(5,___S__25_generator,"%generator")
___DEF_SYM1(6,___S__25_interpreted_2d_method,"%interpreted-method")
___DEF_SYM1(7,___S__25_method,"%method")
___DEF_SYM1(8,___S__25_primitive_2d_method,"%primitive-method")
___DEF_SYM1(9,___S__3c_function_3e_,"<function>")
___DEF_SYM1(10,___S__3c_generator_3e_,"<generator>")
___DEF_SYM1(11,___S__3c_interpreted_2d_method_3e_,"<interpreted-method>")
___DEF_SYM1(12,___S__3c_primitive_2d_method_3e_,"<primitive-method>")
___DEF_SYM1(13,___S_bindings,"bindings")
___DEF_SYM1(14,___S_body,"body")
___DEF_SYM1(15,___S_environment,"environment")
___DEF_SYM1(16,___S_fields,"fields")
___DEF_SYM1(17,___S_flags,"flags")
___DEF_SYM1(18,___S_formals,"formals")
___DEF_SYM1(19,___S_function,"function")
___DEF_SYM1(20,___S_id,"id")
___DEF_SYM1(21,___S_methods,"methods")
___DEF_SYM1(22,___S_name,"name")
___DEF_SYM1(23,___S_required_2d_count,"required-count")
___DEF_SYM1(24,___S_restarg,"restarg")
___DEF_SYM1(25,___S_signatures,"signatures")
___DEF_SYM1(26,___S_super,"super")
___DEF_SYM1(27,___S_then_2d_expression,"then-expression")
___DEF_SYM1(28,___S_type,"type")
___DEF_SYM1(29,___S_yield_2d_expression,"yield-expression")
___END_SYM1

___BEGIN_KEY1
___DEF_KEY1(0,___K_environment,"environment")
___DEF_KEY1(1,___K_name,"name")
___DEF_KEY1(2,___K_parameters,"parameters")
___DEF_KEY1(3,___K_required_2d_count,"required-count")
___DEF_KEY1(4,___K_restarg,"restarg")
___END_KEY1

___BEGIN_GLO
___DEF_GLO(0," functions")
___DEF_GLO(1,"##type-5-%primitive-method")
___DEF_GLO(2,"##type-6-%interpreted-method")
___DEF_GLO(3,"%add-method!")
___DEF_GLO(4,"%add-primitive-method!")
___DEF_GLO(5,"%function-best-method")
___DEF_GLO(6,"%function-max-method-index")
___DEF_GLO(7,"%function-method-formals")
___DEF_GLO(8,"%function-method-signatures")
___DEF_GLO(9,"%function-methods")
___DEF_GLO(10,"%function-name")
___DEF_GLO(11,"%function-nth-method")
___DEF_GLO(12,"%function-nth-method-signature")
___DEF_GLO(13,"%function?")
___DEF_GLO(14,"%generator-bindings")
___DEF_GLO(15,"%generator-environment")
___DEF_GLO(16,"%generator-then-expression")
___DEF_GLO(17,"%generator-yield-expression")
___DEF_GLO(18,"%generator?")
___DEF_GLO(19,"%interpreted-method?")
___DEF_GLO(20,"%make-function")
___DEF_GLO(21,"%make-generator")
___DEF_GLO(22,"%make-interpreted-method")
___DEF_GLO(23,"%make-primitive-method")
___DEF_GLO(24,"%method-body")
___DEF_GLO(25,"%method-environment")
___DEF_GLO(26,"%method-formals")
___DEF_GLO(27,"%method-function")
___DEF_GLO(28,"%method-name")
___DEF_GLO(29,"%method-name?")
___DEF_GLO(30,"%method-required-count")
___DEF_GLO(31,"%method-restarg")
___DEF_GLO(32,"%method-signature-matches?")
___DEF_GLO(33,"%method-signature-more-specific?")
___DEF_GLO(34,"%method?")
___DEF_GLO(35,"%next")
___DEF_GLO(36,"%primitive-method?")
___DEF_GLO(37,"%private-make-function")
___DEF_GLO(38,"%private-make-interpreted-method")
___DEF_GLO(39,"%private-make-primitive-method")
___DEF_GLO(40,"%set-function-method-formals!")
___DEF_GLO(41,"%set-function-method-signatures!")
___DEF_GLO(42,"%set-function-methods!")
___DEF_GLO(43,"%set-generator-bindings!")
___DEF_GLO(44,"%set-generator-environment!")
___DEF_GLO(45,"%type-more-specific?")
___DEF_GLO(46,"<function>")
___DEF_GLO(47,"<generator>")
___DEF_GLO(48,"<interpreted-method>")
___DEF_GLO(49,"<primitive-method>")
___DEF_GLO(50,"make-%method")
___DEF_GLO(51,"##direct-structure-ref")
___DEF_GLO(52,"##direct-structure-set!")
___DEF_GLO(53,"##make-uninterned-symbol")
___DEF_GLO(54,"##structure")
___DEF_GLO(55,"##structure-instance-of?")
___DEF_GLO(56,"##structure-ref")
___DEF_GLO(57,"##structure-type")
___DEF_GLO(58,"##type-type")
___DEF_GLO(59,"%append")
___DEF_GLO(60,"%cadr")
___DEF_GLO(61,"%car")
___DEF_GLO(62,"%cdr")
___DEF_GLO(63,"%cons")
___DEF_GLO(64,"%define-standard-type")
___DEF_GLO(65,"%eval")
___DEF_GLO(66,"%eval-sequence")
___DEF_GLO(67,"%every?")
___DEF_GLO(68,"%instance-of?")
___DEF_GLO(69,"%length")
___DEF_GLO(70,"%list")
___DEF_GLO(71,"%list-put")
___DEF_GLO(72,"%list-ref")
___DEF_GLO(73,"%nil")
___DEF_GLO(74,"%null?")
___DEF_GLO(75,"%object->bard-type")
___DEF_GLO(76,"%position")
___DEF_GLO(77,"%singleton-value")
___DEF_GLO(78,"%singleton?")
___DEF_GLO(79,"+")
___DEF_GLO(80,"-")
___DEF_GLO(81,"=")
___DEF_GLO(82,">")
___DEF_GLO(83,">=")
___DEF_GLO(84,"Anything")
___DEF_GLO(85,"eq?")
___DEF_GLO(86,"equal?")
___DEF_GLO(87,"list")
___DEF_GLO(88,"map")
___DEF_GLO(89,"string?")
___DEF_GLO(90,"symbol?")
___DEF_GLO(91,"values")
___END_GLO

___DEF_SUB_STR(___X0,26)
               ___STR8(35,35,116,121,112,101,45,53)
               ___STR8(45,37,112,114,105,109,105,116)
               ___STR8(105,118,101,45,109,101,116,104)
               ___STR2(111,100)
___DEF_SUB_VEC(___X1,3)
               ___VEC1(___REF_SYM(19,___S_function))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STRUCTURE(___X2,6)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SYM(2,___S__23__23_type_2d_4_2d_927A1AD2_2d_762A_2d_4DE6_2d_9900_2d_C22857D20E5A))
               ___VEC1(___REF_SYM(7,___S__25_method))
               ___VEC1(___REF_FIX(26))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(5))
               ___VEC0
___DEF_SUB_STRUCTURE(___X3,6)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SYM(3,___S__23__23_type_2d_5))
               ___VEC1(___REF_SYM(28,___S_type))
               ___VEC1(___REF_FIX(8))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(4))
               ___VEC0
___DEF_SUB_VEC(___X4,15)
               ___VEC1(___REF_SYM(20,___S_id))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(22,___S_name))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(17,___S_flags))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(26,___S_super))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(16,___S_fields))
               ___VEC1(___REF_FIX(5))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X5,12)
               ___VEC1(___REF_SYM(22,___S_name))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(18,___S_formals))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(24,___S_restarg))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(23,___S_required_2d_count))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STR(___X6,28)
               ___STR8(35,35,116,121,112,101,45,54)
               ___STR8(45,37,105,110,116,101,114,112)
               ___STR8(114,101,116,101,100,45,109,101)
               ___STR4(116,104,111,100)
___DEF_SUB_VEC(___X7,6)
               ___VEC1(___REF_SYM(15,___S_environment))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(14,___S_body))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X8,8)
               ___VEC1(___REF_KEY(2,___K_parameters))
               ___VEC1(___REF_ABSENT)
               ___VEC1(___REF_KEY(1,___K_name))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_KEY(3,___K_required_2d_count))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_KEY(4,___K_restarg))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X9,8)
               ___VEC1(___REF_KEY(0,___K_environment))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_KEY(1,___K_name))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_KEY(3,___K_required_2d_count))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_KEY(4,___K_restarg))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STRUCTURE(___X10,6)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SYM(0,___S__23__23_type_2d_4_2d_0E7FE105_2d_F4B7_2d_4EE4_2d_AA16_2d_9475976B003D))
               ___VEC1(___REF_SYM(4,___S__25_function))
               ___VEC1(___REF_FIX(24))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(11))
               ___VEC0
___DEF_SUB_VEC(___X11,12)
               ___VEC1(___REF_SYM(22,___S_name))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(25,___S_signatures))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(18,___S_formals))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(21,___S_methods))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X12,2)
               ___VEC1(___REF_KEY(1,___K_name))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_VEC(___X13,2)
               ___VEC1(___REF_KEY(1,___K_name))
               ___VEC1(___REF_FAL)
               ___VEC0
___DEF_SUB_STRUCTURE(___X14,6)
               ___VEC1(___REF_SUB(3))
               ___VEC1(___REF_SYM(1,___S__23__23_type_2d_4_2d_32EDAE4A_2d_BA00_2d_4313_2d_BC4F_2d_1F3A1F7AC8C0))
               ___VEC1(___REF_SYM(5,___S__25_generator))
               ___VEC1(___REF_FIX(24))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SUB(15))
               ___VEC0
___DEF_SUB_VEC(___X15,12)
               ___VEC1(___REF_SYM(29,___S_yield_2d_expression))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(27,___S_then_2d_expression))
               ___VEC1(___REF_FIX(2))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(15,___S_environment))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_FAL)
               ___VEC1(___REF_SYM(13,___S_bindings))
               ___VEC1(___REF_FIX(0))
               ___VEC1(___REF_FAL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
,___DEF_SUB(___X2)
,___DEF_SUB(___X3)
,___DEF_SUB(___X4)
,___DEF_SUB(___X5)
,___DEF_SUB(___X6)
,___DEF_SUB(___X7)
,___DEF_SUB(___X8)
,___DEF_SUB(___X9)
,___DEF_SUB(___X10)
,___DEF_SUB(___X11)
,___DEF_SUB(___X12)
,___DEF_SUB(___X13)
,___DEF_SUB(___X14)
,___DEF_SUB(___X15)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64( \
___F64V2)
#undef ___MR_ALL
#define ___MR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_functions)
___DEF_M_HLBL(___L1__20_functions)
___DEF_M_HLBL(___L2__20_functions)
___DEF_M_HLBL(___L3__20_functions)
___DEF_M_HLBL(___L4__20_functions)
___DEF_M_HLBL(___L5__20_functions)
___DEF_M_HLBL(___L6__20_functions)
___DEF_M_HLBL(___L7__20_functions)
___DEF_M_HLBL(___L8__20_functions)
___DEF_M_HLBL(___L9__20_functions)
___DEF_M_HLBL(___L10__20_functions)
___DEF_M_HLBL(___L11__20_functions)
___DEF_M_HLBL(___L12__20_functions)
___DEF_M_HLBL(___L13__20_functions)
___DEF_M_HLBL(___L14__20_functions)
___DEF_M_HLBL(___L15__20_functions)
___DEF_M_HLBL(___L16__20_functions)
___DEF_M_HLBL(___L17__20_functions)
___DEF_M_HLBL(___L18__20_functions)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L1__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L2__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L3__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L4__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L5__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L6__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L7__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L8__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L9__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L10__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L11__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L12__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L13__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L14__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L15__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L16__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L17__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L18__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L19__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL(___L20__25_method_2d_signature_2d_matches_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L1__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L2__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L3__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L4__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L5__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L6__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L7__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L8__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L9__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L10__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L11__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L12__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L13__25_type_2d_more_2d_specific_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L1__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L2__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L3__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L4__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L5__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L6__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L7__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L8__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L9__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L10__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L11__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L12__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L13__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L14__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L15__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L16__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L17__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L18__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL(___L19__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0_make_2d__25_method)
___DEF_M_HLBL(___L1_make_2d__25_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_3f_)
___DEF_M_HLBL(___L1__25_method_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_name)
___DEF_M_HLBL(___L1__25_method_2d_name)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_formals)
___DEF_M_HLBL(___L1__25_method_2d_formals)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_restarg)
___DEF_M_HLBL(___L1__25_method_2d_restarg)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_required_2d_count)
___DEF_M_HLBL(___L1__25_method_2d_required_2d_count)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_private_2d_make_2d_primitive_2d_method)
___DEF_M_HLBL(___L1__25_private_2d_make_2d_primitive_2d_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_primitive_2d_method_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_function)
___DEF_M_HLBL(___L1__25_method_2d_function)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_make_2d_primitive_2d_method)
___DEF_M_HLBL(___L1__25_make_2d_primitive_2d_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_private_2d_make_2d_interpreted_2d_method)
___DEF_M_HLBL(___L1__25_private_2d_make_2d_interpreted_2d_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_interpreted_2d_method_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_environment)
___DEF_M_HLBL(___L1__25_method_2d_environment)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_body)
___DEF_M_HLBL(___L1__25_method_2d_body)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_make_2d_interpreted_2d_method)
___DEF_M_HLBL(___L1__25_make_2d_interpreted_2d_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_method_2d_name_3f_)
___DEF_M_HLBL(___L1__25_method_2d_name_3f_)
___DEF_M_HLBL(___L2__25_method_2d_name_3f_)
___DEF_M_HLBL(___L3__25_method_2d_name_3f_)
___DEF_M_HLBL(___L4__25_method_2d_name_3f_)
___DEF_M_HLBL(___L5__25_method_2d_name_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_private_2d_make_2d_function)
___DEF_M_HLBL(___L1__25_private_2d_make_2d_function)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_name)
___DEF_M_HLBL(___L1__25_function_2d_name)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_method_2d_signatures)
___DEF_M_HLBL(___L1__25_function_2d_method_2d_signatures)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_set_2d_function_2d_method_2d_signatures_21_)
___DEF_M_HLBL(___L1__25_set_2d_function_2d_method_2d_signatures_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_method_2d_formals)
___DEF_M_HLBL(___L1__25_function_2d_method_2d_formals)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_set_2d_function_2d_method_2d_formals_21_)
___DEF_M_HLBL(___L1__25_set_2d_function_2d_method_2d_formals_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_methods)
___DEF_M_HLBL(___L1__25_function_2d_methods)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_set_2d_function_2d_methods_21_)
___DEF_M_HLBL(___L1__25_set_2d_function_2d_methods_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_make_2d_function)
___DEF_M_HLBL(___L1__25_make_2d_function)
___DEF_M_HLBL(___L2__25_make_2d_function)
___DEF_M_HLBL(___L3__25_make_2d_function)
___DEF_M_HLBL(___L4__25_make_2d_function)
___DEF_M_HLBL(___L5__25_make_2d_function)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_max_2d_method_2d_index)
___DEF_M_HLBL(___L1__25_function_2d_max_2d_method_2d_index)
___DEF_M_HLBL(___L2__25_function_2d_max_2d_method_2d_index)
___DEF_M_HLBL(___L3__25_function_2d_max_2d_method_2d_index)
___DEF_M_HLBL(___L4__25_function_2d_max_2d_method_2d_index)
___DEF_M_HLBL(___L5__25_function_2d_max_2d_method_2d_index)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_nth_2d_method_2d_signature)
___DEF_M_HLBL(___L1__25_function_2d_nth_2d_method_2d_signature)
___DEF_M_HLBL(___L2__25_function_2d_nth_2d_method_2d_signature)
___DEF_M_HLBL(___L3__25_function_2d_nth_2d_method_2d_signature)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_nth_2d_method)
___DEF_M_HLBL(___L1__25_function_2d_nth_2d_method)
___DEF_M_HLBL(___L2__25_function_2d_nth_2d_method)
___DEF_M_HLBL(___L3__25_function_2d_nth_2d_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L1__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L2__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L3__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L4__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L5__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L6__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L7__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L8__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L9__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L10__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L11__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L12__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L13__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L14__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L15__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L16__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L17__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L18__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L19__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L20__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L21__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L22__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L23__25_function_2d_best_2d_method)
___DEF_M_HLBL(___L24__25_function_2d_best_2d_method)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_add_2d_method_21_)
___DEF_M_HLBL(___L1__25_add_2d_method_21_)
___DEF_M_HLBL(___L2__25_add_2d_method_21_)
___DEF_M_HLBL(___L3__25_add_2d_method_21_)
___DEF_M_HLBL(___L4__25_add_2d_method_21_)
___DEF_M_HLBL(___L5__25_add_2d_method_21_)
___DEF_M_HLBL(___L6__25_add_2d_method_21_)
___DEF_M_HLBL(___L7__25_add_2d_method_21_)
___DEF_M_HLBL(___L8__25_add_2d_method_21_)
___DEF_M_HLBL(___L9__25_add_2d_method_21_)
___DEF_M_HLBL(___L10__25_add_2d_method_21_)
___DEF_M_HLBL(___L11__25_add_2d_method_21_)
___DEF_M_HLBL(___L12__25_add_2d_method_21_)
___DEF_M_HLBL(___L13__25_add_2d_method_21_)
___DEF_M_HLBL(___L14__25_add_2d_method_21_)
___DEF_M_HLBL(___L15__25_add_2d_method_21_)
___DEF_M_HLBL(___L16__25_add_2d_method_21_)
___DEF_M_HLBL(___L17__25_add_2d_method_21_)
___DEF_M_HLBL(___L18__25_add_2d_method_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L1__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L2__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L3__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L4__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L5__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L6__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L7__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L8__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L9__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L10__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L11__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L12__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L13__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L14__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L15__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L16__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L17__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L18__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L19__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL(___L20__25_add_2d_primitive_2d_method_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_make_2d_generator)
___DEF_M_HLBL(___L1__25_make_2d_generator)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_generator_3f_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_generator_2d_yield_2d_expression)
___DEF_M_HLBL(___L1__25_generator_2d_yield_2d_expression)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_generator_2d_then_2d_expression)
___DEF_M_HLBL(___L1__25_generator_2d_then_2d_expression)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_generator_2d_environment)
___DEF_M_HLBL(___L1__25_generator_2d_environment)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_set_2d_generator_2d_environment_21_)
___DEF_M_HLBL(___L1__25_set_2d_generator_2d_environment_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_generator_2d_bindings)
___DEF_M_HLBL(___L1__25_generator_2d_bindings)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_set_2d_generator_2d_bindings_21_)
___DEF_M_HLBL(___L1__25_set_2d_generator_2d_bindings_21_)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__25_next)
___DEF_M_HLBL(___L1__25_next)
___DEF_M_HLBL(___L2__25_next)
___DEF_M_HLBL(___L3__25_next)
___DEF_M_HLBL(___L4__25_next)
___DEF_M_HLBL(___L5__25_next)
___DEF_M_HLBL(___L6__25_next)
___DEF_M_HLBL(___L7__25_next)
___DEF_M_HLBL(___L8__25_next)
___DEF_M_HLBL(___L9__25_next)
___DEF_M_HLBL(___L10__25_next)
___DEF_M_HLBL(___L11__25_next)
___DEF_M_HLBL(___L12__25_next)
___DEF_M_HLBL(___L13__25_next)
___DEF_M_HLBL(___L14__25_next)
___DEF_M_HLBL(___L15__25_next)
___DEF_M_HLBL(___L16__25_next)
___DEF_M_HLBL(___L17__25_next)
___DEF_M_HLBL(___L18__25_next)
___DEF_M_HLBL(___L19__25_next)
___DEF_M_HLBL(___L20__25_next)
___DEF_M_HLBL(___L21__25_next)
___DEF_M_HLBL(___L22__25_next)
___DEF_M_HLBL(___L23__25_next)
___DEF_M_HLBL(___L24__25_next)
___DEF_M_HLBL(___L25__25_next)
___DEF_M_HLBL(___L26__25_next)
___DEF_M_HLBL(___L27__25_next)
___DEF_M_HLBL(___L28__25_next)
___DEF_M_HLBL(___L29__25_next)
___DEF_M_HLBL(___L30__25_next)
___DEF_M_HLBL(___L31__25_next)
___DEF_M_HLBL(___L32__25_next)
___DEF_M_HLBL(___L33__25_next)
___DEF_M_HLBL(___L34__25_next)
___DEF_M_HLBL(___L35__25_next)
___DEF_M_HLBL(___L36__25_next)
___DEF_M_HLBL(___L37__25_next)
___DEF_M_HLBL(___L38__25_next)
___DEF_M_HLBL(___L39__25_next)
___DEF_M_HLBL(___L40__25_next)
___DEF_M_HLBL(___L41__25_next)
___DEF_M_HLBL(___L42__25_next)
___DEF_M_HLBL(___L43__25_next)
___DEF_M_HLBL(___L44__25_next)
___DEF_M_HLBL(___L45__25_next)
___DEF_M_HLBL(___L46__25_next)
___DEF_M_HLBL(___L47__25_next)
___DEF_M_HLBL(___L48__25_next)
___DEF_M_HLBL(___L49__25_next)
___DEF_M_HLBL(___L50__25_next)
___DEF_M_HLBL(___L51__25_next)
___DEF_M_HLBL(___L52__25_next)
___DEF_M_HLBL(___L53__25_next)
___DEF_M_HLBL(___L54__25_next)
___DEF_M_HLBL(___L55__25_next)
___DEF_M_HLBL(___L56__25_next)
___DEF_M_HLBL(___L57__25_next)
___DEF_M_HLBL(___L58__25_next)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_functions
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_functions)
___DEF_P_HLBL(___L1__20_functions)
___DEF_P_HLBL(___L2__20_functions)
___DEF_P_HLBL(___L3__20_functions)
___DEF_P_HLBL(___L4__20_functions)
___DEF_P_HLBL(___L5__20_functions)
___DEF_P_HLBL(___L6__20_functions)
___DEF_P_HLBL(___L7__20_functions)
___DEF_P_HLBL(___L8__20_functions)
___DEF_P_HLBL(___L9__20_functions)
___DEF_P_HLBL(___L10__20_functions)
___DEF_P_HLBL(___L11__20_functions)
___DEF_P_HLBL(___L12__20_functions)
___DEF_P_HLBL(___L13__20_functions)
___DEF_P_HLBL(___L14__20_functions)
___DEF_P_HLBL(___L15__20_functions)
___DEF_P_HLBL(___L16__20_functions)
___DEF_P_HLBL(___L17__20_functions)
___DEF_P_HLBL(___L18__20_functions)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_functions)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_functions)
   ___SET_GLO(32,___G__25_method_2d_signature_2d_matches_3f_,___PRC(21))
   ___SET_GLO(45,___G__25_type_2d_more_2d_specific_3f_,___PRC(43))
   ___SET_GLO(33,___G__25_method_2d_signature_2d_more_2d_specific_3f_,___PRC(58))
   ___SET_GLO(50,___G_make_2d__25_method,___PRC(79))
   ___SET_GLO(34,___G__25_method_3f_,___PRC(82))
   ___SET_GLO(28,___G__25_method_2d_name,___PRC(85))
   ___SET_GLO(26,___G__25_method_2d_formals,___PRC(88))
   ___SET_GLO(31,___G__25_method_2d_restarg,___PRC(91))
   ___SET_GLO(30,___G__25_method_2d_required_2d_count,___PRC(94))
   ___SET_STK(1,___R0)
   ___SET_STK(5,___GLO(58,___G__23__23_type_2d_type))
   ___SET_R1(___SUB(0))
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__20_functions)
   ___JUMPGLOSAFE(___SET_NARGS(1),53,___G__23__23_make_2d_uninterned_2d_symbol)
___DEF_SLBL(2,___L2__20_functions)
   ___SET_STK(-2,___R1)
   ___SET_STK(-1,___SYM(8,___S__25_primitive_2d_method))
   ___SET_R3(___SUB(1))
   ___SET_R2(___SUB(2))
   ___SET_R1(___FIX(8L))
   ___SET_R0(___LBL(3))
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(6),___PRM(54,___G__23__23_structure))
___DEF_SLBL(3,___L3__20_functions)
   ___SET_GLO(1,___G__23__23_type_2d_5_2d__25_primitive_2d_method,___R1)
   ___SET_GLO(39,___G__25_private_2d_make_2d_primitive_2d_method,___PRC(97))
   ___SET_GLO(36,___G__25_primitive_2d_method_3f_,___PRC(100))
   ___SET_GLO(27,___G__25_method_2d_function,___PRC(102))
   ___SET_GLO(23,___G__25_make_2d_primitive_2d_method,___PRC(105))
   ___SET_R1(___FAL)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(1),23,___G__25_make_2d_primitive_2d_method)
___DEF_SLBL(4,___L4__20_functions)
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(1),57,___G__23__23_structure_2d_type)
___DEF_SLBL(5,___L5__20_functions)
   ___SET_R2(___R1)
   ___SET_R1(___SYM(12,___S__3c_primitive_2d_method_3e_))
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(2),64,___G__25_define_2d_standard_2d_type)
___DEF_SLBL(6,___L6__20_functions)
   ___SET_GLO(49,___G__3c_primitive_2d_method_3e_,___R1)
   ___SET_STK(1,___GLO(58,___G__23__23_type_2d_type))
   ___SET_R1(___SUB(6))
   ___SET_R0(___LBL(7))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(1),53,___G__23__23_make_2d_uninterned_2d_symbol)
___DEF_SLBL(7,___L7__20_functions)
   ___SET_STK(-2,___R1)
   ___SET_STK(-1,___SYM(6,___S__25_interpreted_2d_method))
   ___SET_R3(___SUB(7))
   ___SET_R2(___SUB(2))
   ___SET_R1(___FIX(8L))
   ___SET_R0(___LBL(8))
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(6),___PRM(54,___G__23__23_structure))
___DEF_SLBL(8,___L8__20_functions)
   ___SET_GLO(2,___G__23__23_type_2d_6_2d__25_interpreted_2d_method,___R1)
   ___SET_GLO(38,___G__25_private_2d_make_2d_interpreted_2d_method,___PRC(108))
   ___SET_GLO(19,___G__25_interpreted_2d_method_3f_,___PRC(111))
   ___SET_GLO(25,___G__25_method_2d_environment,___PRC(113))
   ___SET_GLO(24,___G__25_method_2d_body,___PRC(116))
   ___SET_GLO(22,___G__25_make_2d_interpreted_2d_method,___PRC(119))
   ___SET_R2(___NUL)
   ___SET_R1(___NUL)
   ___SET_R0(___LBL(9))
   ___JUMPGLOSAFE(___SET_NARGS(2),22,___G__25_make_2d_interpreted_2d_method)
___DEF_SLBL(9,___L9__20_functions)
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(1),57,___G__23__23_structure_2d_type)
___DEF_SLBL(10,___L10__20_functions)
   ___SET_R2(___R1)
   ___SET_R1(___SYM(11,___S__3c_interpreted_2d_method_3e_))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(2),64,___G__25_define_2d_standard_2d_type)
___DEF_SLBL(11,___L11__20_functions)
   ___SET_GLO(48,___G__3c_interpreted_2d_method_3e_,___R1)
   ___SET_GLO(29,___G__25_method_2d_name_3f_,___PRC(122))
   ___SET_GLO(37,___G__25_private_2d_make_2d_function,___PRC(129))
   ___SET_GLO(13,___G__25_function_3f_,___PRC(132))
   ___SET_GLO(10,___G__25_function_2d_name,___PRC(134))
   ___SET_GLO(8,___G__25_function_2d_method_2d_signatures,___PRC(137))
   ___SET_GLO(41,___G__25_set_2d_function_2d_method_2d_signatures_21_,___PRC(140))
   ___SET_GLO(7,___G__25_function_2d_method_2d_formals,___PRC(143))
   ___SET_GLO(40,___G__25_set_2d_function_2d_method_2d_formals_21_,___PRC(146))
   ___SET_GLO(9,___G__25_function_2d_methods,___PRC(149))
   ___SET_GLO(42,___G__25_set_2d_function_2d_methods_21_,___PRC(152))
   ___SET_GLO(20,___G__25_make_2d_function,___PRC(155))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(0),20,___G__25_make_2d_function)
___DEF_SLBL(12,___L12__20_functions)
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),57,___G__23__23_structure_2d_type)
___DEF_SLBL(13,___L13__20_functions)
   ___SET_R2(___R1)
   ___SET_R1(___SYM(9,___S__3c_function_3e_))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(2),64,___G__25_define_2d_standard_2d_type)
___DEF_SLBL(14,___L14__20_functions)
   ___SET_GLO(46,___G__3c_function_3e_,___R1)
   ___SET_GLO(6,___G__25_function_2d_max_2d_method_2d_index,___PRC(162))
   ___SET_GLO(12,___G__25_function_2d_nth_2d_method_2d_signature,___PRC(169))
   ___SET_GLO(11,___G__25_function_2d_nth_2d_method,___PRC(174))
   ___SET_GLO(5,___G__25_function_2d_best_2d_method,___PRC(179))
   ___SET_GLO(3,___G__25_add_2d_method_21_,___PRC(205))
   ___SET_GLO(4,___G__25_add_2d_primitive_2d_method_21_,___PRC(225))
   ___SET_GLO(21,___G__25_make_2d_generator,___PRC(247))
   ___SET_GLO(18,___G__25_generator_3f_,___PRC(250))
   ___SET_GLO(17,___G__25_generator_2d_yield_2d_expression,___PRC(252))
   ___SET_GLO(16,___G__25_generator_2d_then_2d_expression,___PRC(255))
   ___SET_GLO(15,___G__25_generator_2d_environment,___PRC(258))
   ___SET_GLO(44,___G__25_set_2d_generator_2d_environment_21_,___PRC(261))
   ___SET_GLO(14,___G__25_generator_2d_bindings,___PRC(264))
   ___SET_GLO(43,___G__25_set_2d_generator_2d_bindings_21_,___PRC(267))
   ___SET_STK(1,___NUL)
   ___SET_R3(___NUL)
   ___SET_R2(___NUL)
   ___SET_R1(___NUL)
   ___SET_R0(___LBL(15))
   ___ADJFP(1)
   ___JUMPGLOSAFE(___SET_NARGS(4),21,___G__25_make_2d_generator)
___DEF_SLBL(15,___L15__20_functions)
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(1),57,___G__23__23_structure_2d_type)
___DEF_SLBL(16,___L16__20_functions)
   ___SET_R2(___R1)
   ___SET_R1(___SYM(10,___S__3c_generator_3e_))
   ___SET_R0(___LBL(17))
   ___JUMPGLOSAFE(___SET_NARGS(2),64,___G__25_define_2d_standard_2d_type)
___DEF_SLBL(17,___L17__20_functions)
   ___SET_GLO(47,___G__3c_generator_3e_,___R1)
   ___SET_GLO(35,___G__25_next,___PRC(270))
   ___SET_R1(___VOID)
   ___POLL(18)
___DEF_SLBL(18,___L18__20_functions)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_signature_2d_matches_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 21
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64(___F64V2) \

#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L1__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L2__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L3__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L4__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L5__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L6__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L7__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L8__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L9__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L10__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L11__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L12__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L13__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L14__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L15__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L16__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L17__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L18__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L19__25_method_2d_signature_2d_matches_3f_)
___DEF_P_HLBL(___L20__25_method_2d_signature_2d_matches_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_signature_2d_matches_3f_)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FALSEP(___R2)))
   ___GOTO(___L21__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_R2(___GLO(81,___G__3d_))
   ___GOTO(___L22__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L21__25_method_2d_signature_2d_matches_3f_)
   ___SET_R2(___GLO(83,___G__3e__3d_))
___DEF_GLBL(___L22__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R3)
   ___SET_STK(4,___R2)
   ___SET_R1(___R3)
   ___SET_R0(___LBL(2))
   ___ADJFP(7)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_signature_2d_matches_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(1),69,___G__25_length)
___DEF_SLBL(2,___L2__25_method_2d_signature_2d_matches_3f_)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(3))
   ___JUMPGENSAFE(___SET_NARGS(2),___STK(-3))
___DEF_SLBL(3,___L3__25_method_2d_signature_2d_matches_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L23__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___GOTO(___L40__25_method_2d_signature_2d_matches_3f_)
___DEF_SLBL(4,___L4__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L25__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
___DEF_GLBL(___L23__25_method_2d_signature_2d_matches_3f_)
   ___SET_R1(___FAL)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L24__25_method_2d_signature_2d_matches_3f_)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(2))
___DEF_GLBL(___L25__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___EQP(___GLO(79,___G__2b_),___PRM(79,___G__2b_))))
   ___GOTO(___L39__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___STK(-3))))
   ___GOTO(___L39__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_R1(___FIXADDP(___STK(-3),___FIX(1L)))
   ___IF(___FALSEP(___R1))
   ___GOTO(___L39__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___GOTO(___L26__25_method_2d_signature_2d_matches_3f_)
___DEF_SLBL(6,___L6__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L26__25_method_2d_signature_2d_matches_3f_)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(7)
___DEF_SLBL(7,___L7__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L27__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___EQP(___GLO(83,___G__3e__3d_),___PRM(83,___G__3e__3d_))))
   ___GOTO(___L37__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L29__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R3)))
   ___GOTO(___L29__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXGE(___R3,___R1)))
   ___GOTO(___L30__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
___DEF_GLBL(___L28__25_method_2d_signature_2d_matches_3f_)
   ___SET_R1(___TRU)
   ___POLL(8)
___DEF_SLBL(8,___L8__25_method_2d_signature_2d_matches_3f_)
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_GLBL(___L29__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FLONUMP(___R1)))
   ___GOTO(___L37__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FLONUMP(___R3)))
   ___GOTO(___L37__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_F64(___F64V1,___F64UNBOX(___R3))
   ___SET_F64(___F64V2,___F64UNBOX(___R1))
   ___IF(___F64GE(___F64V1,___F64V2))
   ___GOTO(___L28__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
___DEF_GLBL(___L30__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___R3)
   ___SET_R1(___STK(3))
   ___SET_R0(___LBL(10))
   ___ADJFP(7)
   ___POLL(9)
___DEF_SLBL(9,___L9__25_method_2d_signature_2d_matches_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(2),72,___G__25_list_2d_ref)
___DEF_SLBL(10,___L10__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(-2,___R1)
   ___SET_R2(___STK(-3))
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(11))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(2),72,___G__25_list_2d_ref)
___DEF_SLBL(11,___L11__25_method_2d_signature_2d_matches_3f_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(12))
   ___SET_R1(___STK(-6))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),68,___G__25_instance_2d_of_3f_)
___DEF_SLBL(12,___L12__25_method_2d_signature_2d_matches_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L23__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___EQP(___GLO(79,___G__2b_),___PRM(79,___G__2b_))))
   ___GOTO(___L36__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___STK(-3))))
   ___GOTO(___L36__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_R1(___FIXADDP(___STK(-3),___FIX(1L)))
   ___IF(___FALSEP(___R1))
   ___GOTO(___L36__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___EQP(___GLO(83,___G__3e__3d_),___PRM(83,___G__3e__3d_)))
   ___GOTO(___L31__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___GOTO(___L35__25_method_2d_signature_2d_matches_3f_)
___DEF_SLBL(13,___L13__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___EQP(___GLO(83,___G__3e__3d_),___PRM(83,___G__3e__3d_))))
   ___GOTO(___L35__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
___DEF_GLBL(___L31__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FIXNUMP(___STK(-5))))
   ___GOTO(___L33__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L33__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FIXGE(___R1,___STK(-5))))
   ___GOTO(___L34__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
___DEF_GLBL(___L32__25_method_2d_signature_2d_matches_3f_)
   ___SET_R1(___TRU)
   ___POLL(14)
___DEF_SLBL(14,___L14__25_method_2d_signature_2d_matches_3f_)
   ___GOTO(___L24__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L33__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FLONUMP(___STK(-5))))
   ___GOTO(___L35__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___IF(___NOT(___FLONUMP(___R1)))
   ___GOTO(___L35__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_F64(___F64V1,___F64UNBOX(___R1))
   ___SET_F64(___F64V2,___F64UNBOX(___STK(-5)))
   ___IF(___F64GE(___F64V1,___F64V2))
   ___GOTO(___L32__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
___DEF_GLBL(___L34__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(-3,___R1)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(2),72,___G__25_list_2d_ref)
___DEF_SLBL(15,___L15__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(-2,___R1)
   ___SET_R2(___STK(-3))
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(16))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(2),72,___G__25_list_2d_ref)
___DEF_SLBL(16,___L16__25_method_2d_signature_2d_matches_3f_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(4))
   ___SET_R1(___STK(-6))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),68,___G__25_instance_2d_of_3f_)
___DEF_GLBL(___L35__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(-3,___R1)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(17))
   ___JUMPGLOSAFE(___SET_NARGS(2),83,___G__3e__3d_)
___DEF_SLBL(17,___L17__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L32__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_R1(___STK(-3))
   ___GOTO(___L34__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L36__25_method_2d_signature_2d_matches_3f_)
   ___SET_R1(___STK(-3))
   ___SET_R2(___FIX(1L))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(2),79,___G__2b_)
___DEF_GLBL(___L37__25_method_2d_signature_2d_matches_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___R1)
   ___SET_R1(___R3)
   ___SET_R0(___LBL(19))
   ___ADJFP(7)
   ___POLL(18)
___DEF_SLBL(18,___L18__25_method_2d_signature_2d_matches_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(2),83,___G__3e__3d_)
___DEF_SLBL(19,___L19__25_method_2d_signature_2d_matches_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L38__25_method_2d_signature_2d_matches_3f_)
   ___END_IF
   ___SET_R3(___STK(-3))
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___GOTO(___L30__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L38__25_method_2d_signature_2d_matches_3f_)
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___GOTO(___L28__25_method_2d_signature_2d_matches_3f_)
___DEF_GLBL(___L39__25_method_2d_signature_2d_matches_3f_)
   ___SET_R1(___STK(-3))
   ___SET_R2(___FIX(1L))
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(2),79,___G__2b_)
___DEF_GLBL(___L40__25_method_2d_signature_2d_matches_3f_)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___SET_R3(___FIX(0L))
   ___SET_R0(___STK(-6))
   ___ADJFP(-7)
   ___POLL(20)
___DEF_SLBL(20,___L20__25_method_2d_signature_2d_matches_3f_)
   ___GOTO(___L27__25_method_2d_signature_2d_matches_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_type_2d_more_2d_specific_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 43
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L1__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L2__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L3__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L4__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L5__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L6__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L7__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L8__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L9__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L10__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L11__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L12__25_type_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L13__25_type_2d_more_2d_specific_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_type_2d_more_2d_specific_3f_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_type_2d_more_2d_specific_3f_)
   ___IF(___NOT(___EQP(___GLO(85,___G_eq_3f_),___PRM(85,___G_eq_3f_))))
   ___GOTO(___L24__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___IF(___EQP(___R1,___R2))
   ___GOTO(___L14__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___GOTO(___L16__25_type_2d_more_2d_specific_3f_)
___DEF_SLBL(1,___L1__25_type_2d_more_2d_specific_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L15__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
___DEF_GLBL(___L14__25_type_2d_more_2d_specific_3f_)
   ___SET_R1(___TRU)
   ___POLL(2)
___DEF_SLBL(2,___L2__25_type_2d_more_2d_specific_3f_)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_GLBL(___L15__25_type_2d_more_2d_specific_3f_)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
___DEF_GLBL(___L16__25_type_2d_more_2d_specific_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___R2)
   ___SET_R0(___LBL(4))
   ___ADJFP(8)
   ___POLL(3)
___DEF_SLBL(3,___L3__25_type_2d_more_2d_specific_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(1),78,___G__25_singleton_3f_)
___DEF_SLBL(4,___L4__25_type_2d_more_2d_specific_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L17__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___SET_R1(___FAL)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_type_2d_more_2d_specific_3f_)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L17__25_type_2d_more_2d_specific_3f_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(1),78,___G__25_singleton_3f_)
___DEF_SLBL(6,___L6__25_type_2d_more_2d_specific_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L23__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___SET_STK(-6,___GLO(84,___G_Anything))
   ___IF(___NOT(___EQP(___GLO(85,___G_eq_3f_),___PRM(85,___G_eq_3f_))))
   ___GOTO(___L22__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___IF(___NOT(___EQP(___STK(-5),___STK(-6))))
   ___GOTO(___L20__25_type_2d_more_2d_specific_3f_)
   ___END_IF
   ___ADJFP(-4)
   ___GOTO(___L18__25_type_2d_more_2d_specific_3f_)
___DEF_SLBL(7,___L7__25_type_2d_more_2d_specific_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L21__25_type_2d_more_2d_specific_3f_)
   ___END_IF
___DEF_GLBL(___L18__25_type_2d_more_2d_specific_3f_)
   ___SET_R1(___TRU)
   ___POLL(8)
___DEF_SLBL(8,___L8__25_type_2d_more_2d_specific_3f_)
___DEF_GLBL(___L19__25_type_2d_more_2d_specific_3f_)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L20__25_type_2d_more_2d_specific_3f_)
   ___ADJFP(-4)
___DEF_GLBL(___L21__25_type_2d_more_2d_specific_3f_)
   ___SET_R1(___FAL)
   ___POLL(9)
___DEF_SLBL(9,___L9__25_type_2d_more_2d_specific_3f_)
   ___GOTO(___L19__25_type_2d_more_2d_specific_3f_)
___DEF_GLBL(___L22__25_type_2d_more_2d_specific_3f_)
   ___SET_R2(___STK(-6))
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(7))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),85,___G_eq_3f_)
___DEF_GLBL(___L23__25_type_2d_more_2d_specific_3f_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(1),77,___G__25_singleton_2d_value)
___DEF_SLBL(10,___L10__25_type_2d_more_2d_specific_3f_)
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),75,___G__25_object_2d__3e_bard_2d_type)
___DEF_SLBL(11,___L11__25_type_2d_more_2d_specific_3f_)
   ___SET_R2(___STK(-5))
   ___SET_R0(___STK(-7))
   ___POLL(12)
___DEF_SLBL(12,___L12__25_type_2d_more_2d_specific_3f_)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),45,___G__25_type_2d_more_2d_specific_3f_)
___DEF_GLBL(___L24__25_type_2d_more_2d_specific_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R0(___LBL(1))
   ___ADJFP(8)
   ___POLL(13)
___DEF_SLBL(13,___L13__25_type_2d_more_2d_specific_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(2),85,___G_eq_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_signature_2d_more_2d_specific_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 58
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L1__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L2__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L3__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L4__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L5__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L6__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L7__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L8__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L9__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L10__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L11__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L12__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L13__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L14__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L15__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L16__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L17__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L18__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_P_HLBL(___L19__25_method_2d_signature_2d_more_2d_specific_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___GOTO(___L20__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_SLBL(2,___L2__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R2(___R1)
   ___SET_R0(___STK(-7))
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___POLL(3)
___DEF_SLBL(3,___L3__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_GLBL(___L20__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R0(___LBL(5))
   ___ADJFP(8)
   ___POLL(4)
___DEF_SLBL(4,___L4__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(1),74,___G__25_null_3f_)
___DEF_SLBL(5,___L5__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L24__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(6))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(6,___L6__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(7,___L7__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(8))
   ___SET_R1(___STK(-4))
   ___JUMPGLOSAFE(___SET_NARGS(2),45,___G__25_type_2d_more_2d_specific_3f_)
___DEF_SLBL(8,___L8__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L21__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___END_IF
   ___GOTO(___L23__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_SLBL(9,___L9__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L22__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___END_IF
___DEF_GLBL(___L21__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R1(___FAL)
   ___POLL(10)
___DEF_SLBL(10,___L10__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L22__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_SLBL(11,___L11__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(2))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_GLBL(___L23__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_SLBL(12,___L12__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_SLBL(13,___L13__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(1),74,___G__25_null_3f_)
___DEF_SLBL(14,___L14__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L24__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(15,___L15__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(16,___L16__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(9))
   ___SET_R1(___STK(-4))
   ___JUMPGLOSAFE(___SET_NARGS(2),45,___G__25_type_2d_more_2d_specific_3f_)
___DEF_GLBL(___L24__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(17))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(1),74,___G__25_null_3f_)
___DEF_SLBL(17,___L17__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L25__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___END_IF
   ___SET_R1(___FAL)
   ___POLL(18)
___DEF_SLBL(18,___L18__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___GOTO(___L26__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_GLBL(___L25__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___SET_R1(___TRU)
   ___POLL(19)
___DEF_SLBL(19,___L19__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_GLBL(___L26__25_method_2d_signature_2d_more_2d_specific_3f_)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H_make_2d__25_method
#undef ___PH_LBL0
#define ___PH_LBL0 79
#undef ___PD_ALL
#define ___PD_ALL ___D_FP
#undef ___PR_ALL
#define ___PR_ALL ___R_FP
#undef ___PW_ALL
#define ___PW_ALL ___W_FP
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0_make_2d__25_method)
___DEF_P_HLBL(___L1_make_2d__25_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0_make_2d__25_method)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L_make_2d__25_method)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___SUB(2))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1_make_2d__25_method)
   ___JUMPPRM(___SET_NARGS(5),___PRM(54,___G__23__23_structure))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 82
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R2
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R2
#undef ___PW_ALL
#define ___PW_ALL ___W_R2
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_3f_)
___DEF_P_HLBL(___L1__25_method_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_3f_)
   ___SET_R2(___SYM(2,___S__23__23_type_2d_4_2d_927A1AD2_2d_762A_2d_4DE6_2d_9900_2d_C22857D20E5A))
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_3f_)
   ___JUMPPRM(___SET_NARGS(2),___PRM(55,___G__23__23_structure_2d_instance_2d_of_3f_))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_name
#undef ___PH_LBL0
#define ___PH_LBL0 85
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_name)
___DEF_P_HLBL(___L1__25_method_2d_name)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_name)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_name)
   ___SET_STK(1,___R1)
   ___SET_R3(___GLO(28,___G__25_method_2d_name))
   ___SET_R2(___SUB(2))
   ___SET_R1(___FIX(1L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_name)
   ___JUMPPRM(___SET_NARGS(4),___PRM(56,___G__23__23_structure_2d_ref))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_formals
#undef ___PH_LBL0
#define ___PH_LBL0 88
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_formals)
___DEF_P_HLBL(___L1__25_method_2d_formals)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_formals)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_formals)
   ___SET_STK(1,___R1)
   ___SET_R3(___GLO(26,___G__25_method_2d_formals))
   ___SET_R2(___SUB(2))
   ___SET_R1(___FIX(2L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_formals)
   ___JUMPPRM(___SET_NARGS(4),___PRM(56,___G__23__23_structure_2d_ref))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_restarg
#undef ___PH_LBL0
#define ___PH_LBL0 91
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_restarg)
___DEF_P_HLBL(___L1__25_method_2d_restarg)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_restarg)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_restarg)
   ___SET_STK(1,___R1)
   ___SET_R3(___GLO(31,___G__25_method_2d_restarg))
   ___SET_R2(___SUB(2))
   ___SET_R1(___FIX(3L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_restarg)
   ___JUMPPRM(___SET_NARGS(4),___PRM(56,___G__23__23_structure_2d_ref))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_required_2d_count
#undef ___PH_LBL0
#define ___PH_LBL0 94
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_required_2d_count)
___DEF_P_HLBL(___L1__25_method_2d_required_2d_count)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_required_2d_count)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_required_2d_count)
   ___SET_STK(1,___R1)
   ___SET_R3(___GLO(30,___G__25_method_2d_required_2d_count))
   ___SET_R2(___SUB(2))
   ___SET_R1(___FIX(4L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_required_2d_count)
   ___JUMPPRM(___SET_NARGS(4),___PRM(56,___G__23__23_structure_2d_ref))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_private_2d_make_2d_primitive_2d_method
#undef ___PH_LBL0
#define ___PH_LBL0 97
#undef ___PD_ALL
#define ___PD_ALL ___D_FP
#undef ___PR_ALL
#define ___PR_ALL ___R_FP
#undef ___PW_ALL
#define ___PW_ALL ___W_FP
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_private_2d_make_2d_primitive_2d_method)
___DEF_P_HLBL(___L1__25_private_2d_make_2d_primitive_2d_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_private_2d_make_2d_primitive_2d_method)
   ___IF_NARGS_EQ(5,___NOTHING)
   ___WRONG_NARGS(0,5,0,0)
___DEF_GLBL(___L__25_private_2d_make_2d_primitive_2d_method)
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___GLO(1,___G__23__23_type_2d_5_2d__25_primitive_2d_method))
   ___SET_STK(2,___STK(0))
   ___SET_STK(0,___STK(1))
   ___SET_STK(1,___STK(2))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_private_2d_make_2d_primitive_2d_method)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(6),___PRM(54,___G__23__23_structure))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_primitive_2d_method_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 100
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1 ___D_R2
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1 ___R_R2
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_primitive_2d_method_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_primitive_2d_method_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_primitive_2d_method_3f_)
   ___SET_R2(___TYPEID(___GLO(1,___G__23__23_type_2d_5_2d__25_primitive_2d_method)))
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___R2)))
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_function
#undef ___PH_LBL0
#define ___PH_LBL0 102
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_function)
___DEF_P_HLBL(___L1__25_method_2d_function)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_function)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_function)
   ___SET_STK(1,___GLO(1,___G__23__23_type_2d_5_2d__25_primitive_2d_method))
   ___SET_STK(2,___GLO(27,___G__25_method_2d_function))
   ___SET_R2(___TYPEID(___STK(1)))
   ___ADJFP(2)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_method_2d_function)
   ___END_IF
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___R1)
   ___SET_R3(___STK(0))
   ___SET_R2(___STK(1))
   ___SET_R1(___FIX(5L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_function)
   ___ADJFP(-2)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_method_2d_function)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(5L),___STK(-1),___STK(0)))
   ___ADJFP(-2)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_make_2d_primitive_2d_method
#undef ___PH_LBL0
#define ___PH_LBL0 105
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_make_2d_primitive_2d_method)
___DEF_P_HLBL(___L1__25_make_2d_primitive_2d_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_make_2d_primitive_2d_method)
   ___IF_NARGS_EQ(1,___PUSH(___R1) ___PUSH(___ABSENT) ___SET_R1(___FAL) ___SET_R2(___FIX(0L)) ___SET_R3(
___FAL))
   ___GET_KEY(0,1,0,4,___SUB(8))
___DEF_GLBL(___L__25_make_2d_primitive_2d_method)
   ___IF(___NOT(___EQP(___STK(0),___ABSENT)))
   ___GOTO(___L2__25_make_2d_primitive_2d_method)
   ___END_IF
   ___SET_R4(___GLO(73,___G__25_nil))
   ___GOTO(___L3__25_make_2d_primitive_2d_method)
___DEF_GLBL(___L2__25_make_2d_primitive_2d_method)
   ___SET_R4(___STK(0))
___DEF_GLBL(___L3__25_make_2d_primitive_2d_method)
   ___SET_STK(0,___STK(-1))
   ___SET_STK(-1,___R1)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R4)
   ___SET_STK(2,___R3)
   ___SET_R3(___STK(1))
   ___SET_R1(___STK(2))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_make_2d_primitive_2d_method)
   ___ADJFP(-2)
   ___JUMPGLOSAFE(___SET_NARGS(5),39,___G__25_private_2d_make_2d_primitive_2d_method)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_private_2d_make_2d_interpreted_2d_method
#undef ___PH_LBL0
#define ___PH_LBL0 108
#undef ___PD_ALL
#define ___PD_ALL ___D_FP
#undef ___PR_ALL
#define ___PR_ALL ___R_FP
#undef ___PW_ALL
#define ___PW_ALL ___W_FP
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_private_2d_make_2d_interpreted_2d_method)
___DEF_P_HLBL(___L1__25_private_2d_make_2d_interpreted_2d_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_private_2d_make_2d_interpreted_2d_method)
   ___IF_NARGS_EQ(6,___NOTHING)
   ___WRONG_NARGS(0,6,0,0)
___DEF_GLBL(___L__25_private_2d_make_2d_interpreted_2d_method)
   ___SET_STK(1,___STK(-2))
   ___SET_STK(-2,___GLO(2,___G__23__23_type_2d_6_2d__25_interpreted_2d_method))
   ___SET_STK(2,___STK(-1))
   ___SET_STK(-1,___STK(1))
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___STK(2))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_private_2d_make_2d_interpreted_2d_method)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(7),___PRM(54,___G__23__23_structure))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_interpreted_2d_method_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 111
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1 ___D_R2
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1 ___R_R2
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R2
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_interpreted_2d_method_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_interpreted_2d_method_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_interpreted_2d_method_3f_)
   ___SET_R2(___TYPEID(___GLO(2,___G__23__23_type_2d_6_2d__25_interpreted_2d_method)))
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___R2)))
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_environment
#undef ___PH_LBL0
#define ___PH_LBL0 113
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_environment)
___DEF_P_HLBL(___L1__25_method_2d_environment)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_environment)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_environment)
   ___SET_STK(1,___GLO(2,___G__23__23_type_2d_6_2d__25_interpreted_2d_method))
   ___SET_STK(2,___GLO(25,___G__25_method_2d_environment))
   ___SET_R2(___TYPEID(___STK(1)))
   ___ADJFP(2)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_method_2d_environment)
   ___END_IF
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___R1)
   ___SET_R3(___STK(0))
   ___SET_R2(___STK(1))
   ___SET_R1(___FIX(5L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_environment)
   ___ADJFP(-2)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_method_2d_environment)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(5L),___STK(-1),___STK(0)))
   ___ADJFP(-2)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_body
#undef ___PH_LBL0
#define ___PH_LBL0 116
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_body)
___DEF_P_HLBL(___L1__25_method_2d_body)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_body)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_body)
   ___SET_STK(1,___GLO(2,___G__23__23_type_2d_6_2d__25_interpreted_2d_method))
   ___SET_STK(2,___GLO(24,___G__25_method_2d_body))
   ___SET_R2(___TYPEID(___STK(1)))
   ___ADJFP(2)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_method_2d_body)
   ___END_IF
   ___SET_STK(1,___STK(-1))
   ___SET_STK(-1,___R1)
   ___SET_R3(___STK(0))
   ___SET_R2(___STK(1))
   ___SET_R1(___FIX(6L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_method_2d_body)
   ___ADJFP(-2)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_method_2d_body)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(6L),___STK(-1),___STK(0)))
   ___ADJFP(-2)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_make_2d_interpreted_2d_method
#undef ___PH_LBL0
#define ___PH_LBL0 119
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_make_2d_interpreted_2d_method)
___DEF_P_HLBL(___L1__25_make_2d_interpreted_2d_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_make_2d_interpreted_2d_method)
   ___IF_NARGS_EQ(2,___PUSH(___R1) ___PUSH(___R2) ___PUSH(___NUL) ___SET_R1(___FAL) ___SET_R2(
___FIX(0L)) ___SET_R3(___FAL))
   ___GET_KEY(0,2,0,4,___SUB(9))
___DEF_GLBL(___L__25_make_2d_interpreted_2d_method)
   ___SET_STK(1,___STK(-2))
   ___SET_STK(-2,___R1)
   ___SET_STK(2,___STK(-1))
   ___SET_STK(-1,___STK(1))
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R3)
   ___SET_R3(___STK(2))
   ___SET_STK(2,___R2)
   ___SET_R2(___STK(1))
   ___SET_R1(___STK(2))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_make_2d_interpreted_2d_method)
   ___ADJFP(-2)
   ___JUMPGLOSAFE(___SET_NARGS(6),38,___G__25_private_2d_make_2d_interpreted_2d_method)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_method_2d_name_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 122
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_method_2d_name_3f_)
___DEF_P_HLBL(___L1__25_method_2d_name_3f_)
___DEF_P_HLBL(___L2__25_method_2d_name_3f_)
___DEF_P_HLBL(___L3__25_method_2d_name_3f_)
___DEF_P_HLBL(___L4__25_method_2d_name_3f_)
___DEF_P_HLBL(___L5__25_method_2d_name_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_method_2d_name_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_method_2d_name_3f_)
   ___IF(___NOT(___EQP(___GLO(90,___G_symbol_3f_),___PRM(90,___G_symbol_3f_))))
   ___GOTO(___L10__25_method_2d_name_3f_)
   ___END_IF
   ___SET_R2(___BOOLEAN(___SYMBOLP(___R1)))
   ___GOTO(___L6__25_method_2d_name_3f_)
___DEF_SLBL(1,___L1__25_method_2d_name_3f_)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___ADJFP(-8)
___DEF_GLBL(___L6__25_method_2d_name_3f_)
   ___SET_STK(1,___R1)
   ___SET_R1(___R2)
   ___ADJFP(1)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L9__25_method_2d_name_3f_)
   ___END_IF
   ___IF(___NOT(___EQP(___GLO(89,___G_string_3f_),___PRM(89,___G_string_3f_))))
   ___GOTO(___L8__25_method_2d_name_3f_)
   ___END_IF
   ___SET_R1(___BOOLEAN(___STRINGP(___STK(0))))
   ___POLL(2)
___DEF_SLBL(2,___L2__25_method_2d_name_3f_)
___DEF_GLBL(___L7__25_method_2d_name_3f_)
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_GLBL(___L8__25_method_2d_name_3f_)
   ___SET_R1(___STK(0))
   ___POLL(3)
___DEF_SLBL(3,___L3__25_method_2d_name_3f_)
   ___ADJFP(-1)
   ___JUMPGLOSAFE(___SET_NARGS(1),89,___G_string_3f_)
___DEF_GLBL(___L9__25_method_2d_name_3f_)
   ___POLL(4)
___DEF_SLBL(4,___L4__25_method_2d_name_3f_)
   ___GOTO(___L7__25_method_2d_name_3f_)
___DEF_GLBL(___L10__25_method_2d_name_3f_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(1))
   ___ADJFP(8)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_method_2d_name_3f_)
   ___JUMPGLOSAFE(___SET_NARGS(1),90,___G_symbol_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_private_2d_make_2d_function
#undef ___PH_LBL0
#define ___PH_LBL0 129
#undef ___PD_ALL
#define ___PD_ALL ___D_FP
#undef ___PR_ALL
#define ___PR_ALL ___R_FP
#undef ___PW_ALL
#define ___PW_ALL ___W_FP
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_private_2d_make_2d_function)
___DEF_P_HLBL(___L1__25_private_2d_make_2d_function)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_private_2d_make_2d_function)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L__25_private_2d_make_2d_function)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___SUB(10))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_private_2d_make_2d_function)
   ___JUMPPRM(___SET_NARGS(5),___PRM(54,___G__23__23_structure))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 132
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_function_3f_)
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___SYM(0,___S__23__23_type_2d_4_2d_0E7FE105_2d_F4B7_2d_4EE4_2d_AA16_2d_9475976B003D))))
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_name
#undef ___PH_LBL0
#define ___PH_LBL0 134
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_name)
___DEF_P_HLBL(___L1__25_function_2d_name)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_name)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_function_2d_name)
   ___SET_STK(1,___GLO(10,___G__25_function_2d_name))
   ___SET_R2(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_function_2d_name)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(1L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_name)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_function_2d_name)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(10),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_method_2d_signatures
#undef ___PH_LBL0
#define ___PH_LBL0 137
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_method_2d_signatures)
___DEF_P_HLBL(___L1__25_function_2d_method_2d_signatures)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_method_2d_signatures)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_function_2d_method_2d_signatures)
   ___SET_STK(1,___GLO(8,___G__25_function_2d_method_2d_signatures))
   ___SET_R2(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_function_2d_method_2d_signatures)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(2L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_method_2d_signatures)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_function_2d_method_2d_signatures)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(10),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_set_2d_function_2d_method_2d_signatures_21_
#undef ___PH_LBL0
#define ___PH_LBL0 140
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_set_2d_function_2d_method_2d_signatures_21_)
___DEF_P_HLBL(___L1__25_set_2d_function_2d_method_2d_signatures_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_set_2d_function_2d_method_2d_signatures_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_set_2d_function_2d_method_2d_signatures_21_)
   ___SET_STK(1,___GLO(41,___G__25_set_2d_function_2d_method_2d_signatures_21_))
   ___SET_R3(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R3))
   ___GOTO(___L2__25_set_2d_function_2d_method_2d_signatures_21_)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___R2)
   ___SET_R3(___STK(2))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(2L))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_set_2d_function_2d_method_2d_signatures_21_)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(5),___PRM(52,___G__23__23_direct_2d_structure_2d_set_21_))
___DEF_GLBL(___L2__25_set_2d_function_2d_method_2d_signatures_21_)
   ___UNCHECKEDSTRUCTURESET(___R1,___R2,___FIX(2L),___SUB(10),___STK(0))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_method_2d_formals
#undef ___PH_LBL0
#define ___PH_LBL0 143
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_method_2d_formals)
___DEF_P_HLBL(___L1__25_function_2d_method_2d_formals)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_method_2d_formals)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_function_2d_method_2d_formals)
   ___SET_STK(1,___GLO(7,___G__25_function_2d_method_2d_formals))
   ___SET_R2(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_function_2d_method_2d_formals)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(3L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_method_2d_formals)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_function_2d_method_2d_formals)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(10),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_set_2d_function_2d_method_2d_formals_21_
#undef ___PH_LBL0
#define ___PH_LBL0 146
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_set_2d_function_2d_method_2d_formals_21_)
___DEF_P_HLBL(___L1__25_set_2d_function_2d_method_2d_formals_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_set_2d_function_2d_method_2d_formals_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_set_2d_function_2d_method_2d_formals_21_)
   ___SET_STK(1,___GLO(40,___G__25_set_2d_function_2d_method_2d_formals_21_))
   ___SET_R3(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R3))
   ___GOTO(___L2__25_set_2d_function_2d_method_2d_formals_21_)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___R2)
   ___SET_R3(___STK(2))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(3L))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_set_2d_function_2d_method_2d_formals_21_)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(5),___PRM(52,___G__23__23_direct_2d_structure_2d_set_21_))
___DEF_GLBL(___L2__25_set_2d_function_2d_method_2d_formals_21_)
   ___UNCHECKEDSTRUCTURESET(___R1,___R2,___FIX(3L),___SUB(10),___STK(0))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_methods
#undef ___PH_LBL0
#define ___PH_LBL0 149
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_methods)
___DEF_P_HLBL(___L1__25_function_2d_methods)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_methods)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_function_2d_methods)
   ___SET_STK(1,___GLO(9,___G__25_function_2d_methods))
   ___SET_R2(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_function_2d_methods)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(4L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_methods)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_function_2d_methods)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(10),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_set_2d_function_2d_methods_21_
#undef ___PH_LBL0
#define ___PH_LBL0 152
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_set_2d_function_2d_methods_21_)
___DEF_P_HLBL(___L1__25_set_2d_function_2d_methods_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_set_2d_function_2d_methods_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_set_2d_function_2d_methods_21_)
   ___SET_STK(1,___GLO(42,___G__25_set_2d_function_2d_methods_21_))
   ___SET_R3(___TYPEID(___SUB(10)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R3))
   ___GOTO(___L2__25_set_2d_function_2d_methods_21_)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___R2)
   ___SET_R3(___STK(2))
   ___SET_R2(___SUB(10))
   ___SET_R1(___FIX(4L))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_set_2d_function_2d_methods_21_)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(5),___PRM(52,___G__23__23_direct_2d_structure_2d_set_21_))
___DEF_GLBL(___L2__25_set_2d_function_2d_methods_21_)
   ___UNCHECKEDSTRUCTURESET(___R1,___R2,___FIX(4L),___SUB(10),___STK(0))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_make_2d_function
#undef ___PH_LBL0
#define ___PH_LBL0 155
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_make_2d_function)
___DEF_P_HLBL(___L1__25_make_2d_function)
___DEF_P_HLBL(___L2__25_make_2d_function)
___DEF_P_HLBL(___L3__25_make_2d_function)
___DEF_P_HLBL(___L4__25_make_2d_function)
___DEF_P_HLBL(___L5__25_make_2d_function)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_make_2d_function)
   ___IF_NARGS_EQ(0,___SET_R1(___FAL))
   ___GET_KEY(0,0,0,1,___SUB(12))
___DEF_GLBL(___L__25_make_2d_function)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R0)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_make_2d_function)
   ___JUMPGLOSAFE(___SET_NARGS(0),70,___G__25_list)
___DEF_SLBL(2,___L2__25_make_2d_function)
   ___SET_STK(-5,___R1)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(0),70,___G__25_list)
___DEF_SLBL(3,___L3__25_make_2d_function)
   ___SET_STK(-4,___R1)
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(0),70,___G__25_list)
___DEF_SLBL(4,___L4__25_make_2d_function)
   ___SET_R3(___R1)
   ___SET_R0(___STK(-6))
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-5))
   ___POLL(5)
___DEF_SLBL(5,___L5__25_make_2d_function)
   ___ADJFP(-7)
   ___JUMPGLOSAFE(___SET_NARGS(4),37,___G__25_private_2d_make_2d_function)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_max_2d_method_2d_index
#undef ___PH_LBL0
#define ___PH_LBL0 162
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_max_2d_method_2d_index)
___DEF_P_HLBL(___L1__25_function_2d_max_2d_method_2d_index)
___DEF_P_HLBL(___L2__25_function_2d_max_2d_method_2d_index)
___DEF_P_HLBL(___L3__25_function_2d_max_2d_method_2d_index)
___DEF_P_HLBL(___L4__25_function_2d_max_2d_method_2d_index)
___DEF_P_HLBL(___L5__25_function_2d_max_2d_method_2d_index)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_max_2d_method_2d_index)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_function_2d_max_2d_method_2d_index)
   ___SET_STK(1,___R0)
   ___SET_R0(___LBL(2))
   ___ADJFP(4)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_max_2d_method_2d_index)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(2,___L2__25_function_2d_max_2d_method_2d_index)
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(1),69,___G__25_length)
___DEF_SLBL(3,___L3__25_function_2d_max_2d_method_2d_index)
   ___IF(___NOT(___EQP(___GLO(80,___G__2d_),___PRM(80,___G__2d_))))
   ___GOTO(___L6__25_function_2d_max_2d_method_2d_index)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L6__25_function_2d_max_2d_method_2d_index)
   ___END_IF
   ___SET_R2(___FIXSUBP(___R1,___FIX(1L)))
   ___IF(___NOT(___FALSEP(___R2)))
   ___GOTO(___L7__25_function_2d_max_2d_method_2d_index)
   ___END_IF
___DEF_GLBL(___L6__25_function_2d_max_2d_method_2d_index)
   ___SET_R2(___FIX(1L))
   ___SET_R0(___STK(-3))
   ___POLL(4)
___DEF_SLBL(4,___L4__25_function_2d_max_2d_method_2d_index)
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),80,___G__2d_)
___DEF_GLBL(___L7__25_function_2d_max_2d_method_2d_index)
   ___SET_R1(___R2)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_function_2d_max_2d_method_2d_index)
   ___ADJFP(-4)
   ___JUMPPRM(___NOTHING,___STK(1))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_nth_2d_method_2d_signature
#undef ___PH_LBL0
#define ___PH_LBL0 169
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_nth_2d_method_2d_signature)
___DEF_P_HLBL(___L1__25_function_2d_nth_2d_method_2d_signature)
___DEF_P_HLBL(___L2__25_function_2d_nth_2d_method_2d_signature)
___DEF_P_HLBL(___L3__25_function_2d_nth_2d_method_2d_signature)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_nth_2d_method_2d_signature)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_function_2d_nth_2d_method_2d_signature)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_nth_2d_method_2d_signature)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(2,___L2__25_function_2d_nth_2d_method_2d_signature)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(3)
___DEF_SLBL(3,___L3__25_function_2d_nth_2d_method_2d_signature)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),72,___G__25_list_2d_ref)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_nth_2d_method
#undef ___PH_LBL0
#define ___PH_LBL0 174
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R2 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R2 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0 ___W_R2 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_nth_2d_method)
___DEF_P_HLBL(___L1__25_function_2d_nth_2d_method)
___DEF_P_HLBL(___L2__25_function_2d_nth_2d_method)
___DEF_P_HLBL(___L3__25_function_2d_nth_2d_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_nth_2d_method)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_function_2d_nth_2d_method)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_nth_2d_method)
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_function_2d_methods)
___DEF_SLBL(2,___L2__25_function_2d_nth_2d_method)
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(3)
___DEF_SLBL(3,___L3__25_function_2d_nth_2d_method)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),72,___G__25_list_2d_ref)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_function_2d_best_2d_method
#undef ___PH_LBL0
#define ___PH_LBL0 179
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64( \
___F64V2)
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L1__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L2__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L3__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L4__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L5__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L6__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L7__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L8__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L9__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L10__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L11__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L12__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L13__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L14__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L15__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L16__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L17__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L18__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L19__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L20__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L21__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L22__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L23__25_function_2d_best_2d_method)
___DEF_P_HLBL(___L24__25_function_2d_best_2d_method)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_function_2d_best_2d_method)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_function_2d_best_2d_method)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_function_2d_best_2d_method)
   ___JUMPGLOSAFE(___SET_NARGS(1),6,___G__25_function_2d_max_2d_method_2d_index)
___DEF_SLBL(2,___L2__25_function_2d_best_2d_method)
   ___SET_STK(-4,___STK(-7))
   ___SET_STK(-7,___STK(-6))
   ___SET_STK(-6,___STK(-5))
   ___SET_STK(-5,___R1)
   ___SET_R3(___FAL)
   ___SET_R2(___FAL)
   ___SET_R1(___FIX(0L))
   ___SET_R0(___STK(-4))
   ___ADJFP(-5)
   ___POLL(3)
___DEF_SLBL(3,___L3__25_function_2d_best_2d_method)
   ___GOTO(___L26__25_function_2d_best_2d_method)
___DEF_SLBL(4,___L4__25_function_2d_best_2d_method)
___DEF_GLBL(___L25__25_function_2d_best_2d_method)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-7))
   ___SET_R0(___STK(-8))
   ___ADJFP(-9)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_function_2d_best_2d_method)
___DEF_GLBL(___L26__25_function_2d_best_2d_method)
   ___IF(___NOT(___EQP(___GLO(82,___G__3e_),___PRM(82,___G__3e_))))
   ___GOTO(___L42__25_function_2d_best_2d_method)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___STK(0))))
   ___GOTO(___L33__25_function_2d_best_2d_method)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___R1)))
   ___GOTO(___L33__25_function_2d_best_2d_method)
   ___END_IF
   ___IF(___NOT(___FIXGT(___R1,___STK(0))))
   ___GOTO(___L34__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L27__25_function_2d_best_2d_method)
   ___IF(___NOT(___FALSEP(___R3)))
   ___GOTO(___L30__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L28__25_function_2d_best_2d_method)
   ___IF(___NOT(___EQP(___GLO(91,___G_values),___PRM(91,___G_values))))
   ___GOTO(___L29__25_function_2d_best_2d_method)
   ___END_IF
   ___BEGIN_ALLOC_VALUES(2)
   ___ADD_VALUES_ELEM(0,___FAL)
   ___ADD_VALUES_ELEM(1,___FAL)
   ___END_ALLOC_VALUES(2)
   ___SET_R1(___GET_VALUES(2))
   ___CHECK_HEAP(6,4096)
___DEF_SLBL(6,___L6__25_function_2d_best_2d_method)
   ___POLL(7)
___DEF_SLBL(7,___L7__25_function_2d_best_2d_method)
   ___ADJFP(-3)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_GLBL(___L29__25_function_2d_best_2d_method)
   ___SET_R2(___FAL)
   ___SET_R1(___FAL)
   ___POLL(8)
___DEF_SLBL(8,___L8__25_function_2d_best_2d_method)
   ___ADJFP(-3)
   ___JUMPGLOSAFE(___SET_NARGS(2),91,___G_values)
___DEF_SLBL(9,___L9__25_function_2d_best_2d_method)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L32__25_function_2d_best_2d_method)
   ___END_IF
   ___SET_R3(___STK(-5))
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-8))
   ___ADJFP(-9)
   ___IF(___FALSEP(___R3))
   ___GOTO(___L28__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L30__25_function_2d_best_2d_method)
   ___SET_STK(-1,___R0)
   ___SET_STK(0,___R3)
   ___SET_R1(___STK(-2))
   ___SET_R0(___LBL(11))
   ___ADJFP(5)
   ___POLL(10)
___DEF_SLBL(10,___L10__25_function_2d_best_2d_method)
   ___JUMPGLOSAFE(___SET_NARGS(2),11,___G__25_function_2d_nth_2d_method)
___DEF_SLBL(11,___L11__25_function_2d_best_2d_method)
   ___IF(___NOT(___EQP(___GLO(91,___G_values),___PRM(91,___G_values))))
   ___GOTO(___L31__25_function_2d_best_2d_method)
   ___END_IF
   ___BEGIN_ALLOC_VALUES(2)
   ___ADD_VALUES_ELEM(0,___R1)
   ___ADD_VALUES_ELEM(1,___STK(-5))
   ___END_ALLOC_VALUES(2)
   ___SET_R1(___GET_VALUES(2))
   ___CHECK_HEAP(12,4096)
___DEF_SLBL(12,___L12__25_function_2d_best_2d_method)
   ___POLL(13)
___DEF_SLBL(13,___L13__25_function_2d_best_2d_method)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(2))
___DEF_GLBL(___L31__25_function_2d_best_2d_method)
   ___SET_R2(___STK(-5))
   ___SET_R0(___STK(-6))
   ___POLL(14)
___DEF_SLBL(14,___L14__25_function_2d_best_2d_method)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),91,___G_values)
___DEF_GLBL(___L32__25_function_2d_best_2d_method)
   ___SET_R3(___STK(-5))
   ___SET_R2(___STK(-6))
   ___SET_R1(___STK(-7))
   ___SET_R0(___STK(-8))
   ___ADJFP(-9)
   ___GOTO(___L34__25_function_2d_best_2d_method)
___DEF_GLBL(___L33__25_function_2d_best_2d_method)
   ___IF(___NOT(___FLONUMP(___STK(0))))
   ___GOTO(___L42__25_function_2d_best_2d_method)
   ___END_IF
   ___IF(___NOT(___FLONUMP(___R1)))
   ___GOTO(___L42__25_function_2d_best_2d_method)
   ___END_IF
   ___SET_F64(___F64V1,___F64UNBOX(___R1))
   ___SET_F64(___F64V2,___F64UNBOX(___STK(0)))
   ___IF(___F64GT(___F64V1,___F64V2))
   ___GOTO(___L27__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L34__25_function_2d_best_2d_method)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-2))
   ___SET_R0(___LBL(16))
   ___ADJFP(9)
   ___POLL(15)
___DEF_SLBL(15,___L15__25_function_2d_best_2d_method)
   ___JUMPGLOSAFE(___SET_NARGS(2),12,___G__25_function_2d_nth_2d_method_2d_signature)
___DEF_SLBL(16,___L16__25_function_2d_best_2d_method)
   ___SET_STK(-4,___R1)
   ___SET_R2(___STK(-7))
   ___SET_R1(___STK(-11))
   ___SET_R0(___LBL(17))
   ___JUMPGLOSAFE(___SET_NARGS(2),11,___G__25_function_2d_nth_2d_method)
___DEF_SLBL(17,___L17__25_function_2d_best_2d_method)
   ___SET_STK(-3,___R1)
   ___SET_R0(___LBL(18))
   ___JUMPGLOSAFE(___SET_NARGS(1),30,___G__25_method_2d_required_2d_count)
___DEF_SLBL(18,___L18__25_function_2d_best_2d_method)
   ___SET_STK(-2,___R1)
   ___SET_R1(___STK(-3))
   ___SET_R0(___LBL(19))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(1),31,___G__25_method_2d_restarg)
___DEF_SLBL(19,___L19__25_function_2d_best_2d_method)
   ___SET_STK(-3,___STK(-8))
   ___SET_R3(___STK(-14))
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(20))
   ___ADJFP(-3)
   ___JUMPGLOSAFE(___SET_NARGS(4),32,___G__25_method_2d_signature_2d_matches_3f_)
___DEF_SLBL(20,___L20__25_function_2d_best_2d_method)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L35__25_function_2d_best_2d_method)
   ___END_IF
   ___GOTO(___L38__25_function_2d_best_2d_method)
___DEF_SLBL(21,___L21__25_function_2d_best_2d_method)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L39__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L35__25_function_2d_best_2d_method)
   ___IF(___NOT(___EQP(___GLO(79,___G__2b_),___PRM(79,___G__2b_))))
   ___GOTO(___L37__25_function_2d_best_2d_method)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___STK(-7))))
   ___GOTO(___L37__25_function_2d_best_2d_method)
   ___END_IF
   ___SET_R1(___FIXADDP(___FIX(1L),___STK(-7)))
   ___IF(___FALSEP(___R1))
   ___GOTO(___L37__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L36__25_function_2d_best_2d_method)
   ___SET_R3(___STK(-5))
   ___SET_R2(___STK(-6))
   ___SET_R0(___STK(-8))
   ___ADJFP(-9)
   ___POLL(22)
___DEF_SLBL(22,___L22__25_function_2d_best_2d_method)
   ___GOTO(___L26__25_function_2d_best_2d_method)
___DEF_GLBL(___L37__25_function_2d_best_2d_method)
   ___SET_R2(___STK(-7))
   ___SET_R1(___FIX(1L))
   ___SET_R0(___LBL(23))
   ___JUMPGLOSAFE(___SET_NARGS(2),79,___G__2b_)
___DEF_SLBL(23,___L23__25_function_2d_best_2d_method)
   ___GOTO(___L36__25_function_2d_best_2d_method)
___DEF_GLBL(___L38__25_function_2d_best_2d_method)
   ___IF(___NOT(___FALSEP(___STK(-5))))
   ___GOTO(___L41__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L39__25_function_2d_best_2d_method)
   ___IF(___NOT(___EQP(___GLO(79,___G__2b_),___PRM(79,___G__2b_))))
   ___GOTO(___L40__25_function_2d_best_2d_method)
   ___END_IF
   ___IF(___NOT(___FIXNUMP(___STK(-7))))
   ___GOTO(___L40__25_function_2d_best_2d_method)
   ___END_IF
   ___SET_R1(___FIXADDP(___FIX(1L),___STK(-7)))
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L25__25_function_2d_best_2d_method)
   ___END_IF
___DEF_GLBL(___L40__25_function_2d_best_2d_method)
   ___SET_R2(___STK(-7))
   ___SET_R1(___FIX(1L))
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(2),79,___G__2b_)
___DEF_GLBL(___L41__25_function_2d_best_2d_method)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(21))
   ___JUMPGLOSAFE(___SET_NARGS(2),33,___G__25_method_2d_signature_2d_more_2d_specific_3f_)
___DEF_GLBL(___L42__25_function_2d_best_2d_method)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___STK(0))
   ___SET_R0(___LBL(9))
   ___ADJFP(9)
   ___POLL(24)
___DEF_SLBL(24,___L24__25_function_2d_best_2d_method)
   ___JUMPGLOSAFE(___SET_NARGS(2),82,___G__3e_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_add_2d_method_21_
#undef ___PH_LBL0
#define ___PH_LBL0 205
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_add_2d_method_21_)
___DEF_P_HLBL(___L1__25_add_2d_method_21_)
___DEF_P_HLBL(___L2__25_add_2d_method_21_)
___DEF_P_HLBL(___L3__25_add_2d_method_21_)
___DEF_P_HLBL(___L4__25_add_2d_method_21_)
___DEF_P_HLBL(___L5__25_add_2d_method_21_)
___DEF_P_HLBL(___L6__25_add_2d_method_21_)
___DEF_P_HLBL(___L7__25_add_2d_method_21_)
___DEF_P_HLBL(___L8__25_add_2d_method_21_)
___DEF_P_HLBL(___L9__25_add_2d_method_21_)
___DEF_P_HLBL(___L10__25_add_2d_method_21_)
___DEF_P_HLBL(___L11__25_add_2d_method_21_)
___DEF_P_HLBL(___L12__25_add_2d_method_21_)
___DEF_P_HLBL(___L13__25_add_2d_method_21_)
___DEF_P_HLBL(___L14__25_add_2d_method_21_)
___DEF_P_HLBL(___L15__25_add_2d_method_21_)
___DEF_P_HLBL(___L16__25_add_2d_method_21_)
___DEF_P_HLBL(___L17__25_add_2d_method_21_)
___DEF_P_HLBL(___L18__25_add_2d_method_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_add_2d_method_21_)
   ___IF_NARGS_EQ(3,___NOTHING)
   ___WRONG_NARGS(0,3,0,0)
___DEF_GLBL(___L__25_add_2d_method_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_STK(5,___ALLOC_CLO(1))
   ___BEGIN_SETUP_CLO(1,___STK(5),17)
   ___ADD_CLO_ELEM(0,___R2)
   ___END_SETUP_CLO(1)
   ___SET_R0(___LBL(3))
   ___ADJFP(8)
   ___CHECK_HEAP(1,4096)
___DEF_SLBL(1,___L1__25_add_2d_method_21_)
   ___POLL(2)
___DEF_SLBL(2,___L2__25_add_2d_method_21_)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(3,___L3__25_add_2d_method_21_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(4))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),76,___G__25_position)
___DEF_SLBL(4,___L4__25_add_2d_method_21_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L22__25_add_2d_method_21_)
   ___END_IF
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(5))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(5,___L5__25_add_2d_method_21_)
   ___SET_STK(-2,___R1)
   ___SET_R1(___STK(-5))
   ___SET_R0(___LBL(6))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(1),70,___G__25_list)
___DEF_SLBL(6,___L6__25_add_2d_method_21_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(7))
   ___SET_R1(___STK(-6))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),59,___G__25_append)
___DEF_SLBL(7,___L7__25_add_2d_method_21_)
   ___SET_STK(-5,___STK(-3))
   ___IF(___FALSEP(___STK(-5)))
   ___GOTO(___L19__25_add_2d_method_21_)
   ___END_IF
   ___GOTO(___L21__25_add_2d_method_21_)
___DEF_SLBL(8,___L8__25_add_2d_method_21_)
   ___IF(___NOT(___FALSEP(___STK(-5))))
   ___GOTO(___L21__25_add_2d_method_21_)
   ___END_IF
___DEF_GLBL(___L19__25_add_2d_method_21_)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(9))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_function_2d_methods)
___DEF_SLBL(9,___L9__25_add_2d_method_21_)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(1),70,___G__25_list)
___DEF_SLBL(10,___L10__25_add_2d_method_21_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(11))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),59,___G__25_append)
___DEF_SLBL(11,___L11__25_add_2d_method_21_)
   ___GOTO(___L20__25_add_2d_method_21_)
___DEF_SLBL(12,___L12__25_add_2d_method_21_)
   ___SET_STK(-5,___STK(-3))
___DEF_GLBL(___L20__25_add_2d_method_21_)
   ___SET_STK(-4,___R1)
   ___SET_R2(___STK(-5))
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(2),41,___G__25_set_2d_function_2d_method_2d_signatures_21_)
___DEF_SLBL(13,___L13__25_add_2d_method_21_)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(2),42,___G__25_set_2d_function_2d_methods_21_)
___DEF_SLBL(14,___L14__25_add_2d_method_21_)
   ___SET_R1(___STK(-6))
   ___POLL(15)
___DEF_SLBL(15,___L15__25_add_2d_method_21_)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L21__25_add_2d_method_21_)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_function_2d_methods)
___DEF_SLBL(16,___L16__25_add_2d_method_21_)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(3),71,___G__25_list_2d_put)
___DEF_GLBL(___L22__25_add_2d_method_21_)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(17,___L17__25_add_2d_method_21_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(17,1,0,0)
   ___SET_R3(___CLO(___R4,1))
   ___SET_R2(___R1)
   ___SET_R1(___GLO(86,___G_equal_3f_))
   ___POLL(18)
___DEF_SLBL(18,___L18__25_add_2d_method_21_)
   ___JUMPGLOSAFE(___SET_NARGS(3),67,___G__25_every_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_add_2d_primitive_2d_method_21_
#undef ___PH_LBL0
#define ___PH_LBL0 225
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L1__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L2__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L3__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L4__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L5__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L6__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L7__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L8__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L9__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L10__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L11__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L12__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L13__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L14__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L15__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L16__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L17__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L18__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L19__25_add_2d_primitive_2d_method_21_)
___DEF_P_HLBL(___L20__25_add_2d_primitive_2d_method_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_add_2d_primitive_2d_method_21_)
   ___IF_NARGS_EQ(4,___PUSH(___R1) ___SET_R1(___R2) ___SET_R2(___R3) ___SET_R3(___FAL))
   ___GET_KEY(0,4,0,1,___SUB(13))
___DEF_GLBL(___L__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(1,___R0)
   ___SET_STK(7,___R2)
   ___SET_STK(8,___KEY(1,___K_name))
   ___SET_STK(9,___R3)
   ___SET_STK(10,___KEY(2,___K_parameters))
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(2))
   ___ADJFP(14)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_add_2d_primitive_2d_method_21_)
   ___JUMPGLOSAFE(___SET_NARGS(1),69,___G__25_length)
___DEF_SLBL(2,___L2__25_add_2d_primitive_2d_method_21_)
   ___SET_R3(___R1)
   ___SET_R1(___STK(-12))
   ___SET_R2(___KEY(3,___K_required_2d_count))
   ___SET_R0(___LBL(3))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(7),23,___G__25_make_2d_primitive_2d_method)
___DEF_SLBL(3,___L3__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-4,___R1)
   ___SET_STK(-3,___ALLOC_CLO(1))
   ___BEGIN_SETUP_CLO(1,___STK(-3),19)
   ___ADD_CLO_ELEM(0,___STK(-6))
   ___END_SETUP_CLO(1)
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(5))
   ___CHECK_HEAP(4,4096)
___DEF_SLBL(4,___L4__25_add_2d_primitive_2d_method_21_)
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(5,___L5__25_add_2d_primitive_2d_method_21_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(6))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),76,___G__25_position)
___DEF_SLBL(6,___L6__25_add_2d_primitive_2d_method_21_)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L24__25_add_2d_primitive_2d_method_21_)
   ___END_IF
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(7))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(7,___L7__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-2,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(8))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(1),70,___G__25_list)
___DEF_SLBL(8,___L8__25_add_2d_primitive_2d_method_21_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(9))
   ___SET_R1(___STK(-6))
   ___ADJFP(-4)
   ___JUMPGLOSAFE(___SET_NARGS(2),59,___G__25_append)
___DEF_SLBL(9,___L9__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-6,___STK(-3))
   ___IF(___FALSEP(___STK(-6)))
   ___GOTO(___L21__25_add_2d_primitive_2d_method_21_)
   ___END_IF
   ___GOTO(___L23__25_add_2d_primitive_2d_method_21_)
___DEF_SLBL(10,___L10__25_add_2d_primitive_2d_method_21_)
   ___IF(___NOT(___FALSEP(___STK(-6))))
   ___GOTO(___L23__25_add_2d_primitive_2d_method_21_)
   ___END_IF
___DEF_GLBL(___L21__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_function_2d_methods)
___DEF_SLBL(11,___L11__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(1),70,___G__25_list)
___DEF_SLBL(12,___L12__25_add_2d_primitive_2d_method_21_)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(13))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),59,___G__25_append)
___DEF_SLBL(13,___L13__25_add_2d_primitive_2d_method_21_)
   ___GOTO(___L22__25_add_2d_primitive_2d_method_21_)
___DEF_SLBL(14,___L14__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-6,___STK(-3))
___DEF_GLBL(___L22__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-4,___R1)
   ___SET_R2(___STK(-6))
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(2),41,___G__25_set_2d_function_2d_method_2d_signatures_21_)
___DEF_SLBL(15,___L15__25_add_2d_primitive_2d_method_21_)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(2),42,___G__25_set_2d_function_2d_methods_21_)
___DEF_SLBL(16,___L16__25_add_2d_primitive_2d_method_21_)
   ___SET_R1(___STK(-7))
   ___POLL(17)
___DEF_SLBL(17,___L17__25_add_2d_primitive_2d_method_21_)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(3))
___DEF_GLBL(___L23__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(18))
   ___JUMPGLOSAFE(___SET_NARGS(1),9,___G__25_function_2d_methods)
___DEF_SLBL(18,___L18__25_add_2d_primitive_2d_method_21_)
   ___SET_R3(___STK(-4))
   ___SET_R2(___STK(-6))
   ___SET_R0(___LBL(14))
   ___JUMPGLOSAFE(___SET_NARGS(3),71,___G__25_list_2d_put)
___DEF_GLBL(___L24__25_add_2d_primitive_2d_method_21_)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-7))
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(1),8,___G__25_function_2d_method_2d_signatures)
___DEF_SLBL(19,___L19__25_add_2d_primitive_2d_method_21_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(19,1,0,0)
   ___SET_R3(___CLO(___R4,1))
   ___SET_R2(___R1)
   ___SET_R1(___GLO(86,___G_equal_3f_))
   ___POLL(20)
___DEF_SLBL(20,___L20__25_add_2d_primitive_2d_method_21_)
   ___JUMPGLOSAFE(___SET_NARGS(3),67,___G__25_every_3f_)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_make_2d_generator
#undef ___PH_LBL0
#define ___PH_LBL0 247
#undef ___PD_ALL
#define ___PD_ALL ___D_FP
#undef ___PR_ALL
#define ___PR_ALL ___R_FP
#undef ___PW_ALL
#define ___PW_ALL ___W_FP
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_make_2d_generator)
___DEF_P_HLBL(___L1__25_make_2d_generator)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_make_2d_generator)
   ___IF_NARGS_EQ(4,___NOTHING)
   ___WRONG_NARGS(0,4,0,0)
___DEF_GLBL(___L__25_make_2d_generator)
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___SUB(14))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_make_2d_generator)
   ___JUMPPRM(___SET_NARGS(5),___PRM(54,___G__23__23_structure))
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_generator_3f_
#undef ___PH_LBL0
#define ___PH_LBL0 250
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_generator_3f_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_generator_3f_)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_generator_3f_)
   ___SET_R1(___BOOLEAN(___STRUCTUREDIOP(___R1,___SYM(1,___S__23__23_type_2d_4_2d_32EDAE4A_2d_BA00_2d_4313_2d_BC4F_2d_1F3A1F7AC8C0))))
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_generator_2d_yield_2d_expression
#undef ___PH_LBL0
#define ___PH_LBL0 252
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_generator_2d_yield_2d_expression)
___DEF_P_HLBL(___L1__25_generator_2d_yield_2d_expression)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_generator_2d_yield_2d_expression)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_generator_2d_yield_2d_expression)
   ___SET_STK(1,___GLO(17,___G__25_generator_2d_yield_2d_expression))
   ___SET_R2(___TYPEID(___SUB(14)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_generator_2d_yield_2d_expression)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(14))
   ___SET_R1(___FIX(1L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_generator_2d_yield_2d_expression)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_generator_2d_yield_2d_expression)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(1L),___SUB(14),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_generator_2d_then_2d_expression
#undef ___PH_LBL0
#define ___PH_LBL0 255
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_generator_2d_then_2d_expression)
___DEF_P_HLBL(___L1__25_generator_2d_then_2d_expression)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_generator_2d_then_2d_expression)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_generator_2d_then_2d_expression)
   ___SET_STK(1,___GLO(16,___G__25_generator_2d_then_2d_expression))
   ___SET_R2(___TYPEID(___SUB(14)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_generator_2d_then_2d_expression)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(14))
   ___SET_R1(___FIX(2L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_generator_2d_then_2d_expression)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_generator_2d_then_2d_expression)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(2L),___SUB(14),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_generator_2d_environment
#undef ___PH_LBL0
#define ___PH_LBL0 258
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_generator_2d_environment)
___DEF_P_HLBL(___L1__25_generator_2d_environment)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_generator_2d_environment)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_generator_2d_environment)
   ___SET_STK(1,___GLO(15,___G__25_generator_2d_environment))
   ___SET_R2(___TYPEID(___SUB(14)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_generator_2d_environment)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(14))
   ___SET_R1(___FIX(3L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_generator_2d_environment)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_generator_2d_environment)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(3L),___SUB(14),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_set_2d_generator_2d_environment_21_
#undef ___PH_LBL0
#define ___PH_LBL0 261
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_set_2d_generator_2d_environment_21_)
___DEF_P_HLBL(___L1__25_set_2d_generator_2d_environment_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_set_2d_generator_2d_environment_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_set_2d_generator_2d_environment_21_)
   ___SET_STK(1,___GLO(44,___G__25_set_2d_generator_2d_environment_21_))
   ___SET_R3(___TYPEID(___SUB(14)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R3))
   ___GOTO(___L2__25_set_2d_generator_2d_environment_21_)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___R2)
   ___SET_R3(___STK(2))
   ___SET_R2(___SUB(14))
   ___SET_R1(___FIX(3L))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_set_2d_generator_2d_environment_21_)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(5),___PRM(52,___G__23__23_direct_2d_structure_2d_set_21_))
___DEF_GLBL(___L2__25_set_2d_generator_2d_environment_21_)
   ___UNCHECKEDSTRUCTURESET(___R1,___R2,___FIX(3L),___SUB(14),___STK(0))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_generator_2d_bindings
#undef ___PH_LBL0
#define ___PH_LBL0 264
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_generator_2d_bindings)
___DEF_P_HLBL(___L1__25_generator_2d_bindings)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_generator_2d_bindings)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_generator_2d_bindings)
   ___SET_STK(1,___GLO(14,___G__25_generator_2d_bindings))
   ___SET_R2(___TYPEID(___SUB(14)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R2))
   ___GOTO(___L2__25_generator_2d_bindings)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_R3(___STK(1))
   ___SET_R2(___SUB(14))
   ___SET_R1(___FIX(4L))
   ___ADJFP(1)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_generator_2d_bindings)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(4),___PRM(51,___G__23__23_direct_2d_structure_2d_ref))
___DEF_GLBL(___L2__25_generator_2d_bindings)
   ___SET_R1(___UNCHECKEDSTRUCTUREREF(___R1,___FIX(4L),___SUB(14),___STK(0)))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_set_2d_generator_2d_bindings_21_
#undef ___PH_LBL0
#define ___PH_LBL0 267
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R1 ___W_R2 ___W_R3
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_set_2d_generator_2d_bindings_21_)
___DEF_P_HLBL(___L1__25_set_2d_generator_2d_bindings_21_)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_set_2d_generator_2d_bindings_21_)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(0,2,0,0)
___DEF_GLBL(___L__25_set_2d_generator_2d_bindings_21_)
   ___SET_STK(1,___GLO(43,___G__25_set_2d_generator_2d_bindings_21_))
   ___SET_R3(___TYPEID(___SUB(14)))
   ___ADJFP(1)
   ___IF(___STRUCTUREDIOP(___R1,___R3))
   ___GOTO(___L2__25_set_2d_generator_2d_bindings_21_)
   ___END_IF
   ___SET_STK(1,___STK(0))
   ___SET_STK(0,___R1)
   ___SET_STK(2,___STK(1))
   ___SET_STK(1,___R2)
   ___SET_R3(___STK(2))
   ___SET_R2(___SUB(14))
   ___SET_R1(___FIX(4L))
   ___ADJFP(2)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_set_2d_generator_2d_bindings_21_)
   ___ADJFP(-1)
   ___JUMPPRM(___SET_NARGS(5),___PRM(52,___G__23__23_direct_2d_structure_2d_set_21_))
___DEF_GLBL(___L2__25_set_2d_generator_2d_bindings_21_)
   ___UNCHECKEDSTRUCTURESET(___R1,___R2,___FIX(4L),___SUB(14),___STK(0))
   ___ADJFP(-1)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__25_next
#undef ___PH_LBL0
#define ___PH_LBL0 270
#undef ___PD_ALL
#define ___PD_ALL ___D_HEAP ___D_FP ___D_R0 ___D_R1 ___D_R2 ___D_R3 ___D_R4 ___D_F64(___F64V1) ___D_F64( \
___F64V2)
#undef ___PR_ALL
#define ___PR_ALL ___R_HEAP ___R_FP ___R_R0 ___R_R1 ___R_R2 ___R_R3 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_HEAP ___W_FP ___W_R0 ___W_R1 ___W_R2 ___W_R3 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__25_next)
___DEF_P_HLBL(___L1__25_next)
___DEF_P_HLBL(___L2__25_next)
___DEF_P_HLBL(___L3__25_next)
___DEF_P_HLBL(___L4__25_next)
___DEF_P_HLBL(___L5__25_next)
___DEF_P_HLBL(___L6__25_next)
___DEF_P_HLBL(___L7__25_next)
___DEF_P_HLBL(___L8__25_next)
___DEF_P_HLBL(___L9__25_next)
___DEF_P_HLBL(___L10__25_next)
___DEF_P_HLBL(___L11__25_next)
___DEF_P_HLBL(___L12__25_next)
___DEF_P_HLBL(___L13__25_next)
___DEF_P_HLBL(___L14__25_next)
___DEF_P_HLBL(___L15__25_next)
___DEF_P_HLBL(___L16__25_next)
___DEF_P_HLBL(___L17__25_next)
___DEF_P_HLBL(___L18__25_next)
___DEF_P_HLBL(___L19__25_next)
___DEF_P_HLBL(___L20__25_next)
___DEF_P_HLBL(___L21__25_next)
___DEF_P_HLBL(___L22__25_next)
___DEF_P_HLBL(___L23__25_next)
___DEF_P_HLBL(___L24__25_next)
___DEF_P_HLBL(___L25__25_next)
___DEF_P_HLBL(___L26__25_next)
___DEF_P_HLBL(___L27__25_next)
___DEF_P_HLBL(___L28__25_next)
___DEF_P_HLBL(___L29__25_next)
___DEF_P_HLBL(___L30__25_next)
___DEF_P_HLBL(___L31__25_next)
___DEF_P_HLBL(___L32__25_next)
___DEF_P_HLBL(___L33__25_next)
___DEF_P_HLBL(___L34__25_next)
___DEF_P_HLBL(___L35__25_next)
___DEF_P_HLBL(___L36__25_next)
___DEF_P_HLBL(___L37__25_next)
___DEF_P_HLBL(___L38__25_next)
___DEF_P_HLBL(___L39__25_next)
___DEF_P_HLBL(___L40__25_next)
___DEF_P_HLBL(___L41__25_next)
___DEF_P_HLBL(___L42__25_next)
___DEF_P_HLBL(___L43__25_next)
___DEF_P_HLBL(___L44__25_next)
___DEF_P_HLBL(___L45__25_next)
___DEF_P_HLBL(___L46__25_next)
___DEF_P_HLBL(___L47__25_next)
___DEF_P_HLBL(___L48__25_next)
___DEF_P_HLBL(___L49__25_next)
___DEF_P_HLBL(___L50__25_next)
___DEF_P_HLBL(___L51__25_next)
___DEF_P_HLBL(___L52__25_next)
___DEF_P_HLBL(___L53__25_next)
___DEF_P_HLBL(___L54__25_next)
___DEF_P_HLBL(___L55__25_next)
___DEF_P_HLBL(___L56__25_next)
___DEF_P_HLBL(___L57__25_next)
___DEF_P_HLBL(___L58__25_next)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__25_next)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__25_next)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_R0(___LBL(2))
   ___ADJFP(8)
   ___POLL(1)
___DEF_SLBL(1,___L1__25_next)
   ___JUMPGLOSAFE(___SET_NARGS(1),15,___G__25_generator_2d_environment)
___DEF_SLBL(2,___L2__25_next)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(3))
   ___JUMPGLOSAFE(___SET_NARGS(1),14,___G__25_generator_2d_bindings)
___DEF_SLBL(3,___L3__25_next)
   ___SET_STK(-4,___R1)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(28))
   ___GOTO(___L59__25_next)
___DEF_SLBL(4,___L4__25_next)
   ___SET_R2(___R1)
   ___SET_R0(___STK(-7))
   ___SET_R1(___STK(-6))
   ___ADJFP(-8)
   ___POLL(5)
___DEF_SLBL(5,___L5__25_next)
___DEF_GLBL(___L59__25_next)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R0(___LBL(7))
   ___ADJFP(8)
   ___POLL(6)
___DEF_SLBL(6,___L6__25_next)
   ___JUMPGLOSAFE(___SET_NARGS(1),74,___G__25_null_3f_)
___DEF_SLBL(7,___L7__25_next)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L63__25_next)
   ___END_IF
   ___GOTO(___L60__25_next)
___DEF_SLBL(8,___L8__25_next)
   ___IF(___FALSEP(___R1))
   ___GOTO(___L62__25_next)
   ___END_IF
___DEF_GLBL(___L60__25_next)
   ___SET_R1(___STK(-5))
   ___POLL(9)
___DEF_SLBL(9,___L9__25_next)
___DEF_GLBL(___L61__25_next)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_GLBL(___L62__25_next)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(10))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(10,___L10__25_next)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(11))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_SLBL(11,___L11__25_next)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(12))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(12,___L12__25_next)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(13))
   ___JUMPGLOSAFE(___SET_NARGS(1),60,___G__25_cadr)
___DEF_SLBL(13,___L13__25_next)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(14))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),63,___G__25_cons)
___DEF_SLBL(14,___L14__25_next)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(4))
   ___JUMPGLOSAFE(___SET_NARGS(2),63,___G__25_cons)
___DEF_GLBL(___L63__25_next)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(15))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(15,___L15__25_next)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(16))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_SLBL(16,___L16__25_next)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(17))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(17,___L17__25_next)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(18))
   ___JUMPGLOSAFE(___SET_NARGS(1),60,___G__25_cadr)
___DEF_SLBL(18,___L18__25_next)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(19))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),63,___G__25_cons)
___DEF_SLBL(19,___L19__25_next)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(20))
   ___JUMPGLOSAFE(___SET_NARGS(2),63,___G__25_cons)
___DEF_SLBL(20,___L20__25_next)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(21))
   ___JUMPGLOSAFE(___SET_NARGS(1),74,___G__25_null_3f_)
___DEF_SLBL(21,___L21__25_next)
   ___IF(___NOT(___FALSEP(___R1)))
   ___GOTO(___L60__25_next)
   ___END_IF
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(22))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(22,___L22__25_next)
   ___SET_STK(-4,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(23))
   ___JUMPGLOSAFE(___SET_NARGS(1),62,___G__25_cdr)
___DEF_SLBL(23,___L23__25_next)
   ___SET_STK(-6,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(24))
   ___JUMPGLOSAFE(___SET_NARGS(1),61,___G__25_car)
___DEF_SLBL(24,___L24__25_next)
   ___SET_STK(-3,___R1)
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(25))
   ___JUMPGLOSAFE(___SET_NARGS(1),60,___G__25_cadr)
___DEF_SLBL(25,___L25__25_next)
   ___SET_R2(___R1)
   ___SET_R0(___LBL(26))
   ___SET_R1(___STK(-3))
   ___JUMPGLOSAFE(___SET_NARGS(2),63,___G__25_cons)
___DEF_SLBL(26,___L26__25_next)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(27))
   ___JUMPGLOSAFE(___SET_NARGS(2),63,___G__25_cons)
___DEF_SLBL(27,___L27__25_next)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(8))
   ___JUMPGLOSAFE(___SET_NARGS(1),74,___G__25_null_3f_)
___DEF_SLBL(28,___L28__25_next)
   ___SET_STK(-5,___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(29))
   ___JUMPGLOSAFE(___SET_NARGS(1),17,___G__25_generator_2d_yield_2d_expression)
___DEF_SLBL(29,___L29__25_next)
   ___SET_R2(___STK(-5))
   ___SET_R0(___LBL(30))
   ___JUMPGLOSAFE(___SET_NARGS(2),66,___G__25_eval_2d_sequence)
___DEF_SLBL(30,___L30__25_next)
   ___SET_STK(-3,___GLO(61,___G__25_car))
   ___IF(___NOT(___EQP(___GLO(88,___G_map),___PRM(88,___G_map))))
   ___GOTO(___L77__25_next)
   ___END_IF
   ___IF(___NOT(___PROCEDUREP(___STK(-3))))
   ___GOTO(___L77__25_next)
   ___END_IF
   ___SET_STK(-2,___R1)
   ___SET_R2(___STK(-3))
   ___SET_R3(___STK(-4))
   ___SET_R1(___STK(-4))
   ___SET_R0(___LBL(58))
   ___ADJFP(4)
   ___IF(___PAIRP(___R3))
   ___GOTO(___L65__25_next)
   ___END_IF
   ___GOTO(___L66__25_next)
___DEF_GLBL(___L64__25_next)
   ___IF(___NOT(___PAIRP(___R3)))
   ___GOTO(___L66__25_next)
   ___END_IF
___DEF_GLBL(___L65__25_next)
   ___SET_R3(___CDR(___R3))
   ___POLL(31)
___DEF_SLBL(31,___L31__25_next)
   ___GOTO(___L64__25_next)
___DEF_GLBL(___L66__25_next)
   ___IF(___NOT(___NULLP(___R3)))
   ___GOTO(___L76__25_next)
   ___END_IF
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___POLL(32)
___DEF_SLBL(32,___L32__25_next)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L67__25_next)
   ___END_IF
   ___GOTO(___L71__25_next)
___DEF_SLBL(33,___L33__25_next)
   ___SET_STK(-4,___R1)
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(54))
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L71__25_next)
   ___END_IF
___DEF_GLBL(___L67__25_next)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R1(___CAR(___R2))
   ___SET_R0(___LBL(33))
   ___ADJFP(8)
   ___POLL(34)
___DEF_SLBL(34,___L34__25_next)
   ___JUMPGENSAFE(___SET_NARGS(1),___STK(-6))
___DEF_SLBL(35,___L35__25_next)
   ___SET_STK(-5,___ALLOC_CLO(1))
   ___BEGIN_SETUP_CLO(1,___STK(-5),42)
   ___ADD_CLO_ELEM(0,___STK(-9))
   ___END_SETUP_CLO(1)
   ___ADJFP(-5)
   ___CHECK_HEAP(36,4096)
___DEF_SLBL(36,___L36__25_next)
   ___IF(___NOT(___EQP(___GLO(88,___G_map),___PRM(88,___G_map))))
   ___GOTO(___L72__25_next)
   ___END_IF
   ___SET_STK(6,___R1)
   ___SET_STK(7,___STK(-3))
   ___SET_R3(___R1)
   ___SET_R2(___STK(-3))
   ___SET_R1(___STK(0))
   ___SET_R0(___LBL(39))
   ___ADJFP(7)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L69__25_next)
   ___END_IF
___DEF_GLBL(___L68__25_next)
   ___IF(___NOT(___PAIRP(___R3)))
   ___GOTO(___L69__25_next)
   ___END_IF
   ___SET_R3(___CDR(___R3))
   ___SET_R2(___CDR(___R2))
   ___POLL(37)
___DEF_SLBL(37,___L37__25_next)
   ___IF(___PAIRP(___R2))
   ___GOTO(___L68__25_next)
   ___END_IF
___DEF_GLBL(___L69__25_next)
   ___IF(___NOT(___NULLP(___R2)))
   ___GOTO(___L74__25_next)
   ___END_IF
   ___IF(___NOT(___NULLP(___R3)))
   ___GOTO(___L74__25_next)
   ___END_IF
   ___SET_R3(___STK(-1))
   ___SET_R2(___STK(0))
   ___ADJFP(-2)
   ___POLL(38)
___DEF_SLBL(38,___L38__25_next)
   ___IF(___NOT(___PAIRP(___R2)))
   ___GOTO(___L71__25_next)
   ___END_IF
___DEF_GLBL(___L70__25_next)
   ___IF(___PAIRP(___R3))
   ___GOTO(___L75__25_next)
   ___END_IF
___DEF_GLBL(___L71__25_next)
   ___SET_R1(___NUL)
   ___JUMPPRM(___NOTHING,___R0)
___DEF_GLBL(___L72__25_next)
   ___SET_R3(___R1)
   ___SET_R2(___STK(-3))
   ___SET_R1(___STK(0))
   ___SET_R0(___LBL(39))
   ___ADJFP(5)
   ___JUMPGLOSAFE(___SET_NARGS(3),88,___G_map)
___DEF_SLBL(39,___L39__25_next)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-10))
   ___SET_R0(___LBL(40))
   ___JUMPGLOSAFE(___SET_NARGS(2),43,___G__25_set_2d_generator_2d_bindings_21_)
___DEF_SLBL(40,___L40__25_next)
   ___SET_R1(___STK(-6))
   ___POLL(41)
___DEF_SLBL(41,___L41__25_next)
   ___ADJFP(-12)
   ___JUMPPRM(___NOTHING,___STK(1))
___DEF_SLBL(42,___L42__25_next)
   ___IF_NARGS_EQ(2,___NOTHING)
   ___WRONG_NARGS(42,2,0,0)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_R2(___CLO(___R4,1))
   ___SET_R1(___STK(3))
   ___SET_R0(___LBL(44))
   ___ADJFP(8)
   ___POLL(43)
___DEF_SLBL(43,___L43__25_next)
   ___JUMPGLOSAFE(___SET_NARGS(2),65,___G__25_eval)
___DEF_SLBL(44,___L44__25_next)
   ___IF(___NOT(___EQP(___GLO(87,___G_list),___PRM(87,___G_list))))
   ___GOTO(___L73__25_next)
   ___END_IF
   ___BEGIN_ALLOC_LIST(2,___R1)
   ___ADD_LIST_ELEM(1,___STK(-6))
   ___END_ALLOC_LIST(2)
   ___SET_R1(___GET_LIST(2))
   ___CHECK_HEAP(45,4096)
___DEF_SLBL(45,___L45__25_next)
   ___POLL(46)
___DEF_SLBL(46,___L46__25_next)
   ___GOTO(___L61__25_next)
___DEF_GLBL(___L73__25_next)
   ___SET_R2(___R1)
   ___SET_R1(___STK(-6))
   ___SET_R0(___STK(-7))
   ___POLL(47)
___DEF_SLBL(47,___L47__25_next)
   ___ADJFP(-8)
   ___JUMPGLOSAFE(___SET_NARGS(2),87,___G_list)
___DEF_GLBL(___L74__25_next)
   ___SET_R3(___STK(-1))
   ___SET_R2(___STK(0))
   ___POLL(48)
___DEF_SLBL(48,___L48__25_next)
   ___ADJFP(-2)
   ___JUMPGLOSAFE(___SET_NARGS(3),88,___G_map)
___DEF_GLBL(___L75__25_next)
   ___SET_STK(1,___R0)
   ___SET_STK(2,___R1)
   ___SET_STK(3,___R2)
   ___SET_STK(4,___R3)
   ___SET_R2(___CAR(___R3))
   ___SET_R1(___CAR(___STK(3)))
   ___SET_R0(___LBL(50))
   ___ADJFP(8)
   ___POLL(49)
___DEF_SLBL(49,___L49__25_next)
   ___JUMPGENSAFE(___SET_NARGS(2),___STK(-6))
___DEF_SLBL(50,___L50__25_next)
   ___SET_STK(-3,___R1)
   ___SET_R3(___CDR(___STK(-4)))
   ___SET_R2(___CDR(___STK(-5)))
   ___SET_R1(___STK(-6))
   ___SET_R0(___LBL(51))
   ___IF(___PAIRP(___R2))
   ___GOTO(___L70__25_next)
   ___END_IF
   ___GOTO(___L71__25_next)
___DEF_SLBL(51,___L51__25_next)
   ___SET_R1(___CONS(___STK(-3),___R1))
   ___CHECK_HEAP(52,4096)
___DEF_SLBL(52,___L52__25_next)
   ___POLL(53)
___DEF_SLBL(53,___L53__25_next)
   ___GOTO(___L61__25_next)
___DEF_SLBL(54,___L54__25_next)
   ___SET_R1(___CONS(___STK(-4),___R1))
   ___CHECK_HEAP(55,4096)
___DEF_SLBL(55,___L55__25_next)
   ___POLL(56)
___DEF_SLBL(56,___L56__25_next)
   ___GOTO(___L61__25_next)
___DEF_GLBL(___L76__25_next)
   ___SET_STK(1,___R2)
   ___SET_R2(___R1)
   ___SET_R1(___STK(1))
   ___ADJFP(1)
   ___POLL(57)
___DEF_SLBL(57,___L57__25_next)
   ___ADJFP(-1)
   ___JUMPGLOSAFE(___SET_NARGS(2),88,___G_map)
___DEF_SLBL(58,___L58__25_next)
   ___SET_STK(-8,___R1)
   ___SET_R1(___STK(-10))
   ___SET_R0(___LBL(35))
   ___JUMPGLOSAFE(___SET_NARGS(1),16,___G__25_generator_2d_then_2d_expression)
___DEF_GLBL(___L77__25_next)
   ___SET_STK(-2,___R1)
   ___SET_R2(___STK(-4))
   ___SET_R1(___STK(-3))
   ___SET_R0(___LBL(58))
   ___ADJFP(4)
   ___JUMPGLOSAFE(___SET_NARGS(2),88,___G_map)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_functions," functions",___REF_FAL,19,0)
,___DEF_LBL_PROC(___H__20_functions,0,0)
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETI,8,0,0x3f11L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,5,0,0x11L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,5,0,0x11L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__20_functions,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_INTRO(___H__25_method_2d_signature_2d_matches_3f_,0,___REF_FAL,21,0)
,___DEF_LBL_PROC(___H__25_method_2d_signature_2d_matches_3f_,4,0)
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,8,1,0x3f02L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,9,1,0x3fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,8,1,0x3f02L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,9,1,0x3fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,8,1,0x3f1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETN,5,1,0x1fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_matches_3f_,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_type_2d_more_2d_specific_3f_,0,___REF_FAL,14,0)
,___DEF_LBL_PROC(___H__25_type_2d_more_2d_specific_3f_,2,0)
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_type_2d_more_2d_specific_3f_,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_INTRO(___H__25_method_2d_signature_2d_more_2d_specific_3f_,0,___REF_FAL,20,0)
,___DEF_LBL_PROC(___H__25_method_2d_signature_2d_more_2d_specific_3f_,2,0)
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x5L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__25_method_2d_signature_2d_more_2d_specific_3f_,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_INTRO(___H_make_2d__25_method,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H_make_2d__25_method,4,0)
,___DEF_LBL_RET(___H_make_2d__25_method,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_method_3f_,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_3f_,1,0)
,___DEF_LBL_RET(___H__25_method_3f_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H__25_method_2d_name,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_name,1,0)
,___DEF_LBL_RET(___H__25_method_2d_name,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_method_2d_formals,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_formals,1,0)
,___DEF_LBL_RET(___H__25_method_2d_formals,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_method_2d_restarg,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_restarg,1,0)
,___DEF_LBL_RET(___H__25_method_2d_restarg,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_method_2d_required_2d_count,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_required_2d_count,1,0)
,___DEF_LBL_RET(___H__25_method_2d_required_2d_count,___IFD(___RETI,1,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_private_2d_make_2d_primitive_2d_method,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_private_2d_make_2d_primitive_2d_method,5,0)
,___DEF_LBL_RET(___H__25_private_2d_make_2d_primitive_2d_method,___IFD(___RETI,4,4,0x3f7L))
,___DEF_LBL_INTRO(___H__25_primitive_2d_method_3f_,0,___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__25_primitive_2d_method_3f_,1,0)
,___DEF_LBL_INTRO(___H__25_method_2d_function,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_function,1,0)
,___DEF_LBL_RET(___H__25_method_2d_function,___IFD(___RETI,3,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_make_2d_primitive_2d_method,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_make_2d_primitive_2d_method,5,0)
,___DEF_LBL_RET(___H__25_make_2d_primitive_2d_method,___IFD(___RETI,4,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_private_2d_make_2d_interpreted_2d_method,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_private_2d_make_2d_interpreted_2d_method,6,0)
,___DEF_LBL_RET(___H__25_private_2d_make_2d_interpreted_2d_method,___IFD(___RETI,5,8,0x3f0fL))
,___DEF_LBL_INTRO(___H__25_interpreted_2d_method_3f_,0,___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__25_interpreted_2d_method_3f_,1,0)
,___DEF_LBL_INTRO(___H__25_method_2d_environment,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_environment,1,0)
,___DEF_LBL_RET(___H__25_method_2d_environment,___IFD(___RETI,3,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_method_2d_body,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_method_2d_body,1,0)
,___DEF_LBL_RET(___H__25_method_2d_body,___IFD(___RETI,3,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_make_2d_interpreted_2d_method,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_make_2d_interpreted_2d_method,6,0)
,___DEF_LBL_RET(___H__25_make_2d_interpreted_2d_method,___IFD(___RETI,5,8,0x3f07L))
,___DEF_LBL_INTRO(___H__25_method_2d_name_3f_,0,___REF_FAL,6,0)
,___DEF_LBL_PROC(___H__25_method_2d_name_3f_,1,0)
,___DEF_LBL_RET(___H__25_method_2d_name_3f_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_method_2d_name_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H__25_method_2d_name_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H__25_method_2d_name_3f_,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H__25_method_2d_name_3f_,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_INTRO(___H__25_private_2d_make_2d_function,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_private_2d_make_2d_function,4,0)
,___DEF_LBL_RET(___H__25_private_2d_make_2d_function,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_function_3f_,0,___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__25_function_3f_,1,0)
,___DEF_LBL_INTRO(___H__25_function_2d_name,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_function_2d_name,1,0)
,___DEF_LBL_RET(___H__25_function_2d_name,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_function_2d_method_2d_signatures,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_function_2d_method_2d_signatures,1,0)
,___DEF_LBL_RET(___H__25_function_2d_method_2d_signatures,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_set_2d_function_2d_method_2d_signatures_21_,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_set_2d_function_2d_method_2d_signatures_21_,2,0)
,___DEF_LBL_RET(___H__25_set_2d_function_2d_method_2d_signatures_21_,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_function_2d_method_2d_formals,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_function_2d_method_2d_formals,1,0)
,___DEF_LBL_RET(___H__25_function_2d_method_2d_formals,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_set_2d_function_2d_method_2d_formals_21_,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_set_2d_function_2d_method_2d_formals_21_,2,0)
,___DEF_LBL_RET(___H__25_set_2d_function_2d_method_2d_formals_21_,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_function_2d_methods,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_function_2d_methods,1,0)
,___DEF_LBL_RET(___H__25_function_2d_methods,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_set_2d_function_2d_methods_21_,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_set_2d_function_2d_methods_21_,2,0)
,___DEF_LBL_RET(___H__25_set_2d_function_2d_methods_21_,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_make_2d_function,0,___REF_FAL,6,0)
,___DEF_LBL_PROC(___H__25_make_2d_function,1,0)
,___DEF_LBL_RET(___H__25_make_2d_function,___IFD(___RETI,8,1,0x3f03L))
,___DEF_LBL_RET(___H__25_make_2d_function,___IFD(___RETN,5,1,0x3L))
,___DEF_LBL_RET(___H__25_make_2d_function,___IFD(___RETN,5,1,0x7L))
,___DEF_LBL_RET(___H__25_make_2d_function,___IFD(___RETN,5,1,0xfL))
,___DEF_LBL_RET(___H__25_make_2d_function,___IFD(___RETI,8,8,0x3f0dL))
,___DEF_LBL_INTRO(___H__25_function_2d_max_2d_method_2d_index,0,___REF_FAL,6,0)
,___DEF_LBL_PROC(___H__25_function_2d_max_2d_method_2d_index,1,0)
,___DEF_LBL_RET(___H__25_function_2d_max_2d_method_2d_index,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_RET(___H__25_function_2d_max_2d_method_2d_index,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_function_2d_max_2d_method_2d_index,___IFD(___RETN,1,0,0x1L))
,___DEF_LBL_RET(___H__25_function_2d_max_2d_method_2d_index,___IFD(___RETI,4,4,0x3f0L))
,___DEF_LBL_RET(___H__25_function_2d_max_2d_method_2d_index,___IFD(___RETI,4,0,0x3f1L))
,___DEF_LBL_INTRO(___H__25_function_2d_nth_2d_method_2d_signature,0,___REF_FAL,4,0)
,___DEF_LBL_PROC(___H__25_function_2d_nth_2d_method_2d_signature,2,0)
,___DEF_LBL_RET(___H__25_function_2d_nth_2d_method_2d_signature,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_function_2d_nth_2d_method_2d_signature,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_function_2d_nth_2d_method_2d_signature,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H__25_function_2d_nth_2d_method,0,___REF_FAL,4,0)
,___DEF_LBL_PROC(___H__25_function_2d_nth_2d_method,2,0)
,___DEF_LBL_RET(___H__25_function_2d_nth_2d_method,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_function_2d_nth_2d_method,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_function_2d_nth_2d_method,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_INTRO(___H__25_function_2d_best_2d_method,0,___REF_FAL,25,0)
,___DEF_LBL_PROC(___H__25_function_2d_best_2d_method,2,0)
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0x9fL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,3,4,0x3f0L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0x7fL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,8,1,0x3f06L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,5,1,0x6L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,8,1,0x3f02L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,8,1,0x3f02L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___OFD(___RETI,12,3,0x3f07fL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0x7fL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0xffL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0x1ffL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,13,3,0x2ffL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0xffL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0xffL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETI,3,4,0x3f7L))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___IFD(___RETN,9,3,0x6fL))
,___DEF_LBL_RET(___H__25_function_2d_best_2d_method,___OFD(___RETI,12,3,0x3f07fL))
,___DEF_LBL_INTRO(___H__25_add_2d_method_21_,0,___REF_FAL,19,0)
,___DEF_LBL_PROC(___H__25_add_2d_method_21_,3,0)
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETI,8,0,0x3f1fL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETI,8,0,0x3f1fL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,9,0,0x3bL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x1bL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x17L))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x13L))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0xbL))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETN,5,0,0x1fL))
,___DEF_LBL_PROC(___H__25_add_2d_method_21_,1,1)
,___DEF_LBL_RET(___H__25_add_2d_method_21_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H__25_add_2d_primitive_2d_method_21_,0,___REF_FAL,21,0)
,___DEF_LBL_PROC(___H__25_add_2d_primitive_2d_method_21_,5,0)
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___OFD(___RETI,16,2,0x3f0f0fL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,13,2,0xf0fL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x7L))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETI,8,2,0x3f1fL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0xfL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,9,2,0x3dL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x1dL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0xfL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0xfL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x17L))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x7L))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x15L))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0xdL))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x5L))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETI,8,2,0x3f04L))
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETN,5,2,0x1fL))
,___DEF_LBL_PROC(___H__25_add_2d_primitive_2d_method_21_,1,1)
,___DEF_LBL_RET(___H__25_add_2d_primitive_2d_method_21_,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H__25_make_2d_generator,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_make_2d_generator,4,0)
,___DEF_LBL_RET(___H__25_make_2d_generator,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_generator_3f_,0,___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__25_generator_3f_,1,0)
,___DEF_LBL_INTRO(___H__25_generator_2d_yield_2d_expression,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_generator_2d_yield_2d_expression,1,0)
,___DEF_LBL_RET(___H__25_generator_2d_yield_2d_expression,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_generator_2d_then_2d_expression,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_generator_2d_then_2d_expression,1,0)
,___DEF_LBL_RET(___H__25_generator_2d_then_2d_expression,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_generator_2d_environment,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_generator_2d_environment,1,0)
,___DEF_LBL_RET(___H__25_generator_2d_environment,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_set_2d_generator_2d_environment_21_,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_set_2d_generator_2d_environment_21_,2,0)
,___DEF_LBL_RET(___H__25_set_2d_generator_2d_environment_21_,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_generator_2d_bindings,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_generator_2d_bindings,1,0)
,___DEF_LBL_RET(___H__25_generator_2d_bindings,___IFD(___RETI,2,4,0x3f1L))
,___DEF_LBL_INTRO(___H__25_set_2d_generator_2d_bindings_21_,0,___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__25_set_2d_generator_2d_bindings_21_,2,0)
,___DEF_LBL_RET(___H__25_set_2d_generator_2d_bindings_21_,___IFD(___RETI,3,4,0x3f3L))
,___DEF_LBL_INTRO(___H__25_next,0,___REF_FAL,59,0)
,___DEF_LBL_PROC(___H__25_next,1,0)
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x17L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x17L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xdL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x17L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xbL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x7L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f07L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,9,0,0x2fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,7,0,0x3f6fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,2,4,0x3f3L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,9,0,0x23L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,9,0,0x21L))
,___DEF_LBL_RET(___H__25_next,___OFD(___RETI,12,0,0x3f001L))
,___DEF_LBL_PROC(___H__25_next,2,1)
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f03L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x3L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,8,0x3f00L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,2,4,0x3f0L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f0fL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0xfL))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x11L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,5,0,0x9L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,8,0,0x3f01L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETI,1,4,0x3f0L))
,___DEF_LBL_RET(___H__25_next,___IFD(___RETN,9,0,0x27L))
___END_LBL

___BEGIN_OFD
 ___DEF_OFD(___RETI,12,3)
               ___GCMAP1(0x3f07fL)
,___DEF_OFD(___RETI,12,3)
               ___GCMAP1(0x3f07fL)
,___DEF_OFD(___RETI,16,2)
               ___GCMAP1(0x3f0f0fL)
,___DEF_OFD(___RETI,12,0)
               ___GCMAP1(0x3f001L)
___END_OFD

___BEGIN_MOD1
___DEF_PRM(0,___G__20_functions,1)
___END_MOD1

___BEGIN_MOD2
___DEF_SYM2(0,___S__23__23_type_2d_4_2d_0E7FE105_2d_F4B7_2d_4EE4_2d_AA16_2d_9475976B003D,"##type-4-0E7FE105-F4B7-4EE4-AA16-9475976B003D")

___DEF_SYM2(1,___S__23__23_type_2d_4_2d_32EDAE4A_2d_BA00_2d_4313_2d_BC4F_2d_1F3A1F7AC8C0,"##type-4-32EDAE4A-BA00-4313-BC4F-1F3A1F7AC8C0")

___DEF_SYM2(2,___S__23__23_type_2d_4_2d_927A1AD2_2d_762A_2d_4DE6_2d_9900_2d_C22857D20E5A,"##type-4-927A1AD2-762A-4DE6-9900-C22857D20E5A")

___DEF_SYM2(3,___S__23__23_type_2d_5,"##type-5")
___DEF_SYM2(4,___S__25_function,"%function")
___DEF_SYM2(5,___S__25_generator,"%generator")
___DEF_SYM2(6,___S__25_interpreted_2d_method,"%interpreted-method")
___DEF_SYM2(7,___S__25_method,"%method")
___DEF_SYM2(8,___S__25_primitive_2d_method,"%primitive-method")
___DEF_SYM2(9,___S__3c_function_3e_,"<function>")
___DEF_SYM2(10,___S__3c_generator_3e_,"<generator>")
___DEF_SYM2(11,___S__3c_interpreted_2d_method_3e_,"<interpreted-method>")
___DEF_SYM2(12,___S__3c_primitive_2d_method_3e_,"<primitive-method>")
___DEF_SYM2(13,___S_bindings,"bindings")
___DEF_SYM2(14,___S_body,"body")
___DEF_SYM2(15,___S_environment,"environment")
___DEF_SYM2(16,___S_fields,"fields")
___DEF_SYM2(17,___S_flags,"flags")
___DEF_SYM2(18,___S_formals,"formals")
___DEF_SYM2(19,___S_function,"function")
___DEF_SYM2(20,___S_id,"id")
___DEF_SYM2(21,___S_methods,"methods")
___DEF_SYM2(22,___S_name,"name")
___DEF_SYM2(23,___S_required_2d_count,"required-count")
___DEF_SYM2(24,___S_restarg,"restarg")
___DEF_SYM2(25,___S_signatures,"signatures")
___DEF_SYM2(26,___S_super,"super")
___DEF_SYM2(27,___S_then_2d_expression,"then-expression")
___DEF_SYM2(28,___S_type,"type")
___DEF_SYM2(29,___S_yield_2d_expression,"yield-expression")
___DEF_KEY2(0,___K_environment,"environment")
___DEF_KEY2(1,___K_name,"name")
___DEF_KEY2(2,___K_parameters,"parameters")
___DEF_KEY2(3,___K_required_2d_count,"required-count")
___DEF_KEY2(4,___K_restarg,"restarg")
___END_MOD2

#endif
