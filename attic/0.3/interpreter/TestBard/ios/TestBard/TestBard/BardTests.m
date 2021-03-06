//
//  BardTests.m
//  TestBard_simulator
//
//  Created by mikel evins on 8/9/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "BardTests.h"

@implementation BardTests

-(void)reportTestResult:(BOOL)succeeded  
     withSuccessMessage:(NSString*)successMsg andFailureMessage:(NSString*)failureMsg{
    if (succeeded) {
        NSLog(@"%@",successMsg);
    } else{
        NSLog(@"%@",failureMsg);
    }
}

-(BOOL)testVersion {
    const char* version_str = bard_version();
    NSString* versionStr = [NSString stringWithCString:version_str encoding:NSASCIIStringEncoding];
    if ([versionStr isEqualToString:@"Bard version 0.2.8"]) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testInit {
    BOOL init_result = init_bard();
    if (init_result) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testReadZero {
    const char* zero_str = "0";
    BardValue zero_val = bard_read(zero_str);
    BOOL isZero = ___FIXZEROP(zero_val);
    if (isZero) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testReadTypes {
    BOOL succeeded = YES;
    
    BardValue null_val = bard_read("nothing");
    int null_type = bard_type(null_val);
    if(null_type!=BARD_NULL){succeeded=NO;NSLog(@"reading 'nothing' type FAILED");}
    const char* null_str = bard_typename(null_val);
    if(strcmp(null_str, BARD_NULL_NAME)!=0){NSLog(@"reading 'nothing' type name FAILED");}
    
    
    BardValue char_val = bard_read("#\\f");
    int char_type = bard_type(char_val);
    if(char_type!=BARD_CHARACTER){succeeded=NO;NSLog(@"reading '#\\f' type FAILED");}
    const char* char_str = bard_typename(char_val);
    if(strcmp(char_str, BARD_CHARACTER_NAME)!=0){NSLog(@"reading '#\\f' type name FAILED");}
    
    BardValue bool_val = bard_read("#f");
    int bool_type = bard_type(bool_val);
    if(bool_type!=BARD_BOOLEAN){succeeded=NO;NSLog(@"reading '#f' type FAILED");}
    const char* bool_str = bard_typename(bool_val);
    if(strcmp(bool_str, BARD_BOOLEAN_NAME)!=0){NSLog(@"reading '#f' type name FAILED");}
    
    BardValue int_val = bard_read("13");
    int int_type = bard_type(int_val);
    if(int_type!=BARD_INTEGER){succeeded=NO;NSLog(@"reading '13' type FAILED");}
    const char* int_str = bard_typename(int_val);
    if(strcmp(int_str, BARD_INTEGER_NAME)!=0){NSLog(@"reading '13' type name FAILED");}
    
    BardValue float_val = bard_read("13.14");
    int float_type = bard_type(float_val);
    if(float_type!=BARD_FLOAT){succeeded=NO;NSLog(@"reading '13.14' type FAILED");}
    const char* float_str = bard_typename(float_val);
    if(strcmp(float_str, BARD_FLOAT_NAME)!=0){NSLog(@"reading '13.14' type name FAILED");}
    
    BardValue rat_val = bard_read("13/14");
    int rat_type = bard_type(rat_val);
    if(rat_type!=BARD_RATIO){succeeded=NO;NSLog(@"reading '13/14' type FAILED");}
    const char* rat_str = bard_typename(rat_val);
    if(strcmp(rat_str, BARD_RATIO_NAME)!=0){NSLog(@"reading '13/14' type name FAILED");}
    
    BardValue sym_val = bard_read("foo");
    int sym_type = bard_type(sym_val);
    if(sym_type!=BARD_SYMBOL){succeeded=NO;NSLog(@"reading 'foo' FAILED");}
    const char* sym_str = bard_typename(sym_val);
    if(strcmp(sym_str, BARD_SYMBOL_NAME)!=0){NSLog(@"reading 'foo' type name FAILED");}
    
    BardValue key_val = bard_read("bar:");
    int key_type = bard_type(key_val);
    if(key_type!=BARD_KEYWORD){succeeded=NO;NSLog(@"reading 'bar:' FAILED");}
    const char* key_str = bard_typename(key_val);
    if(strcmp(key_str, BARD_KEYWORD_NAME)!=0){NSLog(@"reading 'bar:' type name FAILED");}
    
    BardValue str_val = bard_read("\"grault\"");
    int str_type = bard_type(str_val);
    if(str_type!=BARD_TEXT){succeeded=NO;NSLog(@"reading '\"grault\"' FAILED");}
    const char* str_str = bard_typename(str_val);
    if(strcmp(str_str, BARD_TEXT_NAME)!=0){NSLog(@"reading '\"grault\"' type name FAILED");}
   
    BardValue ls_val = bard_read("[0 1 2 3]");
    int ls_type = bard_type(ls_val);
    if(ls_type!=BARD_LIST){succeeded=NO;NSLog(@"reading '[0 1 2 3]' FAILED");}
    const char* ls_str = bard_typename(ls_val);
    if(strcmp(ls_str, BARD_LIST_NAME)!=0){NSLog(@"reading '[0 1 2 3]' type name FAILED");}
   
    return succeeded;
}

-(BOOL)testIsNothing {    
    BardValue null_val = bard_read("nothing");
    if (bard_is_nothing(null_val)) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testIsEmpty {    
    BardValue str_val = bard_read("\"\"");
    if (bard_is_empty(str_val)) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testAsChar {    
    BardValue char_val = bard_read("#\\c");
    char ch = as_char(char_val);
    if (ch == 'c') {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testAsBool {    
    BardValue bool_val = bard_read("#f");
    BOOL b = as_bool(bool_val);
    if (b) {
        return NO;
    } else {
        return YES;
    }
}

-(BOOL)testAsInt {    
    BardValue int_val = bard_read("127");
    int i = as_int(int_val);
    if (i==127) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testAsFloat {    
    BardValue float_val = bard_read("123.45");
    float f = as_float(float_val);
    if ((123.44<f)&&(f<123.46)) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testAsString {    
    BardValue str_val = bard_read("\"Wibble\"");
    const char* str = as_string(str_val);
    if (strcmp(str, "Wibble")==0) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testOid {
    BardValue sym_ref1 = bard_read("foo");
    int sym_oid1 = bard_object_to_id(sym_ref1);
    BardValue sym_ref2 = bard_id_to_object(sym_oid1);
    BOOL same = ___EQP(sym_ref1, sym_ref2);
    if (same) {
        return YES;
    } else {
        return NO;
    }
}

-(BOOL)testReadLines {
    const char* test_text = "line one\nline two\nline three\n";
    BardValue lines_val = bard_read_lines(test_text);
    int lines_type = bard_type(lines_val);
    if (lines_type != BARD_LIST) {
        NSLog(@"Reading lines FAILED: result is not a list.");
        return NO;
    }
    BOOL more = ( ! ___NULLP(lines_val));
    int i = 0;
    while (more) {
        BardValue next = ___CAR(lines_val);
        lines_val = ___CDR(lines_val);
        more = ( ! ___NULLP(lines_val));
        const char* next_str = as_string(next);
        switch (i) {
            case 0:
                if (strcmp(next_str,"line one")!=0) { return NO;}
                break;
            case 1:
                if (strcmp(next_str,"line two")!=0) { return NO;}
                break;
            case 2:
                if (strcmp(next_str,"line three")!=0) { return NO;}
                break;
            default:
                break;
        }
        i++;
    }
    return YES;
}

-(BOOL)testReadNonEmptyLines {
    const char* test_text = "   \nline two\n;;;line three\n";
    BardValue lines_val = bard_read_nonempty_lines(test_text);
    int lines_type = bard_type(lines_val);
    if (lines_type != BARD_LIST) {
        NSLog(@"Reading non-empty lines FAILED: result is not a list.");
        return NO;
    }
    int len = bard_length(lines_val);
    if (len==1) {
        return YES;
    } else {
        NSLog(@"Reading non-empty lines FAILED: length was %d",len);
        return NO;
    }
}

-(BOOL)testEval {
    const char* test_text = "(+ 1 2)";
    BardValue expr = bard_read(test_text);
    BardValue val = bard_eval(expr);
    int val_type = bard_type(val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(eval '(+ 1 2)) FAILED: result is not an integer.");
        return NO;
    }

    int v = as_int(val);
    if (v==3) {
        return YES;
    } else {
        NSLog(@"(eval '(+ 1 2)) FAILED: result is not 3.");
        return NO;
    }
}

-(BOOL)testLoadFromString {
    const char* test_text = "(begin (define x 5)(define y 6)(define z 7) y)";
    BardValue expr = bard_read(test_text);
    BardValue val = bard_eval(expr);
    int val_type = bard_type(val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"loading (begin (define x 5)(define y 6)(define z 7) y) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(val);
    if (v==6) {
        return YES;
    } else {
        NSLog(@"loading (begin (define x 5)(define y 6)(define z 7) y) FAILED: result is not 6.");
        return NO;
    }
}

-(BOOL)testGet {
    const char* test_frame_text = "{a: 1 b: 2 c: 3}";
    const char* test_key_text = "b:";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {a: 1 b: 2 c: 3} b:) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {a: 1 b: 2 c: 3} b:) FAILED: result is not 2");
        return NO;
    }
}


-(BOOL)testGetChar {
    const char* test_frame_text = "{#\\a 1 #\\b 2 #\\c 3}";
    const char* test_key_text = "#\\b";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {#\\a 1 #\\b 2 #\\c 3} #\\b) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {#\\a 1 #\\b 2 #\\c 3} #\\b) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testGetBool {
    const char* test_frame_text = "{true 1 false 2}";
    const char* test_key_text = "false";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {true 1 false 2} false) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {true 1 false 2} false) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testGetInt {
    const char* test_frame_text = "{1 1 2 2 3 3}";
    const char* test_key_text = "2";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {1 1 2 2 3 3} false) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {1 1 2 2 3 3} false) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testGetFloat {
    const char* test_frame_text = "{1.0 1 2.0 2 3.0 3}";
    const char* test_key_text = "2.0";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {1.0 1 2.0 2 3.0 3} false) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {1.0 1 2.0 2 3.0 3} false) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testGetSymbol {
    const char* test_frame_text = "{'one 1 'two 2 'three 3}";
    const char* test_key_text = "'two";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {'one 1 'two 2 'three 3} false) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {'one 1 'two 2 'three 3} false) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testGetKeyword {
    const char* test_frame_text = "{one: 1 two: 2 three: 3}";
    const char* test_key_text = "two:";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {one: 1 two: 2 three: 3} false) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {one: 1 two: 2 three: 3} false) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testGetString {
    const char* test_frame_text = "{\"one\" 1 \"two\" 2 \"three\" 3}";
    const char* test_key_text = "\"two\"";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_key_expr = bard_read(test_key_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_key = bard_eval(test_key_expr);
    BardValue test_val = bard_get(test_frame, test_key);
    int val_type = bard_type(test_val);
    if (val_type!=BARD_INTEGER) {
        NSLog(@"(get {\"one\" 1 \"two\" 2 \"three\" 3} false) FAILED: result is not an integer.");
        return NO;
    }
    
    int v = as_int(test_val);
    if (v==2) {
        return YES;
    } else {
        NSLog(@"(get {\"one\" 1 \"two\" 2 \"three\" 3} false) FAILED: result is not 2");
        return NO;
    }
}

-(BOOL)testKeys {
    const char* test_frame_text = "{a: 1 b: 2 c: 3}";
    BardValue test_frame_expr = bard_read(test_frame_text);
    BardValue test_frame = bard_eval(test_frame_expr);
    BardValue test_keys = bard_keys(test_frame);
    int val_type = bard_type(test_keys);
    if (val_type!=BARD_LIST) {
        NSLog(@"(keys {a: 1 b: 2 c: 3} false) FAILED: result is not an list.");
        return NO;
    }
    BardValue k = ___CAR(test_keys);
    BardValue k2 = bard_read("a:");
    BOOL same = ___EQP(k, k2);
    if (!same) {
        NSLog(@"(keys {a: 1 b: 2 c: 3} false) FAILED: first key is not a:");
        return NO;
    }
    test_keys = ___CDR(test_keys);

    k = ___CAR(test_keys);
    k2 = bard_read("b:");
    same = ___EQP(k, k2);
    if (!same) {
        NSLog(@"(keys {a: 1 b: 2 c: 3} false) FAILED: second key is not b:");
        return NO;
    }
    test_keys = ___CDR(test_keys);
    k = ___CAR(test_keys);
    k2 = bard_read("c:");
    same = ___EQP(k, k2);
    if (!same) {
        NSLog(@"(keys {a: 1 b: 2 c: 3} false) FAILED: third key is not c:");
        return NO;
    }
    
    return YES;
}

-(BOOL)testLists {
    const char* test_list_text = "nothing";
    BardValue test_expr = bard_read(test_list_text);
    BardValue test_list = bard_eval(test_expr);
    int test_list_type = bard_type(test_list);
    
    if (test_list_type!=BARD_NULL) {
        NSLog(@"reading 'nothing' FAILED: result is not nothing");
        return NO;
    }
    
    const char* zero_text = "0";
    BardValue zero_val = bard_read(zero_text);
    BardValue test_list2 = bard_cons(zero_val, test_list);
    int test_list2_type = bard_type(test_list2);
    
    if (test_list2_type!=BARD_LIST) {
        NSLog(@"(cons 0 nothing) FAILED: result is not a list");
        return NO;
    }
    
    BardValue hd = ___CAR(test_list2);
    int hdtype = bard_type(hd);
    if (hdtype!=BARD_INTEGER) {
        NSLog(@"testing lists FAILED: (car '(0)) is not an integer");
        return NO;
    }
    int hdval = as_int(hd);
    if (hdval!=0) {
        NSLog(@"testing lists FAILED: (car '(0)) is not 0");
        return NO;
    }
    
    const char* one_text = "1";
    BardValue one_val = bard_read(one_text);
    BardValue test_list3 = bard_cons(one_val, test_list2);
    int test_list3_type = bard_type(test_list3);
    
    if (test_list3_type!=BARD_LIST) {
        NSLog(@"(cons 1 '(0)) FAILED: result is not a list");
        return NO;
    }
    
    BardValue elt_val = bard_element(test_list3, 1);
    int elt_type = bard_type(elt_val);
    if (elt_type!=BARD_INTEGER) {
        NSLog(@"(elt '(1 0) 1) FAILED: result is not an integer");
        return NO;
    }
    int elt_testval = as_int(elt_val);
    if (elt_testval!=0) {
        NSLog(@"(elt '(1 0) 1) FAILED: result is not zero");
        return NO;
    }
    
   
    return YES;
}

-(BOOL)testBoxes {
    
    BardValue null_val = bard_read("nothing");
    BoxedBardValue* null_box = as_boxed(null_val);
    int null_tag = null_box->type;
    if (null_tag!=BARD_NULL) {NSLog(@"null box FAILED");return NO;} 
    
    BardValue char_val = bard_read("#\\C");
    BoxedBardValue* char_box = as_boxed(char_val);
    int char_tag = char_box->type;
    if (char_tag!=BARD_CHARACTER) {NSLog(@"char box FAILED");return NO;} 
    
    BardValue bool_val = bard_read("false");
    BoxedBardValue* bool_box = as_boxed(bool_val);
    int bool_tag = bool_box->type;
    if (bool_tag!=BARD_BOOLEAN) {NSLog(@"boolean box FAILED");return NO;} 
    
    BardValue int_val = bard_read("35");
    BoxedBardValue* int_box = as_boxed(int_val);
    int int_tag = int_box->type;
    if (int_tag!=BARD_INTEGER) {NSLog(@"integer box FAILED");return NO;} 
    
    BardValue float_val = bard_read("123.45");
    BoxedBardValue* float_box = as_boxed(float_val);
    int float_tag = float_box->type;
    if (float_tag!=BARD_FLOAT) {NSLog(@"float box FAILED");return NO;} 
    
    BardValue ratio_val = bard_read("2/3");
    BoxedBardValue* ratio_box = as_boxed(ratio_val);
    int ratio_tag = ratio_box->type;
    if (ratio_tag!=BARD_FLOAT) {NSLog(@"ratio box FAILED");return NO;} 
    
    BardValue symbol_val = bard_read("Foo");
    BoxedBardValue* symbol_box = as_boxed(symbol_val);
    int symbol_tag = symbol_box->type;
    if (symbol_tag!=BARD_SYMBOL) {NSLog(@"symbol box FAILED");return NO;} 
    
    BardValue keyword_val = bard_read("Bar:");
    BoxedBardValue* keyword_box = as_boxed(keyword_val);
    int keyword_tag = keyword_box->type;
    if (keyword_tag!=BARD_KEYWORD) {NSLog(@"keyword box FAILED");return NO;} 
    
    BardValue text_val = bard_read("\"Baz!\"");
    BoxedBardValue* text_box = as_boxed(text_val);
    int text_tag = text_box->type;
    if (text_tag!=BARD_TEXT) {NSLog(@"text box FAILED");return NO;} 
    
    
return YES;
}



-(void)run {
    [self reportTestResult:[self testVersion] 
        withSuccessMessage:@"testVersion: bard_version() succeeded" 
         andFailureMessage:@"testVersion: bard_version() FAILED"];
    
    [self reportTestResult:[self testInit] 
        withSuccessMessage:@"testInit: init_bard() succeeded" 
         andFailureMessage:@"testInit: init_bard() FAILED"];
    
    [self reportTestResult:[self testReadZero] 
        withSuccessMessage:@"testReadZero: bard_read(0) succeeded" 
         andFailureMessage:@"testReadZero: bard_read(0) FAILED"];
    
    [self reportTestResult:[self testReadTypes] 
        withSuccessMessage:@"testReadTypes: reading several types succeeded" 
         andFailureMessage:@"testReadTypes: reading several types FAILED"];
    
    [self reportTestResult:[self testIsNothing] 
        withSuccessMessage:@"testIsNothing: testing for nothing succeeded" 
         andFailureMessage:@"testIsNothing: testing for nothing FAILED"];
    
    [self reportTestResult:[self testIsEmpty] 
        withSuccessMessage:@"testIsEmpty: testing for empty succeeded" 
         andFailureMessage:@"testIsEmpty: testing for empty FAILED"];
    
    [self reportTestResult:[self testAsChar] 
        withSuccessMessage:@"testAsChar: as_char() succeeded" 
         andFailureMessage:@"testAsChar: as_char() FAILED"];
    
    [self reportTestResult:[self testAsBool] 
        withSuccessMessage:@"testAsBool: as_bool() succeeded" 
         andFailureMessage:@"testAsBool: as_bool() FAILED"];
    
    [self reportTestResult:[self testAsInt] 
        withSuccessMessage:@"testAsInt: as_int() succeeded" 
         andFailureMessage:@"testAsInt: as_int() FAILED"];
    
    [self reportTestResult:[self testAsFloat] 
        withSuccessMessage:@"testAsFloat: as_float() succeeded" 
         andFailureMessage:@"testAsFloat: as_float() FAILED"];
    
    [self reportTestResult:[self testAsString] 
        withSuccessMessage:@"testAsString: as_string() succeeded" 
         andFailureMessage:@"testAsString: as_string() FAILED"];
    
    [self reportTestResult:[self testOid] 
        withSuccessMessage:@"testOid: oid tests succeeded" 
         andFailureMessage:@"testOid: oid tests FAILED"];
    
    [self reportTestResult:[self testReadLines] 
        withSuccessMessage:@"testReadLines: reading several lines succeeded" 
         andFailureMessage:@"testReadLines: reading several lines FAILED"];
    
    [self reportTestResult:[self testReadNonEmptyLines] 
        withSuccessMessage:@"testReadNonEmptyLines: reading non-empty lines succeeded" 
         andFailureMessage:@"testReadNonEmptyLines: reading non-empty lines FAILED"];
    
    [self reportTestResult:[self testEval] 
        withSuccessMessage:@"testEval: (eval '(+ 1 2)) succeeded" 
         andFailureMessage:@"testEval: (eval '(+ 1 2)) FAILED"];
    
    [self reportTestResult:[self testLoadFromString] 
        withSuccessMessage:@"testLoadFromString: loading (begin (define x 5)(define y 6)(define z 7) y) succeeded" 
         andFailureMessage:@"testLoadFromString: loading (begin (define x 5)(define y 6)(define z 7) y) FAILED"];
    
    [self reportTestResult:[self testGet] 
        withSuccessMessage:@"testGet: (get {a: 1 b: 2 c: 3} b:) succeeded" 
         andFailureMessage:@"testGet: (get {a: 1 b: 2 c: 3} b:) FAILED"];
    
    [self reportTestResult:[self testGetChar] 
        withSuccessMessage:@"testGetChar: (get {a: 1 b: 2 c: 3} #\\b) succeeded" 
         andFailureMessage:@"testGetChar: (get {a: 1 b: 2 c: 3} #\\b) FAILED"];
    
    [self reportTestResult:[self testGetBool] 
        withSuccessMessage:@"testGetBool: (get {true 1 false 2} false) succeeded" 
         andFailureMessage:@"testGetBool: (get {true 1 false 2} false) FAILED"];
    
    [self reportTestResult:[self testGetInt] 
        withSuccessMessage:@"testGetInt: testGetInt succeeded" 
         andFailureMessage:@"testGetInt: testGetInt FAILED"];
    
    [self reportTestResult:[self testGetFloat] 
        withSuccessMessage:@"testGetFloat: testGetFloat succeeded" 
         andFailureMessage:@"testGetFloat: testGetFloat FAILED"];
    
    [self reportTestResult:[self testGetSymbol] 
        withSuccessMessage:@"testGetSymbol: testGetSymbol succeeded" 
         andFailureMessage:@"testGetSymbol: testGetSymbol FAILED"];
    
    [self reportTestResult:[self testGetKeyword] 
        withSuccessMessage:@"testGetKeyword: testGetKeyword succeeded" 
         andFailureMessage:@"testGetKeyword: testGetKeyword FAILED"];
    
    [self reportTestResult:[self testGetString] 
        withSuccessMessage:@"testGetString: testGetString succeeded" 
         andFailureMessage:@"testGetString: testGetString FAILED"];
    
    [self reportTestResult:[self testKeys] 
        withSuccessMessage:@"testKeys: testKeys succeeded" 
         andFailureMessage:@"testKeys: testKeys FAILED"];

    [self reportTestResult:[self testLists] 
        withSuccessMessage:@"testLists: testLists succeeded" 
         andFailureMessage:@"testLists: testLists FAILED"];
    
    NSLog(@"-");
    NSLog(@"-");
    NSLog(@"-");
    
    [self reportTestResult:[self testBoxes] 
        withSuccessMessage:@"testBoxes: testBoxes succeeded" 
         andFailureMessage:@"testBoxes: testBoxes FAILED"];
}


@end
