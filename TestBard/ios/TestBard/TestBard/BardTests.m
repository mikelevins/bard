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


-(void)run {
    [self reportTestResult:[self testVersion] 
        withSuccessMessage:@"test: bard_version() succeeded" 
         andFailureMessage:@"test: bard_version() FAILED"];
    
    [self reportTestResult:[self testInit] 
        withSuccessMessage:@"test: init_bard() succeeded" 
         andFailureMessage:@"test: init_bard() FAILED"];
    
    [self reportTestResult:[self testReadZero] 
        withSuccessMessage:@"test: bard_read(0) succeeded" 
         andFailureMessage:@"test: bard_read(0) FAILED"];
    
    [self reportTestResult:[self testReadTypes] 
        withSuccessMessage:@"test: reading several types succeeded" 
         andFailureMessage:@"test: reading several types FAILED"];
    
    [self reportTestResult:[self testIsNothing] 
        withSuccessMessage:@"test: testing for nothing succeeded" 
         andFailureMessage:@"test: testing for nothing FAILED"];
    
    [self reportTestResult:[self testIsEmpty] 
        withSuccessMessage:@"test: testing for empty succeeded" 
         andFailureMessage:@"test: testing for empty FAILED"];
    
    [self reportTestResult:[self testAsChar] 
        withSuccessMessage:@"test: as_char() succeeded" 
         andFailureMessage:@"test: as_char() FAILED"];
    
    [self reportTestResult:[self testAsBool] 
        withSuccessMessage:@"test: as_bool() succeeded" 
         andFailureMessage:@"test: as_bool() FAILED"];
    
    [self reportTestResult:[self testAsInt] 
        withSuccessMessage:@"test: as_int() succeeded" 
         andFailureMessage:@"test: as_int() FAILED"];
    
    [self reportTestResult:[self testAsFloat] 
        withSuccessMessage:@"test: as_float() succeeded" 
         andFailureMessage:@"test: as_float() FAILED"];
    
    [self reportTestResult:[self testAsString] 
        withSuccessMessage:@"test: as_string() succeeded" 
         andFailureMessage:@"test: as_string() FAILED"];
    
    [self reportTestResult:[self testOid] 
        withSuccessMessage:@"test: oid tests succeeded" 
         andFailureMessage:@"test: oid tests FAILED"];
    
    [self reportTestResult:[self testReadLines] 
        withSuccessMessage:@"test: reading several lines succeeded" 
         andFailureMessage:@"test: reading several lines FAILED"];
    
    [self reportTestResult:[self testReadNonEmptyLines] 
        withSuccessMessage:@"test: reading non-empty lines succeeded" 
         andFailureMessage:@"test: reading non-empty lines FAILED"];
    
    [self reportTestResult:[self testEval] 
        withSuccessMessage:@"test: (eval '(+ 1 2)) succeeded" 
         andFailureMessage:@"test: (eval '(+ 1 2)) FAILED"];
    
    [self reportTestResult:[self testLoadFromString] 
        withSuccessMessage:@"test: loading (begin (define x 5)(define y 6)(define z 7) y) succeeded" 
         andFailureMessage:@"test: loading (begin (define x 5)(define y 6)(define z 7) y) FAILED"];
}


@end
