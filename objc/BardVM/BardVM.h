//
//  BardVM.h
//  BardVM
//
//  Created by mikel evins on 8/17/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "BardContinuation.h"
#import "BardEnv.h"
#import "BardStore.h"
#import "BardValue.h"

#define VM_VERSION @"Bard VM version objc-0.1"

@interface BardVM : NSObject {
    BardValue* code;
    BardEnv* env;
    BardStore* store;
    BardContinuation* k;
}

+(NSString*)version;

@end
