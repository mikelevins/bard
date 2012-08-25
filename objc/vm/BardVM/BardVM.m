//
//  BardVM.m
//  BardVM
//
//  Created by mikel evins on 8/22/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "BardVM.h"

@implementation BardVM

@synthesize pc;
@synthesize code;
@synthesize fn;

-(BardVM*)initWithFunction:(id)aFunction {
    self = [super init];
    if(self!=nil) {
        fn = aFunction;
    }
    return self;
}


@end
