//
//  main.m
//  TestBard
//
//  Created by mikel evins on 5/31/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#define ___VERSION 407002
#include "cbard.h"

#define SCHEME_LIBRARY_LINKER ____20_bard__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

int main(int argc, char *argv[])
{
	___setup_params_struct setup_params;
	___setup_params_reset (&setup_params);
	setup_params.version = ___VERSION;
	setup_params.linker  = SCHEME_LIBRARY_LINKER;
	___setup (&setup_params);
    
    @autoreleasepool {
    const char* vers = bard_version();
    NSLog(@"bard version: %@",vers);
    
    int retval = NSApplicationMain(argc, (const char **)argv);
    
    ___cleanup ();	
	return retval;
    }

}
