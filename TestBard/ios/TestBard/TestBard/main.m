//
//  main.m
//  TestBard
//
//  Created by mikel evins on 5/31/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "AppDelegate.h"
#define ___VERSION 406006
#include "gambit.h"
#include "bard.h"

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

    
    int retval;
    @autoreleasepool {
        //NSString* bardVersion = bard_version();
        //NSLog(@"%@",bardVersion);
        retval=UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
//    ___cleanup ();	
	return retval;
}
