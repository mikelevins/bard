//
//  bardAppDelegate.m
//  bard
//
//  Created by mikel on 4/6/10.
//  Copyright __MyCompanyName__ 2010. All rights reserved.
//

#import "bardAppDelegate.h"
#import "bardViewController.h"

@implementation bardAppDelegate

@synthesize window;
@synthesize viewController;


- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {    
    
    // Override point for customization after app launch    
    [window addSubview:viewController.view];
    [window makeKeyAndVisible];

	return YES;
}


- (void)dealloc {
    [viewController release];
    [window release];
    [super dealloc];
}


@end
