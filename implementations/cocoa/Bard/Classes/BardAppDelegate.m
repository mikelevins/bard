//
//  BardAppDelegate.m
//  Bard
//
//  Created by mikel on 11/1/10.
//  Copyright (c) 2010 __MyCompanyName__. All rights reserved.
//

#import "BardAppDelegate.h"

@implementation BardAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    // Insert code here to initialize your application
}

- (void)dealloc {

    [window release];
    [super dealloc];
}

@end
