//
//  BardAppDelegate.h
//  Bard
//
//  Created by mikel on 11/9/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@class BardViewController;

@interface BardAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    BardViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet BardViewController *viewController;

@end

