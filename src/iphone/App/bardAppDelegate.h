//
//  bardAppDelegate.h
//  bard
//
//  Created by mikel on 4/6/10.
//  Copyright __MyCompanyName__ 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

@class bardViewController;

@interface bardAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    bardViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet bardViewController *viewController;

@end

