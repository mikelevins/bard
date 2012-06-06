#import <Foundation/Foundation.h>

extern NSString* bard_version ();
extern bool init_bard();
extern NSMutableDictionary* bard_info (NSString* path);
extern bool bard_evaluate(NSString* text);

