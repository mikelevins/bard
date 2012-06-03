#import <Foundation/Foundation.h>

extern NSString* bard_version ();
extern NSMutableArray* list_files (NSString* path);
extern bool bard_load (NSString* path);
extern NSNumber* count_files (NSString* path);
extern NSMutableDictionary* bard_info (NSString* path);

