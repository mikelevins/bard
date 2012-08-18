//
//  BardValue.h
//  TestBardVM
//
//  Created by mikel evins on 8/17/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

// ----------------------------------------------------------------------
// Abstract Classes
// ----------------------------------------------------------------------
#pragma mark -
#pragma mark Abstract Classes

@interface BardValue : NSObject
@end

@interface BardAtom : BardValue
@end

@interface BardCollection : BardValue
@end

// ----------------------------------------------------------------------
//  Protocols
// ----------------------------------------------------------------------
#pragma mark -
#pragma mark Protocols

@protocol BardName
@end

@protocol BardSequence 
@end

@protocol BardTable 
@end

@protocol BardApplicable 
@end

// ----------------------------------------------------------------------
// Types
// ----------------------------------------------------------------------
#pragma mark -
#pragma mark Types

@interface BardType : BardValue
@end

@interface BardSchema : BardType
@end

@interface BardClass : BardType <BardName>
@end

@interface BardProtocol : BardType
@end

@interface BardSingleton : BardType
@end


// ----------------------------------------------------------------------
//  Atoms
// ----------------------------------------------------------------------
#pragma mark -
#pragma mark Atoms

@interface BardUndefined : BardAtom
@end

@interface BardNull : BardAtom
@end

@interface BardCharacter : BardAtom
@end

@interface BardBoolean : BardAtom
@end

@interface BardInteger : BardAtom
@end

@interface BardFloat : BardAtom
@end

@interface BardRatio : BardAtom
@end

@interface BardSymbol : BardAtom <BardName>
@end

@interface BardKeyword : BardAtom <BardName>
@end

@interface BardFunction : BardAtom <BardApplicable>
@end

@interface BardMethod : BardAtom <BardApplicable>
@end

@interface BardPrimitive : BardAtom <BardApplicable>
@end


// ----------------------------------------------------------------------
//  Collections
// ----------------------------------------------------------------------
#pragma mark -
#pragma mark Collections

@interface BardList : BardCollection <BardSequence>
@end

@interface BardFrame : BardCollection <BardTable>
@end

