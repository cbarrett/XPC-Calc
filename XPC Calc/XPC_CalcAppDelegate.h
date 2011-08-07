//
//  XPC_CalcAppDelegate.h
//  XPC Calc
//
//  Created by Colin Barrett on 8/6/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <xpc/xpc.h>

@interface XPC_CalcAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
    NSTextField *inputTextField;
    xpc_connection_t serviceConnection;
    NSTextView *stackTextView;
}
@property (assign) IBOutlet NSTextView *stackTextView;
@property (assign) IBOutlet NSTextField *inputTextField;
@property (assign) IBOutlet NSWindow *window;

- (IBAction)push:(id)sender;
- (IBAction)add:(id)sender;
- (IBAction)subtract:(id)sender;
- (IBAction)multiply:(id)sender;
- (IBAction)divide:(id)sender;
- (IBAction)clear:(id)sender;

@end
