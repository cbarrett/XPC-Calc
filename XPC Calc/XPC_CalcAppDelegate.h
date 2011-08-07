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
    NSTextField *messageTextField;
    xpc_connection_t serviceConnection;
}

@property (assign) IBOutlet NSTextField *messageTextField;
@property (assign) IBOutlet NSWindow *window;

- (IBAction)getMessage:(id)sender;

@end
