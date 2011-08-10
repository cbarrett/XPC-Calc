//
//  XPC_CalcAppDelegate.m
//  XPC Calc
//
//  Created by Colin Barrett on 8/6/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "XPC_CalcAppDelegate.h"
#import <xpc/xpc.h>
#import "Shared.h"

#define HASKELL_SERVICE 1

#if HASKELL_SERVICE
#define SERVICE_NAME "com.springsandstruts.xpc-calc.xpc-calc-service-hs"
#else
#define SERVICE_NAME "com.springsandstruts.xpc-calc.xpc-calc-service"
#endif

@implementation XPC_CalcAppDelegate

@synthesize stackTextView;
@synthesize inputTextField;
@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    serviceConnection = xpc_connection_create(SERVICE_NAME, dispatch_get_main_queue());
    
    xpc_connection_set_event_handler(serviceConnection, ^(xpc_object_t event) {
        xpc_type_t type = xpc_get_type(event);
        
        if (type == XPC_TYPE_ERROR) {
            if (event == XPC_ERROR_CONNECTION_INTERRUPTED) {
                // The service has either cancaled itself, crashed, or been
                // terminated.  The XPC connection is still valid and sending a
                // message to it will re-launch the service.  If the service is
                // state-full, this is the time to initialize the new service.
            } else if (event == XPC_ERROR_CONNECTION_INVALID) {            
                // The service is invalid. Either the service name supplied to
                // xpc_connection_create() is incorrect or we (this process) have
                // canceled the service; we can do any cleanup of appliation
                // state at this point.
                NSLog(@"Uh oh, connection invalid");
                xpc_release(serviceConnection);
                serviceConnection = nil;
                xpc_release(stack);
                stack = NULL;
            } else {
                // whoops
            }
        } else {
            // whoops
        }
    });
    xpc_connection_resume(serviceConnection);
    stack = xpc_array_create(NULL, 0);
}

- (void)applicationWillTerminate:(NSNotification *)notification
{
    xpc_connection_cancel(serviceConnection);
    xpc_release(serviceConnection);
    serviceConnection = nil;
    
    xpc_release(stack);
    stack = NULL;
}

- (void)updateStackView
{
    NSMutableString *stackString = [NSMutableString string];
    xpc_array_apply(stack, ^ (size_t idx, xpc_object_t value) {
        [stackString insertString:[NSString stringWithFormat:@"%li\n", xpc_int64_get_value(value)] atIndex:0];
        return (bool)true;
    });
    [[self stackTextView] setString:stackString];
}

- (void)setStack:(xpc_object_t)inStack
{
    stack = xpc_retain(inStack);
    [self updateStackView];
}

- (IBAction)push:(id)sender
{
    if (![[[self inputTextField] stringValue] isEqualToString:@""]) {
        xpc_array_set_int64(stack, XPC_ARRAY_APPEND, [[self inputTextField] integerValue]);
        [[self inputTextField] setStringValue:@""];
    }
    [self updateStackView];
}

- (void)sendOperatorMessage:(int64_t)operator
{
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    xpc_dictionary_set_int64(message, "op", operator);
    xpc_dictionary_set_value(message, "stack", stack);
    xpc_connection_send_message_with_reply(serviceConnection, message, dispatch_get_main_queue(), ^ (xpc_object_t reply) {
        xpc_type_t type = xpc_get_type(reply);
        if (type == XPC_TYPE_ERROR) {
            if (reply == XPC_ERROR_CONNECTION_INTERRUPTED) {
                NSLog(@"interrupted");
            } else if (reply == XPC_ERROR_CONNECTION_INVALID) {            
                NSLog(@"invalid");
            }
        } else if (type == XPC_TYPE_DICTIONARY) {            
            [self setStack:xpc_dictionary_get_value(reply, "stack")];
        }
    });
    xpc_release(message);

}

- (IBAction)add:(id)sender
{
    [self sendOperatorMessage:OperatorAdd];
}

- (IBAction)subtract:(id)sender
{
    [self sendOperatorMessage:OperatorSub];
}

- (IBAction)multiply:(id)sender
{
    [self sendOperatorMessage:OperatorMul];
}

- (IBAction)divide:(id)sender
{
    [self sendOperatorMessage:OperatorDiv];
}

- (IBAction)clear:(id)sender
{
    xpc_object_t new_stack = xpc_array_create(NULL, 0);
    [self setStack:new_stack];
    xpc_release(new_stack);
}
    
@end
