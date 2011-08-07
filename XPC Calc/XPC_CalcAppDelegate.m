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

@implementation XPC_CalcAppDelegate

@synthesize stackTextView;
@synthesize inputTextField;
@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    serviceConnection = xpc_connection_create("com.springsandstruts.xpc-calc.xpc-calc-service-hs", dispatch_get_main_queue());
    
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
                xpc_release(serviceConnection);
                serviceConnection = nil;
            } else {
                // whoops
            }
        } else {
            // whoops
        }
    });
    xpc_connection_resume(serviceConnection);
}

- (void)applicationWillTerminate:(NSNotification *)notification
{
    xpc_connection_cancel(serviceConnection);
    xpc_release(serviceConnection);
    serviceConnection = nil;
}

- (void)setStack:(xpc_object_t)stack
{
    NSMutableString *stackString = [NSMutableString string];
    for (size_t i = 0; i < xpc_array_get_count(stack); i++) {
        int64_t num = xpc_array_get_int64(stack, i);
        [stackString appendFormat:@"%li\n", num];
    }
    [[self stackTextView] setString:stackString];
}

- (IBAction)push:(id)sender
{
    
#if 0
    if (![[inputTextField stringValue] isEqualToString:@""]) {
        xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
        xpc_dictionary_set_int64(message, "type", MessagePush);
        xpc_dictionary_set_int64(message, "num", [inputTextField integerValue]);
        xpc_connection_send_message_with_reply(serviceConnection, message, dispatch_get_main_queue(), ^ (xpc_object_t reply) {
            [self setStack:xpc_dictionary_get_value(reply, "stack")];
            [[self inputTextField] setStringValue:@""];
        });
        xpc_release(message);
    }
#else
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    xpc_connection_send_message_with_reply(serviceConnection, message, dispatch_get_main_queue(), ^ (xpc_object_t reply) {
        NSLog(@"%s", xpc_dictionary_get_string(reply, "message"));
    });
    xpc_release(message);
#endif
    
}

- (void)sendOperatorMessage:(int64_t)operator
{
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    xpc_dictionary_set_int64(message, "type", MessageOperator);
    xpc_dictionary_set_int64(message, "op", operator);
    xpc_connection_send_message_with_reply(serviceConnection, message, dispatch_get_main_queue(), ^ (xpc_object_t reply) {
        [self setStack:xpc_dictionary_get_value(reply, "stack")];
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
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    xpc_dictionary_set_int64(message, "type", MessageOperator);
    xpc_connection_send_message(serviceConnection, message);
    [[self stackTextView] setString:@""];
    xpc_release(message);
}
    
@end
