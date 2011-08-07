//
//  XPC_CalcAppDelegate.m
//  XPC Calc
//
//  Created by Colin Barrett on 8/6/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "XPC_CalcAppDelegate.h"
#import <xpc/xpc.h>

@implementation XPC_CalcAppDelegate

@synthesize messageTextField;
@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    serviceConnection = xpc_connection_create("com.springsandstruts.xpc-calc.xpc-calc-service", dispatch_get_main_queue());
    
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

- (IBAction)getMessage:(id)sender
{
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    xpc_connection_send_message_with_reply(serviceConnection, message, dispatch_get_main_queue(), ^ (xpc_object_t reply) {
        const char *messageCStr = xpc_dictionary_get_string(reply, "message");
        NSString *messageString = [NSString stringWithUTF8String:messageCStr];
        [[self messageTextField] setStringValue:messageString];
    });
}
    
@end
