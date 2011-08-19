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

#define HASKELL_SERVICE "com.springsandstruts.xpc-calc.xpc-calc-service-hs"
#define OBJC_SERVICE "com.springsandstruts.xpc-calc.xpc-calc-service"


@interface XPC_CalcAppDelegate ()

@property (nonatomic) xpc_connection_t serviceConnection;
@property (nonatomic) xpc_object_t stack;

@end


@implementation XPC_CalcAppDelegate

@synthesize stackTextView;
@synthesize inputTextField;
@synthesize window;
@synthesize lastButton;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    [self clear:nil];
    self.useHaskellService = YES;
}


- (void)awakeFromNib
{
    NSDictionary *viewDict = NSDictionaryOfVariableBindings(lastButton);
    NSArray *constraints = [NSLayoutConstraint constraintsWithVisualFormat:@"[lastButton]-|"
                                                                   options:0
                                                                   metrics:nil
                                                                     views:viewDict];
    [self.window.contentView addConstraints:constraints];
}


- (void)setUpService:(const char *)serviceName
{
    xpc_connection_t connection = xpc_connection_create(serviceName, dispatch_get_main_queue());
    
    xpc_connection_set_event_handler(connection, ^(xpc_object_t event) {
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
                if (self.serviceConnection == connection) {
                    self.serviceConnection = NULL;
                }
            } else {
                // whoops
            }
        } else {
            // whoops
        }
    });
    xpc_connection_resume(connection);
    
    self.serviceConnection = connection;
    xpc_release(connection);
}


- (void)applicationWillTerminate:(NSNotification *)notification
{
    self.serviceConnection = NULL;
    self.stack = NULL;
}


- (void)setUseHaskellService:(BOOL)value
{
    useHaskellService = !!value;
    [self setUpService:value ? HASKELL_SERVICE : OBJC_SERVICE];
}


- (BOOL) useHaskellService
{
    return useHaskellService;
}


- (void)setServiceConnection:(xpc_connection_t)connection
{
    if (connection != serviceConnection) {
        if (serviceConnection != NULL) {
            xpc_connection_cancel(serviceConnection);
            xpc_release(serviceConnection);
        }
        if (connection) {
            serviceConnection = xpc_retain(connection);
        }
        else {
            serviceConnection = NULL;
        }
    }
}


- (xpc_connection_t)serviceConnection
{
    return serviceConnection;
}


- (void)updateStackView
{
    NSMutableString *stackString = [NSMutableString string];
    if (self.stack != NULL)
    {
        xpc_array_apply(stack, ^bool(size_t idx, xpc_object_t value) {
            [stackString insertString:[NSString stringWithFormat:@"%li\n", xpc_int64_get_value(value)] atIndex:0];
            return true;
        });
    }
    self.stackTextView.string = stackString;
}


- (void)setStack:(xpc_object_t)inStack
{
    if (stack != NULL)  xpc_release(stack);
    stack = NULL;
    
    if (inStack != NULL)
    {
        stack = xpc_retain(inStack);
    }
    
    [self updateStackView];
}


- (xpc_object_t)stack
{
    return stack;
}


- (IBAction)push:(id)sender
{
    if (![self.inputTextField.stringValue isEqualToString:@""]) {
        xpc_array_set_int64(self.stack, XPC_ARRAY_APPEND, self.inputTextField.integerValue);
        self.inputTextField.stringValue = @"";
    }
    [self updateStackView];
}


- (void)sendOperatorMessage:(int64_t)operator
{
    xpc_connection_t connection = self.serviceConnection;
    if (connection == NULL)  return;
    
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
    xpc_dictionary_set_int64(message, "op", operator);
    xpc_dictionary_set_value(message, "stack", self.stack);
    xpc_connection_send_message_with_reply(connection, message, dispatch_get_main_queue(), ^ (xpc_object_t reply) {
        xpc_type_t type = xpc_get_type(reply);
        if (type == XPC_TYPE_ERROR) {
            if (reply == XPC_ERROR_CONNECTION_INTERRUPTED) {
                NSLog(@"interrupted");
            } else if (reply == XPC_ERROR_CONNECTION_INVALID) {            
                NSLog(@"invalid");
            }
        } else if (type == XPC_TYPE_DICTIONARY) {            
            self.stack = xpc_dictionary_get_value(reply, "stack");
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
    self.stack = new_stack;
    xpc_release(new_stack);
}
    
@end
