//
//  File.c
//  XPC Calc
//
//  Created by Colin Barrett on 8/7/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#include "main.h"
#include <stdio.h>
#include "HsFFI.h"
#include "HsXPC_stub.h"

extern void __stginit_HsXPC();

void hsxpc_dictionary_get_keys_and_values(xpc_object_t dictionary, const char **keys, xpc_object_t *values) {    
    __block size_t idx = 0;
    xpc_dictionary_apply(dictionary,  ^ bool (const char *key, xpc_object_t value) {
        keys[idx] = key;
        values[idx] = value;
        ++idx;
        return true;
    });
}

static void XPC_Calc_Service_event_handler(xpc_connection_t peer) 
{    
	// By defaults, new connections will target the default dispatch
	// concurrent queue.
	xpc_connection_set_event_handler(peer, ^(xpc_object_t event) {
		hsEventHandler(peer, event);
	});
	
	// This will tell the connection to begin listening for events. If you
	// have some other initialization that must be done asynchronously, then
	// you can defer this call until after that initialization is done.
	xpc_connection_resume(peer);
}

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    hs_add_root(__stginit_HsXPC);
    
	xpc_main(XPC_Calc_Service_event_handler);
    
    hs_exit();
	return 0;
}
