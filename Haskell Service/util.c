#include "main.h"

void hsxpc_dictionary_get_keys_and_values(xpc_object_t dictionary, const char **keys, xpc_object_t *values)
{    
    __block size_t idx = 0;
    xpc_dictionary_apply(dictionary,  ^ bool (const char *key, xpc_object_t value) {
        keys[idx] = key;
        values[idx] = value;
        ++idx;
        return true;
    });
}

void hsxpc_connection_set_event_handler_f(xpc_connection_t peer, hsxpc_handler_funptr_t f)
{
    xpc_connection_set_event_handler(peer, ^ (xpc_object_t obj) { f(obj); });
}
