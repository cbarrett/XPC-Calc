#include <xpc/xpc.h>

void hsxpc_dictionary_get_keys_and_values(xpc_object_t dictionary, const char **keys, xpc_object_t *values);

typedef void (*hsxpc_handler_funptr_t)(xpc_object_t object);
void hsxpc_connection_set_event_handler_f(xpc_connection_t peer, hsxpc_handler_funptr_t);
