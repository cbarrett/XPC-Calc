#include <xpc/xpc.h>

void hsxpc_dictionary_get_keys_and_values(xpc_object_t dictionary, const char **keys, xpc_object_t *values);
void hsxpc_release_and_log(xpc_object_t obj);