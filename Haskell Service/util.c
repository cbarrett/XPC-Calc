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
