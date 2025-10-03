#ifndef TORCH_TYPES_H
#define TORCH_TYPES_H

#include <stdlib.h>

enum torch_type_id
{
    torch_type_ByteStorage = 0,
    torch_type_CharStorage,
    torch_type_ShortStorage,
    torch_type_IntStorage,
    torch_type_LongStorage,
    torch_type_FloatStorage,
    torch_type_DoubleStorage,
    torch_type_HalfStorage,
    torch_type_ByteTensor,
    torch_type_CharTensor,
    torch_type_ShortTensor,
    torch_type_IntTensor,
    torch_type_LongTensor,
    torch_type_FloatTensor,
    torch_type_DoubleTensor,
    torch_type_HalfTensor,
    torch_type_Timer,
    torch_type_Generator,
    torch_type_Allocator,
    torch_type_size
};

struct torch_type_entry {
    const char *name;
    const enum torch_type_id id;
};

const struct torch_type_entry *
get_torch_type_entry(const char *str, size_t len);


#endif // TORCH_TYPES_H