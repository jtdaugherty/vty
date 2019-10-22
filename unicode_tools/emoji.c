#include <wchar.h>
#include "emoji.h"
#include "extended_pictographic.h"

int is_emoji(wchar_t c) {
    if (c >= L'\x20000')
        return 0;
    return ((emoji[emoji[c>>8]*32+((c&255)>>3)]>>(c&7))&1);
}

int is_extended_pictographic(wchar_t c) {
    if (c >= L'\x20000')
        return 0;
    return ((extended_pictographic[extended_pictographic[c>>8]*32+((c&255)>>3)]>>(c&7))&1);
}

