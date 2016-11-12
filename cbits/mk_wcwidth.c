
#include <HsFFI.h>
#include <utf8proc.h>

HsInt vty_mk_wcwidth(HsChar ucs)
{
    return (HsInt) utf8proc_charwidth((utf8proc_int32_t) ucs);
}
