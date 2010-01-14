#define _XOPEN_SOURCE 500
#include <unistd.h>

#include "output_buffer.h"

void stdout_output_buffer ( uint8_t* buffer, size_t buffer_size )
{
    while ( buffer_size )
    {
        const ssize_t r = write( STDOUT_FILENO, (void*) buffer, buffer_size );

        buffer_size -= (size_t) r;
        buffer += (size_t) r;
    }

    fdatasync( STDOUT_FILENO );
}

