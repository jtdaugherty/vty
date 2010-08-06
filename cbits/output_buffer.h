#define _XOPEN_SOURCE 500
#include <unistd.h>
#include <stdint.h>

void stdout_output_buffer ( int out_fd, uint8_t* buffer, size_t buffer_size );
