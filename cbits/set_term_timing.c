#include <termios.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

void vty_set_term_timing(int fd)
{
    struct termios trm;
    tcgetattr(fd, &trm);
    trm.c_cc[VMIN] = 0;
    trm.c_cc[VTIME] = 0;
    tcsetattr(fd, TCSANOW, &trm); 
}
