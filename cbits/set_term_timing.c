#include <termios.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

void vty_set_term_timing(void)
{
    struct termios trm;
    tcgetattr(STDIN_FILENO, &trm);
    trm.c_cc[VMIN] = 0;
    trm.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSANOW, &trm); 
}

