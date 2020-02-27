#include <termios.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

// Given a file descriptor for a terminal, get the ERASE character for
// the terminal. If the terminal info cannot be obtained, this returns
// zero.
char vty_get_tty_erase(int fd)
{
    struct termios trm;

    if (0 == tcgetattr(fd, &trm)) {
        return (char) trm.c_cc[VERASE];
    } else {
        return (char) 0;
    }
}
