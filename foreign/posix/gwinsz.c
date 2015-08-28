#include <sys/ioctl.h>

unsigned long vty_c_get_window_size(int fd) {
    struct winsize w;
    if (ioctl (fd, TIOCGWINSZ, &w) >= 0)
        return (w.ws_row << 16) + w.ws_col;
    else
        return 0x190050;
}
