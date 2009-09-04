#include <sys/ioctl.h>

unsigned long c_get_window_size(void) {
	struct winsize w;
	if (ioctl (0, TIOCGWINSZ, &w) >= 0)
		return (w.ws_row << 16) + w.ws_col;
	else
		return 0x190050;
}
