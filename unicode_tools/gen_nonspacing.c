#include <stdio.h>
#include <stdlib.h>
#include "common.h"

void process_line(char *buf, char *set)
{
	int a, b;
	char dummy;
    if (sscanf(buf, "%x;%*[^;];M%*[en]%c", &a, &dummy)==2)
        set[a] = 1;
    else if (sscanf(buf, "%x;%*[^;];Cf%c", &a, &dummy)==2)
        set[a] = 1;
}

int main()
{
    char *data_file = "data/UnicodeData.txt";

    char *set = process_file(data_file, &process_line);

    set[0xad]=0;

    gen_table(set, "nonspacing");
}

