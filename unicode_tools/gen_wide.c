#include <stdio.h>
#include <stdlib.h>
#include "common.h"

void process_line(char *buf, char *set)
{
	int a, b;
	char dummy;
    if (sscanf(buf, "%x..%x;%*[WF]%c", &a, &b, &dummy)==3)
        for (; a<=b; a++) set[a]=1;
    else if (sscanf(buf, "%x;%*[WF]%c", &a, &dummy)==2)
        set[a] = 1;
}

int main()
{
    char *data_file = "data/EastAsianWidth.txt";

    char *set = process_file(data_file, &process_line);

    gen_table(set, "wide");
}

