#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"

void process_line(char *buf, char *set)
{
    emoji_process_line(buf, set, "Emoji");
}

int main()
{
    char *data_file = "data/emoji-data.txt";

    char *set = process_file(data_file, &process_line);

    gen_table(set, "emoji");
}

