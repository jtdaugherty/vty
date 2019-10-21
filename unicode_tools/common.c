#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"

#define STEP 0x100

char* process_file(char *data_file, void (*process_line)(char *buf, char *set))
{
	char *set = calloc(0x110000,1);
	char buf[128];
	FILE *f;

	f = fopen(data_file, "rb");
	while (fgets(buf, sizeof buf, f))
        (*process_line)(buf, set);
	fclose(f);

    return set;
}

void gen_table(char set[], char *table_name)
{
    printf("static const unsigned char %s[] = {\n", table_name);
	int blocks_needed=0;
	int a, b;
	for (a=0; a<0x20000; a+=STEP) {
		for (b=0; b<STEP; b++)
			if (set[a+b]!=set[a]) break;
		printf("%d, ", b!=STEP ? 18+blocks_needed++ : 16+!!set[a]);
	}
	for (a=0; a<32; a++) printf("0, ");
	for (a=0; a<32; a++) printf("255, ");
	for (a=0; a<0x20000; a+=STEP) {
		for (b=0; b<STEP; b++)
			if (set[a+b]!=set[a]) break;
		unsigned x=0;
		if (b!=STEP) for (b=0; b<STEP; b++) {
			x=x/2+128*!!set[a+b];
			if (!(b+1&7)) printf("%d, ", x&255);
		}
	}
    printf("\n};\n");
}

void emoji_process_line(char *buf, char *set, char *desired_property)
{
	int a, b;
    char *property;
    if (sscanf(buf, "%x..%x ; %m[a-zA-Z_]", &a, &b, &property)==3) {
        if (strcmp(property, desired_property) == 0) {
            for (; a<=b; a++) {
                if (a < 0x20000) {
                    set[a] = 1;
                }
            }
        }
        free(property);
    } else if (sscanf(buf, "%x ; %m[a-zA-Z_]", &a, &property)==2) {
        if (strcmp(property, desired_property) == 0) {
            if (a < 0x20000) {
                set[a] = 1;
            }
        }
        free(property);
    }
}

