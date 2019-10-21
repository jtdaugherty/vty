char* process_file(char *data_file, void (*process_line)(char *buf, char *set));

void gen_table(char set[], char* table_name);

void emoji_process_line(char *buf, char *set, char *desired_property);

