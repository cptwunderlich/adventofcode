#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

    if (argc != 2) {
        puts("Erro: No file");
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL) {
        puts("Error\n");
        return 1;
    }

    if (feof(file)) {
        puts("Error: Empty file\n");
        return 1;
    }

    char line[7];
    fgets(line, 7, file);
    int last = atoi(line);
    int cur = 0, cnt = 0;

    while (fgets(line, 7, file)) {
        cur = atoi(line);

        if (last < cur) cnt++;

        last = cur;
    }

    printf("Part 1: %d\n", cnt);

    return 0;
}

