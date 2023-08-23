#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <arpa/inet.h>

int main(int argc, char **argv) {
    unsigned short word, sum = 0;
    if (argc < 2) {
	printf("usage: cksum [ROM-file]\n");
	return 0;
    }
    int fd = open(argv[1], O_RDWR);
    if (fd < 0) {
	printf("ROM-file \"%s\" does not exist\n", argv[1]);
	return -ENOENT;
    }
    lseek(fd, 0x200, SEEK_SET);
    while (read(fd, &word, sizeof(word)) == sizeof(word)) {
	sum += htons(word);
    }
    printf("cksum[0x%04x]\n", sum);
    lseek(fd, 0x18E, SEEK_SET);
    sum = htons(sum);
    write(fd, &sum, 2);
    return 0;
}
