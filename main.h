#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*(x)))

#define BYTE(x) (* (volatile unsigned char *) (x))
#define WORD(x) (* (volatile unsigned short *) (x))
#define LONG(x) (* (volatile unsigned long *) (x))
