#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*(x)))

#define BIT(x) (1 << (x))

#define BYTE(x) (* (volatile unsigned char *) (x))
#define WORD(x) (* (volatile unsigned short *) (x))
#define LONG(x) (* (volatile unsigned long *) (x))
