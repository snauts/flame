#define BIT(x) (1 << (x))

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*(x)))

typedef unsigned char byte;
typedef unsigned short u16;
typedef unsigned int u32;

#define BYTE(x) (* (volatile byte *) (x))
#define WORD(x) (* (volatile u16 *) (x))
#define LONG(x) (* (volatile u32 *) (x))
