#ifndef _INTTYPES2_H
#define _INTTYPES2_H

#include <inttypes.h>
#ifndef PRIuFAST32
#define PRIuFAST32 "u"
#endif
#ifndef PRIuFAST16
#define PRIuFAST16 "u"
#endif
#ifndef PRIxFAST32
#define PRIxFAST32 "x"
#endif
#ifndef PRId32
#define PRId32 "d"
#endif
#ifndef PRIu32
#define PRIu32 "u"
#endif
#ifndef PRIx32
#define PRIx32 "x"
#endif
#ifndef PRIX32
#define PRIX32 "X"
#endif
#ifndef PRIxPTR
#define PRIxPTR "lx"
#endif

typedef uint_fast32_t line_t;
#define PRIuline PRIuFAST32
#define PRIxline PRIxFAST32
typedef uint_fast32_t address_t;
#define PRIaddress PRIxFAST32
typedef uint_fast16_t linecpos_t;
typedef struct {uint_fast16_t pos, upos;} linepos_t;
#define PRIlinepos PRIuFAST16
#endif
