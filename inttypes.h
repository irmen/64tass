/*
    $Id$

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

*/
#ifndef _INTTYPES2_H
#define _INTTYPES2_H

#include <unistd.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
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
#define PRIxPTR "x"
#endif

#ifndef PRIxSIZE
#ifdef _WIN32
#define PRIxSIZE "x"
#else
#define PRIxSIZE "zx"
#endif
#endif

typedef uint32_t line_t;
#define PRIuline PRIu32
#define PRIxline PRIx32
typedef uint32_t address_t;
#define PRIaddress PRIx32
typedef uint32_t linecpos_t;
struct linepos_s {line_t line;linecpos_t pos;};
#define PRIlinepos PRIu32
typedef const struct linepos_s *linepos_t;
typedef struct {size_t len;const uint8_t *data;} str_t;
typedef int32_t ival_t;
#define PRIdval PRId32
typedef uint32_t uval_t;
#define PRIxval PRIx32
#define PRIuval PRIu32
#define PRIXval PRIX32

#ifndef SSIZE_MAX
# define SSIZE_MAX ((ssize_t)(SIZE_MAX / 2))
#endif

typedef struct value_s *value_t;
typedef const struct obj_s *obj_t;

#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif

#ifdef MUST_CHECK
#elif defined(__GNUC__)
#define MUST_CHECK __attribute__ ((warn_unused_result))
#define NO_RETURN  __attribute__((noreturn))
#else
#define MUST_CHECK
#define NO_RETURN
#endif
#endif
