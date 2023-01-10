/*
    $Id: wchar.h 2966 2023-01-08 21:28:13Z soci $

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

#ifndef WCHAR_H
#define WCHAR_H
#include "inttypes.h"

#if defined __DJGPP__ || (defined __WATCOMC__ && !defined _WIN32)
#include <wchar.h>
extern size_t wcrtomb(char *, wchar_t, mbstate_t *);
extern size_t mbrtowc(wchar_t *, const char *, size_t, mbstate_t *);
#elif defined __GNUC__ || defined _MSC_VER || defined __WATCOMC__
#include <wchar.h>
#elif __STDC_VERSION__ >= 199901L && !defined __VBCC__
#include <wchar.h>
#else
typedef uint32_t wint_t;

typedef struct {
  int shift_state;
} mbstate_t;

extern size_t wcrtomb(char *, wchar_t, mbstate_t *);
extern size_t mbrtowc(wchar_t *, const char *, size_t, mbstate_t *);
#endif

extern int wcwidth_v13(unichar_t);
extern int isprint_v13(unichar_t);

#endif
