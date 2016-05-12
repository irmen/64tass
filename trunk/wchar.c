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
#include "wchar.h"

#ifdef __DJGPP__
#include <errno.h>
#include "inttypes.h"
#include <ctype.h>
#include "wctype.h"
#include <dpmi.h>
#include <go32.h>
#include "codepages.h"

static const wchar_t *cp;
static uint32_t revcp[128];

static int compcp(const void *a, const void *b) {
    return *(uint32_t *)a - *(uint32_t *)b;
}

static int compcp2(const void *a, const void *b) {
    return (*(uint32_t *)a & ~0xff) - (*(uint32_t *)b & ~0xff);
}

static void set_cp(void) {
    uint16_t dcp;
    int i;
    __dpmi_regs regs;
    regs.x.ax = 0x440c;
    regs.x.bx = 1;
    regs.x.cx = 0x36a;
    regs.x.ds = __tb >> 4;
    regs.x.dx = __tb & 0xf;
    regs.x.flags |= 1;
    __dpmi_int(0x21, &regs);
    if (regs.x.flags & 1) {
        regs.x.ax = 0xad02;
        regs.x.bx = 0xfffe;
        __dpmi_int(0x2f, &regs);
        dcp = (regs.x.flags & 1) ? 0 : regs.x.bx;
    } else {
        dosmemget(__tb + 2, sizeof(dcp), &dcp);
    }
    switch (dcp) {
    case 737: cp = cp737; break;
    case 775: cp = cp775; break;
    case 850: cp = cp850; break;
    case 852: cp = cp852; break;
    case 855: cp = cp855; break;
    case 857: cp = cp857; break;
    case 860: cp = cp860; break;
    case 861: cp = cp861; break;
    case 862: cp = cp862; break;
    case 863: cp = cp863; break;
    case 864: cp = cp864; break;
    case 865: cp = cp865; break;
    case 866: cp = cp866; break;
    case 869: cp = cp869; break;
    case 874: cp = cp874; break;
    default:
    case 437: cp = cp437; break;
    }
    for (i = 0; i < 128; i++) {
        revcp[i] = (cp[i] << 8) | i | 0x80;
    }
    qsort(revcp, sizeof(revcp)/sizeof(revcp[0]), sizeof(revcp[0]), compcp);
}

int iswprint(wint_t wc) {
    uint32_t *ch, c;
    if (wc < 0x80) return isprint(wc);
    if (cp == NULL) set_cp();
    c = wc << 8;
    ch = (uint32_t *)bsearch(&c, revcp, sizeof(revcp)/sizeof(revcp[0]), sizeof(revcp[0]), compcp2);
    return (ch != NULL) ? 1 : 0;
}

size_t mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *UNUSED(ps)) {
    uint8_t ch;
    wchar_t w;
    if (n == 0) return (size_t)-2;
    ch = *s;
    if (ch < 0x80) {
        if (!ch) return 0;
        *wc = ch;
        return 1;
    }
    if (cp == NULL) set_cp();
    *wc = w = cp[ch - 0x80];
    if (w != 0) return 1;
    errno = EILSEQ;
    return (size_t)-1;
}

size_t wcrtomb(char *s, wchar_t wc, mbstate_t *UNUSED(ps)) {
    uint32_t *ch, c;
    if (wc < 0x80) {
        *s = wc;
        return 1;
    }
    if (cp == NULL) set_cp();
    c = wc << 8;
    ch = (uint32_t *)bsearch(&c, revcp, sizeof(revcp)/sizeof(revcp[0]), sizeof(revcp[0]), compcp2);
    if (ch != NULL) {
        *s = *ch;
        return 1;
    }
    errno = EILSEQ;
    return (size_t)-1;
}
#elif defined __GNUC__ || defined _MSC_VER
#elif __STDC_VERSION__ >= 199901L && !defined __VBCC__
#else
#include <errno.h>

size_t mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *UNUSED(ps)) {
    uint8_t ch;
    if (n == 0) return (size_t)-2;
    ch = *s;
    if (ch < 0x80) {
        if (!ch) return 0;
        *wc = ch;
        return 1;
    }
    if (ch < 0xa0) {
#ifdef EILSEQ
        errno = EILSEQ;
#endif
        return (size_t)-1;
    }
    *wc = ch;
    return 1;
}

size_t wcrtomb(char *s, wchar_t wc, mbstate_t *UNUSED(ps)) {
    uint32_t *ch, c;
    if (wc < 0x80 || (wc >= 0xa0 && wc <= 0xff)) {
        *s = wc;
        return 1;
    }
#ifdef EILSEQ
    errno = EILSEQ;
#endif
    return (size_t)-1;
}
#endif
