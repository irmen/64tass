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
#ifndef _WIN32
#include <wchar.h>
#endif
#include <wctype.h>
#include "unicode.h"
#include "error.h"

#define U_CASEFOLD 1
#define U_COMPAT 2

unsigned int utf8in(const uint8_t *c, uint32_t *out) { /* only for internal use with validated utf-8! */
    uint32_t ch;
    int i, j;
    ch = c[0];

    if (ch < 0xe0) {
        ch ^= 0xc0;i = 2;
    } else if (ch < 0xf0) {
        ch ^= 0xe0;i = 3;
    } else if (ch < 0xf8) {
        ch ^= 0xf0;i = 4;
    } else if (ch < 0xfc) {
        ch ^= 0xf8;i = 5;
    } else {
        ch ^= 0xfc;i = 6;
    }

    for (j = 1;j < i; j++) {
        ch = (ch << 6) ^ c[j] ^ 0x80;
    }
    *out = ch;
    return i;
}

unsigned int utf8rin(const uint8_t *c, uint32_t *out) { /* only for internal use with validated utf-8! */
    uint32_t ch;
    int i, j;

    if (c[-2] < 0xe0) {
        ch = c[-2] ^ 0xc0;i = 2;
    } else if (c[-3] < 0xf0) {
        ch = c[-3] ^ 0xe0;i = 3;
    } else if (c[-4] < 0xf8) {
        ch = c[-4] ^ 0xf0;i = 4;
    } else if (c[-5] < 0xfc) {
        ch = c[-5] ^ 0xf8;i = 5;
    } else {
        ch = c[-6] ^ 0xfc;i = 6;
    }

    c -= i;
    for (j = 1;j < i; j++) {
        ch = (ch << 6) ^ c[j] ^ 0x80;
    }
    *out = ch;
    return i;
}

uint8_t *utf8out(uint32_t i, uint8_t *c) {
    if (i < 0x800) {
        *c++=0xc0 | (i >> 6);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x10000) {
        *c++=0xe0 | (i >> 12);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x200000) {
        *c++=0xf0 | (i >> 18);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x4000000) {
        *c++=0xf8 | (i >> 24);
        *c++=0x80 | ((i >> 18) & 0x3f);
        *c++=0x80 | ((i >> 12) & 0x3f);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i & ~0x7fffffff) return c;
    *c++=0xfc | (i >> 30);
    *c++=0x80 | ((i >> 24) & 0x3f);
    *c++=0x80 | ((i >> 18) & 0x3f);
    *c++=0x80 | ((i >> 12) & 0x3f);
    *c++=0x80 | ((i >> 6) & 0x3f);
    *c++=0x80 | (i & 0x3f);
    return c;
}

static void extbuff(struct ubuff_s *d) {
    d->len += 16;
    d->data = (uint32_t *)realloc(d->data, d->len * sizeof(uint32_t));
    if (!d->data) err_msg_out_of_memory();
}

static void udecompose(uint32_t ch, struct ubuff_s *d, int options) {
    const struct properties_s *prop;
    if (ch >= 0xac00 && ch <= 0xd7a3) {
        uint32_t ht, hs = ch - 0xac00;
        if (d->p >= d->len) extbuff(d);
        d->data[d->p++] = 0x1100 + hs / 588;
        if (d->p >= d->len) extbuff(d);
        d->data[d->p++] = 0x1161 + (hs % 588) / 28;
        ht = hs % 28;
        if (ht) {
            if (d->p >= d->len) extbuff(d);
            d->data[d->p++] = 0x11a7 + ht;
        }
        return;
    }
    prop = uget_property(ch);
    if ((options & U_CASEFOLD) && prop->casefold) {
        if (prop->casefold > 0) {
            if (d->p >= d->len) extbuff(d);
            d->data[d->p++] = prop->casefold;
            return;
        } else if (prop->casefold > -16384) {
            const int16_t *p = &usequences[-prop->casefold];
            for (;;) {
                if (d->p >= d->len) extbuff(d);
                d->data[d->p++] = abs(*p);
                if (*p < 0) return;
                p++;
            }
        } else {
            const int32_t *p = &usequences2[-prop->casefold - 16384];
            for (;;) {
                if (d->p >= d->len) extbuff(d);
                d->data[d->p++] = abs(*p);
                if (*p < 0) return;
                p++;
            }
        }
    }
    if (prop->decompose) {
        if (!(prop->property & pr_compat) || (options & U_COMPAT)) {
            if (prop->decompose > 0) {
                return udecompose(prop->decompose, d, options);
            } else if (prop->decompose > -16384) {
                const int16_t *p = &usequences[-prop->decompose];
                for (;;) {
                    udecompose(abs(*p), d, options);
                    if (*p < 0) return;
                    p++;
                }
            } else {
                const int32_t *p = &usequences2[-prop->decompose - 16384];
                for (;;) {
                    udecompose(abs(*p), d, options);
                    if (*p < 0) return;
                    p++;
                }
            }
        }
    }
    if (d->p >= d->len) extbuff(d);
    d->data[d->p++] = ch;
}

static void unormalize(struct ubuff_s *d) {
    size_t pos, max;
    if (d->p < 2) return;
    pos = 0;
    max = d->p - 1;
    while (pos < max) {
        uint32_t ch1, ch2;
        uint8_t cc1, cc2;
        ch2 = d->data[pos + 1];
        cc2 = uget_property(ch2)->combclass;
        if (cc2) {
            ch1 = d->data[pos];
            cc1 = uget_property(ch1)->combclass;
            if (cc1 > cc2) {
                d->data[pos] = ch2;
                d->data[pos + 1] = ch1;
                if (pos) {
                    pos--;
                    continue;
                }
            }
        } 
        pos++;
    }
}

static void ucompose(const struct ubuff_s *buff, struct ubuff_s *d) {
    const struct properties_s *prop, *sprop = NULL;
    uint32_t ch;
    int mclass = -1;
    size_t i, sp = (size_t)-1;
    d->p = 0;
    for (i = 0; i < buff->p; i++) {
        ch = buff->data[i];
        prop = uget_property(ch);
        if (sp != (size_t)-1 && prop->combclass > mclass) {
            uint32_t sc = d->data[sp];
            if (sc >= 0xac00) {
                uint32_t hs = sc - 0xac00;
                if (hs < 588*19 && (hs % 28) == 0) {
                    if (ch >= 0x11a7 && ch < 0x11a7+28) {
                        d->data[sp] = sc + ch - 0x11a7;
                        sprop = NULL;
                        continue;
                    }
                }
            } else if (sc >= 0x1100 && sc < 0x1100+19 && ch >= 0x1161 && ch < 0x1161+21) {
                d->data[sp] = 0xac00 + (ch - 0x1161 + (sc - 0x1100) * 21) * 28;
                sprop = NULL;
                continue;
            }
            if (!sprop) sprop = uget_property(sc);
            if (sprop->base >= 0 && prop->diar >= 0) {
                int16_t comp = ucomposing[sprop->base + prop->diar];
                if (comp) {
                    d->data[sp] = (comp > 0) ? (uint16_t)comp : ucomposed[-comp];
                    sprop = NULL;
                    continue;
                }
            }
        }
        if (prop->combclass) {
            if (prop->combclass > mclass) {
                mclass = prop->combclass;
            }
        } else {
            sp = d->p;
            sprop = prop;
            mclass = -1;
        }
        if (d->p >= d->len) extbuff(d);
        d->data[d->p++] = ch;
    }
}

void unfc(struct ubuff_s *b) {
    size_t i;
    static struct ubuff_s dbuf;
    if (!b) return free(dbuf.data);
    for (dbuf.p = i = 0; i < b->p; i++) {
        udecompose(b->data[i], &dbuf, 0);
    }
    unormalize(&dbuf);
    ucompose(&dbuf, b);
}

void printable_print(const uint8_t *line, FILE *f) {
    size_t i = 0, l = 0;
    for (;;) {
        uint32_t ch = line[i];
        if (ch & 0x80) {
#if _WIN32
            i += utf8in(line + i, &ch);
            if (iswprint(ch)) continue;
            if (l != i) fwrite(line + l, i - l, 1, f);
            fprintf(f, "{$%x}", ch);
#else
            if (l != i) fwrite(line + l, i - l, 1, f);
            i += utf8in(line + i, &ch);
            if (!iswprint(ch) || fprintf(f, "%lc", (wint_t)ch) < 0) fprintf(f, "{$%x}", ch);
#endif
            l = i;
            continue;
        }
        if (ch == 0) break;
        if ((ch < 0x20 && ch != 0x09) || ch > 0x7e) {
            if (l != i) fwrite(line + l, i - l, 1, f);
            i++;
            fprintf(f, "{$%x}", ch);
            l = i;
            continue;
        }
        i++;
    }
    if (i != l) fwrite(line + l, i - l, 1, f);
}

