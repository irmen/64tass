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
#include "unicode.h"
#include "wchar.h"
#include "wctype.h"
#include <ctype.h>
#include <string.h>
#include "error.h"

#define U_CASEFOLD 1
#define U_COMPAT 2

unsigned int utf8in(const uint8_t *c, uint32_t *out) { /* only for internal use with validated utf-8! */
    uint32_t ch;
    unsigned int i, j;
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
    unsigned int i, j;

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
        *c++=0xc0 | (uint8_t)(i >> 6);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x10000) {
        *c++=0xe0 | (uint8_t)(i >> 12);
        *c++=0x80 | ((i >> 6) & 0x3f);
        *c++=0x80 | (i & 0x3f);
	return c;
    }
    if (i < 0x200000) {
        *c++=0xf0 | (uint8_t)(i >> 18);
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
    if ((i & ~(uint32_t)0x7fffffff) != 0) return c;
    *c++=0xfc | (i >> 30);
    *c++=0x80 | ((i >> 24) & 0x3f);
    *c++=0x80 | ((i >> 18) & 0x3f);
    *c++=0x80 | ((i >> 12) & 0x3f);
    *c++=0x80 | ((i >> 6) & 0x3f);
    *c++=0x80 | (i & 0x3f);
    return c;
}

static inline unsigned int utf8outlen(uint32_t i) {
    if (i < 0x800) return 2;
    if (i < 0x10000) return 3;
    if (i < 0x200000) return 4;
    if (i < 0x4000000) return 5;
    return 6;
}

static void extbuff(struct ubuff_s *d) {
    d->len += 16;
    if (/*d->len < 16 ||*/ d->len > SIZE_MAX / sizeof *d->data) err_msg_out_of_memory(); /* overflow */
    d->data = (uint32_t *)reallocx(d->data, d->len * sizeof *d->data);
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
        if (ht != 0) {
            if (d->p >= d->len) extbuff(d);
            d->data[d->p++] = 0x11a7 + ht;
        }
        return;
    }
    prop = uget_property(ch);
    if ((options & U_CASEFOLD) != 0 && prop->casefold != 0) {
        if (prop->casefold > 0) {
            if (d->p >= d->len) extbuff(d);
            d->data[d->p++] = prop->casefold;
            return;
        }
        if (prop->casefold > -16384) {
            const int16_t *p = &usequences[-prop->casefold];
            for (;;) {
                if (d->p >= d->len) extbuff(d);
                d->data[d->p++] = (uint16_t)abs(*p);
                if (*p < 0) return;
                p++;
            }
        } else {
            const int32_t *p = &usequences2[-prop->casefold - 16384];
            for (;;) {
                if (d->p >= d->len) extbuff(d);
                d->data[d->p++] = (uint32_t)abs(*p);
                if (*p < 0) return;
                p++;
            }
        }
    }
    if (prop->decompose != 0) {
        if ((prop->property & pr_compat) == 0 || (options & U_COMPAT) != 0) {
            if (prop->decompose > 0) {
                udecompose(prop->decompose, d, options);
                return;
            } 
            if (prop->decompose > -16384) {
                const int16_t *p = &usequences[-prop->decompose];
                for (;;) {
                    udecompose((uint16_t)abs(*p), d, options);
                    if (*p < 0) return;
                    p++;
                }
            } else {
                const int32_t *p = &usequences2[-prop->decompose - 16384];
                for (;;) {
                    udecompose((uint32_t)abs(*p), d, options);
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
        if (cc2 != 0) {
            ch1 = d->data[pos];
            cc1 = uget_property(ch1)->combclass;
            if (cc1 > cc2) {
                d->data[pos] = ch2;
                d->data[pos + 1] = ch1;
                if (pos != 0) {
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
                    if (ch >= 0x11a7 && ch < 0x11a7 + 28) {
                        d->data[sp] = sc + ch - 0x11a7;
                        sprop = NULL;
                        continue;
                    }
                }
            } else if (sc >= 0x1100 && sc < 0x1100 + 19 && ch >= 0x1161 && ch < 0x1161 + 21) {
                d->data[sp] = 0xac00 + (ch - 0x1161 + (sc - 0x1100) * 21) * 28;
                sprop = NULL;
                continue;
            }
            if (sprop == NULL) sprop = uget_property(sc);
            if (sprop->base >= 0 && prop->diar >= 0) {
                int16_t comp = ucomposing[sprop->base + prop->diar];
                if (comp != 0) {
                    d->data[sp] = (comp > 0) ? (uint16_t)comp : ucomposed[-comp];
                    sprop = NULL;
                    continue;
                }
            }
        }
        if (prop->combclass != 0) {
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
    if (b == NULL) {
        free(dbuf.data);
        return;
    }
    for (dbuf.p = i = 0; i < b->p; i++) {
        udecompose(b->data[i], &dbuf, 0);
    }
    unormalize(&dbuf);
    ucompose(&dbuf, b);
}

void unfkc(str_t *s1, const str_t *s2, int mode) {
    const uint8_t *d, *m;
    uint8_t *s, *dd;
    size_t i, l;
    static struct ubuff_s dbuf, dbuf2;
    if (s2 == NULL) {
        free(dbuf.data);
        free(dbuf2.data);
        return;
    }
    mode = ((mode != 0) ? U_CASEFOLD : 0) | U_COMPAT;
    d = s2->data;
    for (dbuf.p = i = 0; i < s2->len;) {
        uint32_t ch;
        ch = d[i];
        if ((ch & 0x80) != 0) {
            i += utf8in(d + i, &ch);
            udecompose(ch, &dbuf, mode);
            continue;
        }
        if ((mode & U_CASEFOLD) != 0 && ch >= 'A' && ch <= 'Z') ch |= 0x20;
        if (dbuf.p >= dbuf.len) extbuff(&dbuf);
        dbuf.data[dbuf.p++] = ch;
        i++;
    }
    unormalize(&dbuf);
    ucompose(&dbuf, &dbuf2);
    l = s2->len;
    if (l > s1->len) {
        s1->data = (uint8_t *)reallocx((uint8_t *)s1->data, l);
    }
    dd = s = (uint8_t *)s1->data;
    m = dd + l;
    for (i = 0; i < dbuf2.p; i++) {
        uint32_t ch;
        ch = dbuf2.data[i];
        if (ch != 0 && ch < 0x80) {
            if (s >= m) {
                size_t o = (size_t)(s - dd);
                l += 16;
                if (l < 16) err_msg_out_of_memory();
                dd = (uint8_t *)reallocx(dd, l);
                s = dd + o;
                m = dd + l;
            }
            *s++ = (uint8_t)ch;
            continue;
        }
        if (s + utf8outlen(ch) > m) {
            size_t o = (size_t)(s - dd);
            l += 16;
            if (l < 16) err_msg_out_of_memory();
            dd = (uint8_t *)reallocx(dd, l);
            s = dd + o;
            m = dd + l;
        }
        s = utf8out(ch, s);
    }
    s1->len = (size_t)(s - dd);
    s1->data = dd;
}

size_t argv_print(const char *line, FILE *f) {
    size_t len = 0;
#ifdef _WIN32
    size_t i = 0, back;
    bool quote = false, space = false;

    for (;;i++) {
        switch (line[i]) {
        case '%':
        case '"': quote = true; if (!space) continue; break;
        case ' ': space = true; if (!quote) continue; break;
        case 0: break;
        default: continue;
        }
        break;
    }

    if (space) {
        if (quote) {len++;putc('^', f);}
        len++;putc('"', f);
    }
    i = 0; back = 0;
    for (;;) {
        uint32_t ch = (uint8_t)line[i];
        if ((ch & 0x80) != 0) {
            unsigned int ln = utf8in((const uint8_t *)line + i, &ch);
            if (iswprint(ch) != 0) {
                int ln2;
                char tmp[64];
                memcpy(tmp, line + i, ln);
                tmp[ln] = 0;
                ln2 = fwprintf(f, L"%S", tmp);
                if (ln2 > 0) {
                    i += ln;
                    len += ln2;
                    continue;
                }
            }
            i += ln;
            len++;putc('?', f);
            continue;
        }
        if (ch == 0) break;

        if (ch == '\\') {
            back++;
            i++;
            len++;putc('\\', f);
            continue;
        }
        if (!space || quote) {
            if (strchr("()%!^<>&|\"", ch) != NULL) {
                if (ch == '"') {
                    while ((back--) != 0) {len++;putc('\\', f);}
                    len++;putc('\\', f);
                }
                len++;putc('^', f);
            }
        } else {
            if (ch == '%') {
                len++;putc('^', f);
            }
        }
        back = 0;

        i++;
        if (isprint(ch) == 0) {
            len++;putc('?', f);
            continue;
        }
        len++;putc(ch, f);
    }
    if (space) {
        while ((back--) != 0) {len++;putc('\\', f);}
        if (quote) {len++;putc('^', f);}
        len++;putc('"', f);
    }
#else
    size_t i;
    bool quote = false;

    for (i = 0;line[i] != 0;i++) {
        if (line[i] == '!') break;
        if (strchr(" \"$&()*;<>'?[\\]`{|}", line[i]) != NULL) quote = true;
    }
    if (line[i] != 0) quote = false;
    if (quote) {len++;putc('"', f);}
    else {
        switch (line[0]) {
        case '~':
        case '#': len++;putc('\\', f); break;
        }
    }
    i = 0;
    for (;;) {
        uint32_t ch = (uint8_t)line[i];
        if ((ch & 0x80) != 0) {
            int ln2;
            i += utf8in((const uint8_t *)line + i, &ch);
            if (iswprint(ch) != 0) {
                mbstate_t ps;
                char temp[64];
                size_t ln;
                memset(&ps, 0, sizeof ps);
                ln = wcrtomb(temp, ch, &ps);
                if (ln != (size_t)-1) {
                    len += fwrite(temp, ln, 1, f);
                    continue;
                }
            }
            ln2 = fprintf(f, ch < 0x10000 ? "$'\\u%" PRIx32 "'" : "$'\\U%" PRIx32 "'", ch);
            if (ln2 > 0) len += (size_t)ln2;
            continue;
        }
        if (ch == 0) break;

        if (quote) {
            if (strchr("$`\"\\", ch) != NULL) {len++;putc('\\', f);}
        } else {
            if (strchr(" !\"$&()*;<>'?[\\]`{|}", ch) != NULL) {
                len++;putc('\\', f);
            }
        }

        i++;
        if (isprint(ch) == 0) {
            int ln = fprintf(f, "$'\\x%" PRIx32 "'", ch);
            if (ln > 0) len += (size_t)ln;
            continue;
        }
        len++;putc(ch, f);
    }
    if (quote) {len++;putc('"', f);}
#endif
    return len;
}

static int unknown_print(FILE *f, uint32_t ch) {
    char temp[64];
    const char *format = (ch >= 256) ? "<U+%" PRIX32 ">" : "<%02" PRIX32 ">";
    if (f != NULL) {
        int ln;
        if (print_use_color) fputs("\33[7m", f);
        ln = fprintf(f, format, ch);
        if (print_use_color) fputs("\33[m", f);
        return ln;
    }
    return sprintf(temp, format, ch);
}

void printable_print(const uint8_t *line, FILE *f) {
#ifdef _WIN32
    size_t i = 0, l = 0;
    for (;;) {
        uint32_t ch = line[i];
        if ((ch & 0x80) != 0) {
            unsigned int ln;
            if (l != i) fwrite(line + l, 1, i - l, f);
            ln = utf8in(line + i, &ch);
            if (iswprint(ch) != 0) {
                char tmp[64];
                memcpy(tmp, line + i, ln);
                tmp[ln] = 0;
                if (fwprintf(f, L"%S", tmp) > 0) {
                    i += ln;
                    l = i;
                    continue;
                }
            }
            i += ln;
            l = i;
            unknown_print(f, ch);
            continue;
        }
        if (ch == 0) break;
        if ((ch < 0x20 && ch != 0x09) || ch > 0x7e) {
            if (l != i) fwrite(line + l, 1, i - l, f);
            i++;
            unknown_print(f, ch);
            l = i;
            continue;
        }
        i++;
    }
    if (i != l) fwrite(line + l, i - l, 1, f);
#else
    size_t i = 0, l = 0;
    for (;;) {
        uint32_t ch = line[i];
        if ((ch & 0x80) != 0) {
            if (l != i) fwrite(line + l, 1, i - l, f);
            i += utf8in(line + i, &ch);
            l = i;
            if (iswprint(ch) != 0) {
                mbstate_t ps;
                char temp[64];
                size_t ln;
                memset(&ps, 0, sizeof ps);
                ln = wcrtomb(temp, ch, &ps);
                if (ln != (size_t)-1) {
                    fwrite(temp, ln, 1, f);
                    continue;
                }
            }
            unknown_print(f, ch);
            continue;
        }
        if (ch == 0) break;
        if ((ch < 0x20 && ch != 0x09) || ch > 0x7e) {
            if (l != i) fwrite(line + l, 1, i - l, f);
            i++;
            unknown_print(f, ch);
            l = i;
            continue;
        }
        i++;
    }
    if (i != l) fwrite(line + l, i - l, 1, f);
#endif
}

size_t printable_print2(const uint8_t *line, FILE *f, size_t max) {
#ifdef _WIN32
    size_t i, l = 0, len = 0;
    int err;
    for (i = 0; i < max;) {
        uint32_t ch = line[i];
        if ((ch & 0x80) != 0) {
            unsigned int ln;
            if (l != i) len += fwrite(line + l, 1, i - l, f);
            ln = utf8in(line + i, &ch);
            if (iswprint(ch) != 0) {
                char tmp[64];
                memcpy(tmp, line + i, ln);
                tmp[ln] = 0;
                if (fwprintf(f, L"%S", tmp) >= 0) {
                    i += ln;
                    l = i;
                    len++;
                    continue;
                }
            }
            i += ln;
            l = i;
            err = unknown_print(f, ch);
            if (err > 0) len += (size_t)err;
            continue;
        }
        if ((ch < 0x20 && ch != 0x09) || ch > 0x7e) {
            if (l != i) len += fwrite(line + l, 1, i - l, f);
            i++;
            l = i;
            err = unknown_print(f, ch);
            if (err > 0) len += (size_t)err;
            continue;
        }
        i++;
    }
    if (i != l) len += fwrite(line + l, 1, i - l, f);
    return len;
#else
    size_t i, l = 0, len = 0;
    int err;
    for (i = 0; i < max;) {
        uint32_t ch = line[i];
        if ((ch & 0x80) != 0) {
            if (l != i) len += fwrite(line + l, 1, i - l, f);
            i += utf8in(line + i, &ch);
            l = i;
            if (iswprint(ch) != 0) {
                mbstate_t ps;
                char temp[64];
                size_t ln;
                memset(&ps, 0, sizeof ps);
                ln = wcrtomb(temp, ch, &ps);
                if (ln != (size_t)-1) {
                    len += fwrite(temp, ln, 1, f); /* 1 character */
                    continue;
                }
            }
            err = unknown_print(f, ch);
            if (err > 0) len += (size_t)err;
            continue;
        }
        if ((ch < 0x20 && ch != 0x09) || ch > 0x7e) {
            if (l != i) len += fwrite(line + l, 1, i - l, f);
            i++;
            l = i;
            err = unknown_print(f, ch);
            if (err > 0) len += (size_t)err;
            continue;
        }
        i++;
    }
    if (i != l) len += fwrite(line + l, 1, i - l, f);
    return len;
#endif
}

struct zw16_s {
    uint16_t start;
    uint16_t end;
};

static const struct zw16_s zw16[] = {
    {0x0300, 0x036F}, {0x0483, 0x0486}, {0x0488, 0x0489},
    {0x0591, 0x05BD}, {0x05BF, 0x05BF}, {0x05C1, 0x05C2},
    {0x05C4, 0x05C5}, {0x05C7, 0x05C7}, {0x0600, 0x0603},
    {0x0610, 0x0615}, {0x064B, 0x065E}, {0x0670, 0x0670},
    {0x06D6, 0x06E4}, {0x06E7, 0x06E8}, {0x06EA, 0x06ED},
    {0x070F, 0x070F}, {0x0711, 0x0711}, {0x0730, 0x074A},
    {0x07A6, 0x07B0}, {0x07EB, 0x07F3}, {0x0901, 0x0902},
    {0x093C, 0x093C}, {0x0941, 0x0948}, {0x094D, 0x094D},
    {0x0951, 0x0954}, {0x0962, 0x0963}, {0x0981, 0x0981},
    {0x09BC, 0x09BC}, {0x09C1, 0x09C4}, {0x09CD, 0x09CD},
    {0x09E2, 0x09E3}, {0x0A01, 0x0A02}, {0x0A3C, 0x0A3C},
    {0x0A41, 0x0A42}, {0x0A47, 0x0A48}, {0x0A4B, 0x0A4D},
    {0x0A70, 0x0A71}, {0x0A81, 0x0A82}, {0x0ABC, 0x0ABC},
    {0x0AC1, 0x0AC5}, {0x0AC7, 0x0AC8}, {0x0ACD, 0x0ACD},
    {0x0AE2, 0x0AE3}, {0x0B01, 0x0B01}, {0x0B3C, 0x0B3C},
    {0x0B3F, 0x0B3F}, {0x0B41, 0x0B43}, {0x0B4D, 0x0B4D},
    {0x0B56, 0x0B56}, {0x0B82, 0x0B82}, {0x0BC0, 0x0BC0},
    {0x0BCD, 0x0BCD}, {0x0C3E, 0x0C40}, {0x0C46, 0x0C48},
    {0x0C4A, 0x0C4D}, {0x0C55, 0x0C56}, {0x0CBC, 0x0CBC},
    {0x0CBF, 0x0CBF}, {0x0CC6, 0x0CC6}, {0x0CCC, 0x0CCD},
    {0x0CE2, 0x0CE3}, {0x0D41, 0x0D43}, {0x0D4D, 0x0D4D},
    {0x0DCA, 0x0DCA}, {0x0DD2, 0x0DD4}, {0x0DD6, 0x0DD6},
    {0x0E31, 0x0E31}, {0x0E34, 0x0E3A}, {0x0E47, 0x0E4E},
    {0x0EB1, 0x0EB1}, {0x0EB4, 0x0EB9}, {0x0EBB, 0x0EBC},
    {0x0EC8, 0x0ECD}, {0x0F18, 0x0F19}, {0x0F35, 0x0F35},
    {0x0F37, 0x0F37}, {0x0F39, 0x0F39}, {0x0F71, 0x0F7E},
    {0x0F80, 0x0F84}, {0x0F86, 0x0F87}, {0x0F90, 0x0F97},
    {0x0F99, 0x0FBC}, {0x0FC6, 0x0FC6}, {0x102D, 0x1030},
    {0x1032, 0x1032}, {0x1036, 0x1037}, {0x1039, 0x1039},
    {0x1058, 0x1059}, {0x1160, 0x11FF}, {0x135F, 0x135F},
    {0x1712, 0x1714}, {0x1732, 0x1734}, {0x1752, 0x1753},
    {0x1772, 0x1773}, {0x17B4, 0x17B5}, {0x17B7, 0x17BD},
    {0x17C6, 0x17C6}, {0x17C9, 0x17D3}, {0x17DD, 0x17DD},
    {0x180B, 0x180E}, {0x18A9, 0x18A9}, {0x1920, 0x1922},
    {0x1927, 0x1928}, {0x1932, 0x1932}, {0x1939, 0x193B},
    {0x1A17, 0x1A18}, {0x1B00, 0x1B03}, {0x1B34, 0x1B34},
    {0x1B36, 0x1B3A}, {0x1B3C, 0x1B3C}, {0x1B42, 0x1B42},
    {0x1B6B, 0x1B73}, {0x1DC0, 0x1DCA}, {0x1DFE, 0x1DFF},
    {0x200B, 0x200F}, {0x202A, 0x202E}, {0x2060, 0x2063},
    {0x206A, 0x206F}, {0x20D0, 0x20EF}, {0x302A, 0x302F},
    {0x3099, 0x309A}, {0xA806, 0xA806}, {0xA80B, 0xA80B},
    {0xA825, 0xA826}, {0xFB1E, 0xFB1E}, {0xFE00, 0xFE0F},
    {0xFE20, 0xFE23}, {0xFEFF, 0xFEFF}, {0xFFF9, 0xFFFB}
};

struct zw32_s {
    uint32_t start;
    uint32_t end;
};

static const struct zw32_s zw32[] = {
    {0x10A01, 0x10A03}, {0x10A05, 0x10A06}, {0x10A0C, 0x10A0F},
    {0x10A38, 0x10A3A}, {0x10A3F, 0x10A3F}, {0x1D167, 0x1D169},
    {0x1D173, 0x1D182}, {0x1D185, 0x1D18B}, {0x1D1AA, 0x1D1AD},
    {0x1D242, 0x1D244}, {0xE0001, 0xE0001}, {0xE0020, 0xE007F},
    {0xE0100, 0xE01EF}
};

static int compw16(const void *aa, const void *bb) {
    uint32_t key = *(const uint32_t *)aa;
    const struct zw16_s *b = (const struct zw16_s *)bb;

    if (key > b->end) return 1;
    if (key < b->start) return -1;
    return 0;
}

static int compw32(const void *aa, const void *bb) {
    uint32_t key = *(const uint32_t *)aa;
    const struct zw32_s *b = (const struct zw32_s *)bb;

    if (key > b->end) return 1;
    if (key < b->start) return -1;
    return 0;
}

static size_t charwidth(uint32_t ch) {
    if (ch < 0x300) return 1;

    if (ch < 0x10000) {
        if (bsearch(&ch, zw16, lenof(zw16), sizeof *zw16, compw16) != NULL) return 0;
        if (ch < 0x1100) return 1;
        if (ch <= 0x115f || ch == 0x2329 || ch == 0x232a) return 2;
        if (ch >= 0x2e80 && ch <= 0xa4cf && ch != 0x303f) return 2;
        if (ch >= 0xac00 && ch <= 0xd7a3) return 2;
        if (ch >= 0xf900 && ch <= 0xfaff) return 2;
        if (ch >= 0xfe10 && ch <= 0xfe19) return 2;
        if (ch >= 0xfe30 && ch <= 0xfe6f) return 2;
        if (ch >= 0xff00 && ch <= 0xff60) return 2;
        if (ch >= 0xffe0 && ch <= 0xffe6) return 2;
        return 1;
    }

    if (bsearch(&ch, zw32, lenof(zw32), sizeof *zw32, compw32) != NULL) return 0;
    if (ch >= 0x20000 && ch <= 0x2fffd) return 2;
    if (ch >= 0x30000 && ch <= 0x3fffd) return 2;
    return 1;
}

void caret_print(const uint8_t *line, FILE *f, size_t max) {
    size_t i, l = 0;
    for (i = 0; i < max;) {
        uint32_t ch = line[i];
        if ((ch & 0x80) != 0) {
#ifdef _WIN32
            unsigned int ln = utf8in(line + i, &ch);
            if (iswprint(ch) != 0) {
                char tmp[64];
                wchar_t tmp2[64];
                memcpy(tmp, line + i, ln);
                tmp[ln] = 0;
                if (swprintf(tmp2, lenof(tmp2), L"%S", tmp) > 0) {
                    i += ln;
                    l += charwidth(ch);
                    continue;
                }
            }
            i += ln;
#else
            i += utf8in(line + i, &ch);
            if (iswprint(ch) != 0) {
                char temp[64];
                mbstate_t ps;
                memset(&ps, 0, sizeof ps);
                if (wcrtomb(temp, ch, &ps) != (size_t)-1) {
                    l += charwidth(ch);
                    continue;
                }
            }
#endif
            l += unknown_print(NULL, ch);
            continue;
        }
        if (ch == 0) break;
        if (ch == '\t') {
            while (l != 0) { 
                putc(' ', f); 
                l--; 
            }
            putc('\t', f);
            i++;
            continue;
        }
        if (ch < 0x20 || ch > 0x7e) l += unknown_print(NULL, ch); else l++;
        i++;
    }
    while (l != 0) { 
        putc(' ', f); 
        l--; 
    }
}

size_t calcpos(const uint8_t *line, size_t pos) {
    size_t s, l;
    s = l = 0;
    while (s < pos) {
        if (line[s] == 0) return l;
        s += utf8len(line[s]);
        l++;
    }
    return l;
}
