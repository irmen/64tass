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
#include <string.h>
#include <wchar.h>
#include <errno.h>
#include <locale.h>
#include "file.h"
#include "values.h"
#include "misc.h"
#include "64tass.h"
#include "unicode.h"

struct include_list_s {
    struct include_list_s *next;
    char path[];
};

static struct include_list_s include_list;
struct include_list_s *include_list_last = &include_list;

static struct avltree file_tree;

void include_list_add(const char *path)
{
    size_t i, j, len;
    j = i = strlen(path);
    if (!i) return;
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
    if (path[i-1] != '/' && path[i-1] != '\\') j++;
#else
    if (path[i-1] != '/') j++;
#endif
    len = j + 1 + sizeof(struct include_list_s);
    include_list_last->next = (struct include_list_s *)malloc(len);
    if (!include_list_last->next || len < sizeof(struct include_list_s)) err_msg_out_of_memory();
    include_list_last = include_list_last->next;
    include_list_last->next = NULL;
    strcpy(include_list_last->path, path);
    if (i != j) strcat(include_list_last->path, "/");
}

const char *get_path(const value_t v, const char *base) {
    char *path;
    size_t i, len;
#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
    size_t j;

    i = strlen(base);
    j = (((base[0] >= 'A' && base[0] <= 'Z') || (base[0] >= 'a' && base[0] <= 'z')) && base[1]==':') ? 2 : 0;
    while (i > j) {
        if (base[i-1] == '/' || base[i-1] == '\\') break;
        i--;
    }
#else
    const char *c;
    c = strrchr(base, '/');
    i = c ? (c - base + 1) : 0;
#endif

    if (!v) {
        len = i + 1;
        path = (char *)malloc(len);
        if (!path || len < 1) err_msg_out_of_memory(); /* overflow */
        memcpy(path, base, i);
        path[i] = 0;
        return path;
    }

#if defined _WIN32 || defined __WIN32__ || defined __EMX__ || defined __DJGPP__
    if (v->u.str.len && (v->u.str.data[0]=='/' || v->u.str.data[0]=='\\')) i = j;
    else if (v->u.str.len > 1 && ((v->u.str.data[0] >= 'A' && v->u.str.data[0] <= 'Z') || (v->u.str.data[0] >= 'a' && v->u.str.data[0] <= 'z')) && v->u.str.data[1]==':') i = 0;
#else
    if (v->u.str.len && v->u.str.data[0]=='/') i = 0;
#endif
    len = i + v->u.str.len;
    if (len < i) err_msg_out_of_memory(); /* overflow */
    len += 1;
    path = (char *)malloc(len);
    if (!path || len < 1) err_msg_out_of_memory(); /* overflow */
    memcpy(path, base, i);
    memcpy(path + i, v->u.str.data, v->u.str.len);
    path[i + v->u.str.len] = 0;
    return path;
}

FILE *file_open(const char *name, const char *mode)
{
    FILE *f;
#ifdef _WIN32
    wchar_t *wname, *c2, wmode[3];
    const uint8_t *c;
    uint32_t ch;
    size_t len = strlen(name) + 1;
    wname = (wchar_t *)malloc(len * sizeof(wchar_t));
    if (!wname || len > SIZE_MAX / sizeof(wchar_t)) err_msg_out_of_memory();
    c2 = wname; c = (uint8_t *)name;
    while (*c) {
        ch = *c;
        if (ch & 0x80) c += utf8in(c, &ch); else c++;
        if (ch < 0x10000) *c2++ = ch;
        else if (ch < 0x110000) {
            *c2++ = (ch >> 10) + 0xd7c0;
            *c2++ = (ch & 0x3ff) | 0xdc00;
        } else *c2++ = 0xfffd;
    }
    *c2++ = 0;
    c2 = wmode; c = (uint8_t *)mode;
    while ((*c2++=(wchar_t)*c++));
    f=_wfopen(wname, wmode);
    free(wname);
#else
    size_t len = 0, max = strlen(name) + 1;
    char *newname = (char *)malloc(max);
    const uint8_t *c = (uint8_t *)name;
    uint32_t ch;
    mbstate_t ps;
    memset(&ps, 0, sizeof(mbstate_t));
    if (!newname || max < 1) err_msg_out_of_memory();
    do {
        char temp[64];
        int l;
        ch = *c;
        if (ch & 0x80) c += utf8in(c, &ch); else c++;
        l = wcrtomb(temp, (wchar_t)ch, &ps);
        if (l <= 0) l = sprintf(temp, "{$%x}", ch);
        len += l;
        if (len < (unsigned int)l) err_msg_out_of_memory();
        if (len > max) {
            max = len + 64;
            newname = (char *)realloc(newname, max);
            if (!newname || max < 64) err_msg_out_of_memory();
        }
        memcpy(newname + len - l, temp, l);
    } while (ch);
    f=fopen(newname, mode);
    free(newname);
#endif
    return f;
}

static int star_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct star_s *a = cavltree_container_of(aa, struct star_s, node);
    const struct star_s *b = cavltree_container_of(bb, struct star_s, node);

    return a->line - b->line;
}

static int file_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    const struct file_s *a = cavltree_container_of(aa, struct file_s, node);
    const struct file_s *b = cavltree_container_of(bb, struct file_s, node);
    int c;
    c = strcmp(a->name, b->name);
    if (c) return c;
    return strcmp(a->base, b->base);
}

static void star_free(struct avltree_node *aa)
{
    struct star_s *a = avltree_container_of(aa, struct star_s, node);

    avltree_destroy(&a->tree, star_free);
}

static void file_free(struct avltree_node *aa)
{
    struct file_s *a = avltree_container_of(aa, struct file_s, node);

    avltree_destroy(&a->star, star_free);
    free(a->data);
    free(a->line);
    free((char *)a->name);
    free((char *)a->realname);
    free((char *)a->base);
    free(a);
}

static inline void flushubuff(struct ubuff_s *ubuff, uint8_t **pp, struct file_s *tmp) {
    if (ubuff->p) {
        uint8_t *p = *pp;
        size_t i;
        for (i = 0; i < ubuff->p; i++) {
            size_t o = p - tmp->data;
            if (o + 6*6 + 1 > tmp->len) {
                tmp->len += 4096;
                tmp->data = (uint8_t *)realloc(tmp->data, tmp->len);
                if (!tmp->data || tmp->len < 4096) err_msg_out_of_memory(); /* overflow */
                p = tmp->data + o;
            }
            if (ubuff->data[i] && ubuff->data[i] < 0x80) *p++ = ubuff->data[i]; else p = utf8out(ubuff->data[i], p);
        }
        *pp = p;
    } else {
        if (ubuff->p >= ubuff->len) {
            ubuff->len += 16;
            ubuff->data = (uint32_t *)realloc(ubuff->data, ubuff->len * sizeof(uint32_t));
            if (!ubuff->data) err_msg_out_of_memory();
        }
    }
}

static uint32_t fromiso2(uint8_t c) {
    static mbstate_t ps;
    wchar_t w;
    int olderrno;
    ssize_t l;

    memset(&ps, 0, sizeof(ps));
    olderrno = errno;
    l = mbrtowc(&w, (char *)&c, 1,  &ps);
    errno = olderrno;
    if (l < 0) w = c;
    return w;
}

inline uint32_t fromiso(uint8_t c) {
    static uint32_t conv[128];
    if (!conv[c - 0x80]) conv[c - 0x80] = fromiso2(c);
    return conv[c - 0x80];
}

static struct file_s *command_line = NULL;
static struct file_s *lastfi = NULL;
static uint16_t curfnum=1;
struct file_s *openfile(const char* name, const char *base, int ftype, const value_t val, linepos_t epoint) {
    const char *base2;
    struct avltree_node *b;
    struct file_s *tmp;
    char *s;
    if (!lastfi) {
        lastfi = (struct file_s *)malloc(sizeof(struct file_s));
        if (!lastfi) err_msg_out_of_memory();
    }
    base2 = get_path(NULL, base);
    lastfi->base = base2;
    if (name) {
        lastfi->name=name;
        b=avltree_insert(&lastfi->node, &file_tree, file_compare);
    } else {
        b = command_line ? &command_line->node : NULL;
        if (!command_line) command_line = lastfi;
    }
    if (!b) { /* new file */
	enum filecoding_e type = E_UNKNOWN;
        FILE *f;
        uint32_t c = 0;
        size_t fp = 0;

	lastfi->line=NULL;
	lastfi->lines=0;
	lastfi->data=NULL;
	lastfi->len=0;
        lastfi->open=0;
        lastfi->type=ftype;
        avltree_init(&lastfi->star);
        tmp = lastfi;
        lastfi=NULL;
        if (name) {
            const char *path = NULL;
            s = (char *)malloc(strlen(name) + 1);
            if (!s) err_msg_out_of_memory();
            strcpy(s, name); tmp->name = s;
            if (val) {
                struct include_list_s *i = include_list.next;
                f = file_open(name, "rb");
                while (!f && i) {
                    free((char *)path);
                    path = get_path(val, i->path);
                    f = file_open(path, "rb");
                    i = i->next;
                }
            } else {
                if (name[0]=='-' && !name[1]) f=stdin;
                else f=file_open(name, "rb");
            }
            if (!path) {
                s = (char *)malloc(strlen(name) + 1);
                if (!s) err_msg_out_of_memory();
                strcpy(s, name);
                path = s;
            }
            tmp->realname = path;
            if (!f) {
                path = val ? get_path(val, "") : NULL;
                err_msg_file(ERROR_CANT_FINDFILE, val ? path : name, epoint);
                free((char *)path);
                return NULL;
            }
            if (ftype) {
                if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1])) {
                    printf("Reading file:      ");
                    argv_print(tmp->realname, stdout);
                    putchar('\n');
                }
                if (!fseek(f, 0, SEEK_END)) {
                    long len = ftell(f);
                    if (len >= 0) {
                        tmp->data = (uint8_t *)malloc(len);
                        if (!tmp->data) err_msg_out_of_memory();
                        tmp->len = len;
                    }
                    rewind(f);
                }
                do {
                    if (fp + 4096 > tmp->len) {
                        tmp->len += 4096;
                        tmp->data = (uint8_t *)realloc(tmp->data, tmp->len);
                        if (!tmp->data || tmp->len < 4096) err_msg_out_of_memory(); /* overflow */
                    }
                    fp += fread(tmp->data + fp, 1, tmp->len - fp, f);
                } while (!feof(f));
            } else {
                struct ubuff_s ubuff = {NULL, 0, 0};
                size_t max_lines = 0;
                uint8_t buffer[BUFSIZ * 2];
                size_t bp = 0, bl, qr = 1;
                if (arguments.quiet && !(arguments.output[0] == '-' && !arguments.output[1])) {
                    printf("Assembling file:   ");
                    argv_print(tmp->realname, stdout);
                    putchar('\n');
                }
                if (!fseek(f, 0, SEEK_END)) {
                    long len = ftell(f);
                    if (len >= 0) {
                        len += 4096;
                        tmp->data = (uint8_t *)malloc(len);
                        if (!tmp->data || len < 4096) err_msg_out_of_memory(); /* overflow */
                        tmp->len = len;
                        max_lines = (len / 20 + 1024) & ~1023;
                        tmp->line = (size_t *)malloc(max_lines * sizeof(tmp->line[0]));
                        if (!tmp->line) max_lines = 0;
                    }
                    rewind(f);
                }
                bl = fread(buffer, 1, BUFSIZ, f);
                if (bl && !buffer[0]) type = E_UTF16BE; /* most likely */
#ifdef _WIN32
                setlocale(LC_CTYPE, "");
#endif
                do {
                    int i, j;
                    uint8_t *p;
                    uint32_t lastchar;
                    int qc = 1;
                    uint8_t cclass = 0;

                    if (tmp->lines >= max_lines) {
                        max_lines += 1024;
                        tmp->line = (size_t *)realloc(tmp->line, max_lines * sizeof(tmp->line[0]));
                        if (!tmp->line || max_lines < 1024) err_msg_out_of_memory(); /* overflow */
                    }
                    tmp->line[tmp->lines++] = fp;
                    ubuff.p = 0;
                    p = tmp->data + fp;
                    for (;;) {
                        size_t o = p - tmp->data;
                        if (o + 6*6 + 1 > tmp->len) {
                            tmp->len += 4096;
                            tmp->data = (uint8_t *)realloc(tmp->data, tmp->len);
                            if (!tmp->data || tmp->len < 4096) err_msg_out_of_memory(); /* overflow */
                            p = tmp->data + o;
                        }
                        if (bp / (BUFSIZ / 2) == qr) {
                            if (qr == 1) {
                                qr = 3;
                                if (!feof(f)) bl = BUFSIZ + fread(buffer + BUFSIZ, 1, BUFSIZ, f);
                            } else {
                                qr = 1;
                                if (!feof(f)) bl = fread(buffer, 1, BUFSIZ, f);
                            }
                        }
                        if (bp == bl) break;
                        lastchar = c;
                        c = buffer[bp]; bp = (bp + 1) % (BUFSIZ * 2);
                        if (!arguments.toascii) {
                            if (c == 10) {
                                if (lastchar == 13) continue;
                                break;
                            } else if (c == 13) {
                                break;
                            }
                            if (c && c < 0x80) *p++ = c; else p = utf8out(c, p);
                            continue;
                        }
                        switch (type) {
                        case E_UNKNOWN:
                        case E_UTF8:
                            if (c < 0x80) goto done;
                            if (c < 0xc0) {
                                if (type == E_UNKNOWN) {
                                    c = fromiso(c);
                                    type = E_ISO; break;
                                }
                                c = 0xfffd; break;
                            } else if (c < 0xe0) {
                                c ^= 0xc0;i = 1;
                            } else if (c < 0xf0) {
                                c ^= 0xe0;i = 2;
                            } else if (c < 0xf8) {
                                c ^= 0xf0;i = 3;
                            } else if (c < 0xfc) {
                                c ^= 0xf8;i = 4;
                            } else if (c < 0xfe) {
                                c ^= 0xfc;i = 5;
                            } else {
                                if (type == E_UNKNOWN) {
                                    if (bp != bl) {
                                        uint8_t ch2 = buffer[bp];
                                        if (c == 0xff && ch2 == 0xfe) {
                                            bp = (bp + 1) % (BUFSIZ * 2);
                                            type = E_UTF16LE;continue;
                                        }
                                        if (c == 0xfe && ch2 == 0xff) {
                                            bp = (bp + 1) % (BUFSIZ * 2);
                                            type = E_UTF16BE;continue;
                                        }
                                    }
                                    c = fromiso(c);
                                    type = E_ISO; break;
                                }
                                c = 0xfffd; break;
                            }

                            for (j = i; i; i--) {
                                uint8_t ch2 = (bp == bl) ? 0 : buffer[bp];
                                if (ch2 < 0x80 || ch2 >= 0xc0) {
                                    if (type == E_UNKNOWN) {
                                        type = E_ISO;
                                        i = (j - i) * 6;
                                        qc = 0;
                                        if (ubuff.p >= ubuff.len) {
                                            ubuff.len += 16;
                                            ubuff.data = (uint32_t *)realloc(ubuff.data, ubuff.len * sizeof(uint32_t));
                                            if (!ubuff.data) err_msg_out_of_memory();
                                        }
                                        ubuff.data[ubuff.p++] = fromiso(((~0x7f >> j) & 0xff) | (c >> i));
                                        for (;i; i-= 6) {
                                            if (ubuff.p >= ubuff.len) {
                                                ubuff.len += 16;
                                                ubuff.data = (uint32_t *)realloc(ubuff.data, ubuff.len * sizeof(uint32_t));
                                                if (!ubuff.data) err_msg_out_of_memory();
                                            }
                                            ubuff.data[ubuff.p++] = fromiso(((c >> (i-6)) & 0x3f) | 0x80);
                                        }
                                        if (bp == bl) goto eof;
                                        c = (ch2 >= 0x80) ? fromiso(ch2) : ch2; 
                                        j = 0;
                                        bp = (bp + 1) % (BUFSIZ * 2);
                                        break;
                                    }
                                    if (bp == bl) goto eof;
                                    c = 0xfffd;break;
                                }
                                c = (c << 6) ^ ch2 ^ 0x80;
                                bp = (bp + 1) % (BUFSIZ * 2);
                            }
                            if (j) type = E_UTF8;
                            break;
                        case E_UTF16LE:
                            if (bp == bl) goto eof;
                            c |= buffer[bp] << 8; bp = (bp + 1) % (BUFSIZ * 2);
                            if (c == 0xfffe) {
                                type = E_UTF16BE;
                                continue;
                            }
                            break;
                        case E_UTF16BE:
                            if (bp == bl) goto eof;
                            c = (c << 8) | buffer[bp]; bp = (bp + 1) % (BUFSIZ * 2);
                            if (c == 0xfffe) {
                                type = E_UTF16LE;
                                continue;
                            }
                            break;
                        case E_ISO:
                            if (c >= 0x80) c = fromiso(c);
                            goto done;
                        }
                        if (c == 0xfeff) continue;
                        if (type != E_UTF8) {
                            if (c >= 0xd800 && c < 0xdc00) {
                                if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                                c = 0xfffd;
                            } else if (c >= 0xdc00 && c < 0xe000) {
                                if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                    c ^= 0x360dc00 ^ (lastchar << 10);
                                    c += 0x10000;
                                } else
                                    c = 0xfffd;
                            } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                c = 0xfffd;
                            }
                        }
                    done:
                        if (c < 0xc0) {
                            if (c == 10) {
                                if (lastchar == 13) continue;
                                break;
                            } else if (c == 13) {
                                break;
                            }
                            cclass = 0;
                            if (!qc) {
                                unfc(&ubuff);
                                qc = 1;
                            }
                            if (ubuff.p == 1) {
                                if (ubuff.data[0] && ubuff.data[0] < 0x80) *p++ = ubuff.data[0]; else p = utf8out(ubuff.data[0], p);
                            } else {
                                flushubuff(&ubuff, &p, tmp);
                                ubuff.p = 1;
                            }
                            ubuff.data[0] = c;
                        } else {
                            const struct properties_s *prop = uget_property(c);
                            uint8_t ncclass = prop->combclass;
                            if ((ncclass && cclass > ncclass) || prop->property & (qc_N | qc_M)) {
                                qc = 0;
                                if (ubuff.p >= ubuff.len) {
                                    ubuff.len += 16;
                                    ubuff.data = (uint32_t *)realloc(ubuff.data, ubuff.len * sizeof(uint32_t));
                                    if (!ubuff.data) err_msg_out_of_memory();
                                }
                                ubuff.data[ubuff.p++] = c;
                            } else {
                                if (!qc) {
                                    unfc(&ubuff);
                                    qc = 1; 
                                }
                                if (ubuff.p == 1) {
                                    if (ubuff.data[0] && ubuff.data[0] < 0x80) *p++ = ubuff.data[0]; else p = utf8out(ubuff.data[0], p);
                                } else {
                                    flushubuff(&ubuff, &p, tmp);
                                    ubuff.p = 1;
                                }
                                ubuff.data[0] = c;
                            }
                            cclass = ncclass;
                        }
                    }
                eof:
                    if (!qc) unfc(&ubuff);
                    if (ubuff.p) flushubuff(&ubuff, &p, tmp);
                    i = (p - tmp->data) - fp;
                    p = tmp->data + fp;
                    while (i && p[i-1]==' ') i--;
                    p[i++] = 0;
                    fp += i;
                } while (bp != bl);
#ifdef _WIN32
                setlocale(LC_CTYPE, "C");
#endif
                free(ubuff.data);
                if (tmp->lines != max_lines) {
                    tmp->line = (size_t *)realloc(tmp->line, tmp->lines * sizeof(tmp->line[0]));
                    if (!tmp->lines) err_msg_out_of_memory();
                }
            }
            if (ferror(f) && errno) err_msg_file(ERROR__READING_FILE, name, epoint);
            if (f != stdin) fclose(f);
            tmp->len = fp;
            tmp->data = (uint8_t *)realloc(tmp->data, tmp->len);
            if (!tmp->data) err_msg_out_of_memory();
            tmp->coding = type;
        } else {
            const char *cmd_name = "<command line>";
            s = (char *)malloc(1);
            if (!s) err_msg_out_of_memory();
            s[0] = 0; tmp->name = s;
            s = (char *)malloc(strlen(cmd_name) + 1);
            if (!s) err_msg_out_of_memory();
            strcpy(s, cmd_name); tmp->realname = s;
            tmp->coding = E_UNKNOWN;
        }

        tmp->uid=curfnum++;
    } else {
        free((char *)base2);
        tmp = avltree_container_of(b, struct file_s, node);
        if (tmp->type != ftype) err_msg_file(ERROR__READING_FILE, name, epoint);
    }
    tmp->open++;
    return tmp;
}

void closefile(struct file_s *f) {
    if (f->open) f->open--;
}

static struct stars_s {
    struct star_s stars[255];
    struct stars_s *next;
} *stars = NULL;

static struct star_s *lastst;
static int starsp;
struct star_s *new_star(line_t line, int *exists) {
    struct avltree_node *b;
    struct star_s *tmp;
    lastst->line=line;
    b=avltree_insert(&lastst->node, star_tree, star_compare);
    if (!b) { /* new label */
	*exists=0;
        avltree_init(&lastst->tree);
        if (starsp == 254) {
            struct stars_s *old = stars;
            stars = (struct stars_s *)malloc(sizeof(struct stars_s));
            if (!stars) err_msg_out_of_memory();
            stars->next = old;
            starsp = 0;
        } else starsp++;
        tmp = lastst;
        lastst = &stars->stars[starsp];
	return tmp;
    }
    *exists=1;
    return avltree_container_of(b, struct star_s, node);            /* already exists */
}

void destroy_file(void) {
    struct stars_s *old;

    avltree_destroy(&file_tree, file_free);
    free(lastfi);
    if (command_line) file_free(&command_line->node);

    include_list_last = include_list.next;
    while (include_list_last) {
        struct include_list_s *tmp = include_list_last;
        include_list_last = tmp->next;
        free(tmp);
    }

    while (stars) {
        old = stars;
        stars = stars->next;
        free(old);
    }
}

void init_file(void) {
    avltree_init(&file_tree);
    stars = (struct stars_s *)malloc(sizeof(struct stars_s));
    if (!stars) err_msg_out_of_memory();
    stars->next = NULL;
    starsp = 0;
    lastst = &stars->stars[starsp];
}

