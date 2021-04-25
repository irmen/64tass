/*
    $Id: file.c 2622 2021-04-25 15:16:03Z soci $

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
#include "file.h"
#include <string.h>
#include "wchar.h"
#include <errno.h>
#ifdef _WIN32
#include <locale.h>
#include <windows.h>
#endif
#if defined _POSIX_C_SOURCE || defined __unix__ || defined __MINGW32__
#include <sys/stat.h>
#endif
#include "64tass.h"
#include "unicode.h"
#include "error.h"
#include "arguments.h"
#include "unicodedata.h"
#include "avl.h"

#define REPLACEMENT_CHARACTER 0xfffd

struct include_list_s {
    struct include_list_s *next;
#if __STDC_VERSION__ >= 199901L
    char path[];
#elif __GNUC__ >= 3
    char path[];
#else
    char path[1];
#endif
};

static struct include_list_s include_list;
static struct include_list_s *include_list_last = &include_list;

static struct {
    size_t len, mask;
    struct file_s **data;
} file_table;

static struct file_s *file_table_update(struct file_s *p) {
    size_t mask, hash, offs;
    if (file_table.len * 3 / 2 >= file_table.mask) {
        size_t i, max = (file_table.data == NULL) ? 8 : (file_table.mask + 1) << 1;
        struct file_s **n = (struct file_s **)calloc(max, sizeof *n);
        if (n == NULL) err_msg_out_of_memory();
        mask = max - 1;
        if (file_table.data != NULL) {
            for (i = 0; i <= file_table.mask; i++) if (file_table.data[i] != NULL) {
                hash = (size_t)file_table.data[i]->hash;
                offs = hash & mask;
                while (n[offs] != NULL) {
                    hash >>= 5;
                    offs = (5 * offs + hash + 1) & mask;
                }
                n[offs] = file_table.data[i];
            }
            free(file_table.data);
        }
        file_table.mask = mask;
        file_table.data = n;
    }
    mask = file_table.mask;
    hash = (size_t)p->hash;
    offs = hash & mask;
    while (file_table.data[offs] != NULL) {
        struct file_s *d = file_table.data[offs];
        if (p->hash == d->hash && p->type == d->type && strcmp(p->name, d->name) == 0) {
            if (p->base.data == d->base.data) return d;
            if (p->base.len == d->base.len && memcmp(p->base.data, d->base.data, p->base.len) == 0) {
                return d;
            }
        }
        hash >>= 5;
        offs = (5 * offs + hash + 1) & mask;
    } 
    file_table.data[offs] = p;
    file_table.len++;
    return NULL;
}

void include_list_add(const char *path)
{
    size_t i, j, len;
    j = i = strlen(path);
    if (i == 0) return;
#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    if (path[i - 1] != '/' && path[i-1] != '\\') j++;
#else
    if (path[i - 1] != '/') j++;
#endif
    len = j + 1 + sizeof(struct include_list_s);
    if (len <= sizeof(struct include_list_s)) err_msg_out_of_memory();
    include_list_last->next = (struct include_list_s *)mallocx(len);
    include_list_last = include_list_last->next;
    include_list_last->next = NULL;
    memcpy(include_list_last->path, path, i + 1);
    if (i != j) memcpy(include_list_last->path + i, "/", 2);
}

#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
static inline bool is_driveletter(const char *name) {
    return (uint8_t)((name[0] | 0x20) - 'a') < 26 && name[1] == ':';
}
#endif

size_t get_base(const char *base) {
#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    size_t i, j = is_driveletter(base) ? 2 : 0;
    for (i = j; base[i] != '\0'; i++) {
        if (base[i] == '/' || base[i] == '\\') j = i + 1;
    }
    return j;
#else
    const char *c = strrchr(base, '/');
    return (c != NULL) ? (size_t)(c - base) + 1 : 0;
#endif
}

char *get_path(const str_t *v, const char *base) {
    char *path;
    size_t i, len;

#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    if (v->len != 0 && (v->data[0] == '/' || v->data[0] == '\\')) i = is_driveletter(base) ? 2 : 0;
    else i = (v->len > 1 && is_driveletter((const char *)v->data)) ? 0 : get_base(base);
#else
    i = (v->len != 0 && v->data[0] == '/') ? 0 : get_base(base);
#endif
    len = i + 1 + v->len;
    if (len <= i) err_msg_out_of_memory(); /* overflow */
    path = (char *)mallocx(len);
    memcpy(path, base, i);
    memcpy(path + i, v->data, v->len);
    path[i + v->len] = 0;
    return path;
}

#ifdef _WIN32
static MUST_CHECK wchar_t *convert_name(const char *name, size_t max) {
    wchar_t *wname;
    uchar_t ch;
    size_t i = 0, j = 0, len = ((max != SIZE_MAX) ? max : strlen(name)) + 2;
    if (len > SIZE_MAX / sizeof *wname) return NULL;
    wname = (wchar_t *)malloc(len * sizeof *wname);
    if (wname == NULL) return NULL;
    while (name[i] != 0 && i < max) {
        ch = (uint8_t)name[i];
        if ((ch & 0x80) != 0) {
            i += utf8in((const uint8_t *)name + i, &ch);
            if (ch == 0) ch = REPLACEMENT_CHARACTER;
        } else i++;
        if (j + 3 > len) {
            wchar_t *d;
            len += 64;
            if (len > SIZE_MAX / sizeof *wname) goto failed;
            d = (wchar_t *)realloc(wname, len * sizeof *wname);
            if (d == NULL) goto failed;
            wname = d;
        }
        if (ch < 0x10000) {
        } else if (ch < 0x110000) {
            wname[j++] = (wchar_t)((ch >> 10) + 0xd7c0);
            ch = (ch & 0x3ff) | 0xdc00;
        } else ch = REPLACEMENT_CHARACTER;
        wname[j++] = (wchar_t)ch;
    }
    wname[j] = 0;
    return wname;
failed:
    free(wname);
    return NULL;
}
#endif

static bool portability(const str_t *name, linepos_t epoint) {
    struct linepos_s epoint2;
    const uint8_t *pos;
#if defined _WIN32 || defined __WIN32__ || defined __MSDOS__ || defined __DOS__
    if (name->len == 0) return true;
    pos = (const uint8_t *)memchr(name->data, '\\', name->len);
    if (pos != NULL) {
        epoint2.line = epoint->line;
        epoint2.pos = interstring_position(epoint, name->data, (size_t)(pos - name->data));
        err_msg2(ERROR_____BACKSLASH, name, &epoint2);
        return false;
    }
    if (name->data[0] == '/' || is_driveletter((const char *)name->data)) {
        err_msg2(ERROR_ABSOLUTE_PATH, name, epoint);
        return false;
    }
#else
    const char *c;
    if (name->len == 0) return true;
    for (c = "\\:*?\"<>|"; *c != '\0'; c++) {
        pos = (const uint8_t *)memchr(name->data, *c, name->len);
        if (pos == NULL) continue;
        epoint2.line = epoint->line;
        epoint2.pos = interstring_position(epoint, name->data, (size_t)(pos - name->data));
        err_msg2(ERROR__RESERVED_CHR, name, &epoint2);
        return false;
    }
    if (name->data[0] == '/') {
        err_msg2(ERROR_ABSOLUTE_PATH, name, epoint);
        return false;
    }
#endif
    return true;
}

#ifdef _WIN32
static wchar_t *get_real_name(const wchar_t *name) {
    typedef DWORD _stdcall (*Getfinalpathnamebyhandleptr)(HANDLE, LPWSTR, DWORD, DWORD);
    static Getfinalpathnamebyhandleptr get_final_path_by_handle;
    static HINSTANCE kernel_handle;
    DWORD ret;
    size_t len = wcslen(name) + 1;
    wchar_t *real_name = (wchar_t *)malloc(len * sizeof *real_name);
    if (real_name == NULL) return NULL;
    if (get_final_path_by_handle == NULL && kernel_handle == NULL) {
        kernel_handle = LoadLibrary("kernel32.dll");
        if (kernel_handle != NULL) get_final_path_by_handle = (Getfinalpathnamebyhandleptr)GetProcAddress(kernel_handle, "GetFinalPathNameByHandleW");
    } 
    if (get_final_path_by_handle != NULL) {
        HANDLE handle = CreateFileW(name, 0, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL); 
        if (handle != INVALID_HANDLE_VALUE) {
            ret = get_final_path_by_handle(handle, real_name, len, 12);
            if (ret > len) {
                wchar_t *tmp = (wchar_t *)realloc(real_name, ret * sizeof *real_name);
                if (tmp != NULL) {
                    real_name = tmp;
                    len = ret;
                    ret = get_final_path_by_handle(handle, real_name, len, 12);
                }
            }
            CloseHandle(handle);
            if (ret < len && ret != 0) {
                return real_name;
            }
        }
    }
    ret = GetLongPathNameW(name, real_name, len);
    if (ret > len) {
        wchar_t *tmp = (wchar_t *)realloc(real_name, ret * sizeof *real_name);
        if (tmp != NULL) {
            real_name = tmp;
            len = ret;
            ret = GetLongPathNameW(name, real_name, len);
        }
    }
    if (ret < len && ret != 0) {
        return real_name;
    }
    free(real_name);
    return NULL;
}

static bool portability2(const str_t *name, const char *realname, linepos_t epoint) {
    wchar_t *wname = convert_name(realname, SIZE_MAX);
    if (wname != NULL) {
        bool different = false;
        wchar_t *wname2 = get_real_name(wname);
        if (wname2 != NULL) {
            wchar_t *bname = convert_name((const char *)name->data, name->len);
            if (bname != NULL) {
                size_t len = wcslen(wname);
                size_t len2 = wcslen(bname);
                size_t ret = wcslen(wname2);
                free(bname);
                if (len2 > len) len2 = len;
                if (len2 > ret) len2 = ret;
                for (; len2 > 0; len2--) {
                    if (wname[len - len2] != wname2[ret - len2] && wname2[ret - len2] != '\\') {
                        different = true;
                        break;
                    }
                }
            }
            free(wname2);
        }
        free(wname);
        if (different) {
            err_msg2(ERROR___INSENSITIVE, name, epoint);
            return false;
        }
    }
    return portability(name, epoint);
}
#endif

FILE *file_open(const char *name, const char *mode) {
    FILE *f;
#ifdef _WIN32
    wchar_t *wname, *c2, wmode[3];
    const uint8_t *c;
    wname = convert_name(name, SIZE_MAX);
    if (wname == NULL) {
        errno = ENOMEM;
        return NULL;
    }
    c2 = wmode; c = (uint8_t *)mode;
    while ((*c2++=(wchar_t)*c++) != 0);
    f = _wfopen(wname, wmode);
    free(wname);
#else
    size_t len = 0, max = strlen(name) + 1;
    char *newname = (char *)malloc(max);
    const uint8_t *c = (const uint8_t *)name;
    uchar_t ch;
    mbstate_t ps;
    errno = ENOMEM;
    f = NULL;
    if (newname == NULL || max < 1) goto failed;
    memset(&ps, 0, sizeof ps);
    do {
        char temp[64];
        ssize_t l;
        ch = *c;
        if ((ch & 0x80) != 0) {
            c += utf8in(c, &ch);
            if (ch == 0) {errno = ENOENT; goto failed;}
        } else c++;
        l = (ssize_t)wcrtomb(temp, (wchar_t)ch, &ps);
        if (l <= 0) goto failed;
        len += (size_t)l;
        if (len < (size_t)l) goto failed;
        if (len > max) {
            char *d;
            max = len + 64;
            if (max < 64) goto failed;
            d = (char *)realloc(newname, max);
            if (d == NULL) goto failed;
            newname = d;
        }
        memcpy(newname + len - l, temp, (size_t)l);
    } while (ch != 0);
    errno = 0;
    f = fopen(newname, mode);
    if (f == NULL && errno == 0) errno = (mode[0] == 'r') ? ENOENT : EINVAL;
failed:
    free(newname);
#endif
    return f;
}

static void file_free(struct file_s *a)
{
    free(a->data);
    free(a->line);
    free(a->nomacro);
    if (a->name != a->realname) free((char *)a->name);
    free((char *)a->realname);
    free(a);
}

static bool extendfile(struct file_s *tmp) {
    uint8_t *d;
    filesize_t len2 = tmp->len + 4096;
    if (len2 < 4096) return true; /* overflow */
    d = (uint8_t *)realloc(tmp->data, len2);
    if (d == NULL) return true;
    tmp->data = d;
    tmp->len = len2;
    return false;
}

static bool flush_ubuff(struct ubuff_s *ubuff, filesize_t *p2, struct file_s *tmp) {
    uint32_t i;
    filesize_t p = *p2;
    for (i = 0; i < ubuff->p; i++) {
        uchar_t ch;
        if (p + 6*6 + 1 > tmp->len && extendfile(tmp)) return true;
        ch = ubuff->data[i];
        if (ch != 0 && ch < 0x80) tmp->data[p++] = (uint8_t)ch; else p += utf8out(ch, tmp->data + p);
    }
    *p2 = p;
    return false;
}

static uchar_t fromiso2(uchar_t c) {
    mbstate_t ps;
    wchar_t w;
    int olderrno;
    ssize_t l;
    uint8_t c2 = (uint8_t)(c | 0x80);

    memset(&ps, 0, sizeof ps);
    olderrno = errno;
    l = (ssize_t)mbrtowc(&w, (char *)&c2, 1,  &ps);
    errno = olderrno;
    if (l < 0) return c2;
    return (uchar_t)w;
}

static inline uchar_t fromiso(uchar_t c) {
    static uchar_t conv[128];
    c &= 0x7f;
    if (conv[c] == 0) conv[c] = fromiso2(c);
    return conv[c];
}

static filesize_t fsize(FILE *f) {
#if defined _POSIX_C_SOURCE || defined __unix__ || defined __MINGW32__
    struct stat st;
    if (fstat(fileno(f), &st) == 0) {
        if (S_ISREG(st.st_mode) && st.st_size > 0) {
            return (st.st_size & ~(off_t)~(filesize_t)0) == 0 ? (filesize_t)st.st_size : ~(filesize_t)0;
        }
    }
#else
    if (fseek(f, 0, SEEK_END) == 0) {
        long len = ftell(f);
        rewind(f);
        if (len > 0) {
            return (unsigned long)len < ~(filesize_t)0 ? (filesize_t)len : ~(filesize_t)0;
        }
    }
#endif
    return 0;
}

static struct file_s *command_line = NULL;
static struct file_s *lastfi = NULL;
static struct ubuff_s last_ubuff;
static uint16_t curfnum;
struct file_s *openfile(const char *name, const char *base, unsigned int ftype, const str_t *val, linepos_t epoint) {
    struct file_s *tmp;
    char *s;
    if (lastfi == NULL) {
        lastfi = (struct file_s *)mallocx(sizeof *lastfi);
    }
    lastfi->base.data = (const uint8_t *)base;
    lastfi->base.len = get_base(base);
    lastfi->type = ftype;
    if (name != NULL) {
        str_t n;
        lastfi->name = name;
        n.data = (const uint8_t *)name;
        n.len = strlen(name);
        lastfi->hash = ((unsigned int)str_hash(&n) + (unsigned int)str_hash(&lastfi->base) + lastfi->type) & ((~0U) >> 1);
        tmp = file_table_update(lastfi);
    } else {
        tmp = (command_line != NULL) ? command_line : NULL;
        if (command_line == NULL) command_line = lastfi;
    }
    if (tmp == NULL) { /* new file */
        Encoding_types encoding = E_UNKNOWN;
        FILE *f;
        uchar_t c = 0;
        filesize_t fp = 0;

        lastfi->nomacro = NULL;
        lastfi->line = NULL;
        lastfi->lines = 0;
        lastfi->data = NULL;
        lastfi->len = 0;
        lastfi->open = 0;
        lastfi->err_no = 0;
        lastfi->read_error = false;
        lastfi->portable = false;
        lastfi->pass = 0;
        lastfi->entercount = 0;
        tmp = lastfi;
        lastfi = NULL;
        if (name != NULL) {
            int err = 1;
            char *path = NULL;
            size_t namelen = strlen(name) + 1;
            s = (char *)mallocx(namelen);
            memcpy(s, name, namelen); tmp->name = s;
            if (val != NULL) {
                struct include_list_s *i = include_list.next;
                f = file_open(name, "rb");
                while (f == NULL && i != NULL) {
                    free(path);
                    path = get_path(val, i->path);
                    f = file_open(path, "rb");
                    i = i->next;
                }
            } else {
                f = dash_name(name) ? stdin : file_open(name, "rb");
            }
            if (path == NULL) path = s;
            tmp->realname = path;
            if (arguments.quiet) {
                fputs((ftype == 1) ? "Reading file:      " : "Assembling file:   ", stdout);
                argv_print(path, stdout);
                putchar('\n');
                fflush(stdout);
            }
            if (f == NULL) goto openerr;
            tmp->read_error = true;
            if (ftype == 1) {
                bool check;
                filesize_t fs = fsize(f);
                if (fs > 0) {
                    tmp->data = (uint8_t *)malloc(fs);
                    if (tmp->data != NULL) tmp->len = fs;
                }
                check = (tmp->data != NULL);
                clearerr(f); errno = 0;
                if (tmp->len != 0 || !extendfile(tmp)) {
                    for (;;) {
                        fp += (filesize_t)fread(tmp->data + fp, 1, tmp->len - fp, f);
                        if (feof(f) == 0 && fp >= tmp->len) {
                            if (check) {
                                int c2 = getc(f);
                                check = false;
                                if (c2 != EOF) {
                                    if (extendfile(tmp)) break;
                                    tmp->data[fp++] = (uint8_t)c2;
                                    continue;
                                }
                            } else {
                                if (extendfile(tmp)) break;
                                continue;
                            }
                        }
                        err = 0;
                        break;
                    }
                }
            } else {
                struct ubuff_s ubuff = last_ubuff;
                size_t max_lines = 0;
                linenum_t lines = 0;
                uint8_t buffer[BUFSIZ * 2];
                size_t bp = 0, bl;
                unsigned int qr = 1;
                filesize_t fs = fsize(f);
                if (fs > 0) {
                    filesize_t len2 = fs + 4096;
                    if (len2 < 4096) len2 = ~(filesize_t)0; /* overflow */
                    tmp->data = (uint8_t *)malloc(len2);
                    if (tmp->data != NULL) tmp->len = len2;
                    max_lines = (len2 / 20 + 1024) & ~(size_t)1023;
                    if (max_lines > SIZE_MAX / sizeof *tmp->line) max_lines = SIZE_MAX / sizeof *tmp->line; /* overflow */
                    tmp->line = (filesize_t *)malloc(max_lines * sizeof *tmp->line);
                    if (tmp->line == NULL) max_lines = 0;
                }
                clearerr(f); errno = 0;
                bl = fread(buffer, 1, BUFSIZ, f);
                if (bl != 0 && buffer[0] == 0) encoding = E_UTF16BE; /* most likely */
#ifdef _WIN32
                setlocale(LC_CTYPE, "");
#endif
                ubuff.p = 0;
                do {
                    filesize_t p;
                    uchar_t lastchar;
                    bool qc = true;
                    uint8_t cclass = 0;

                    if (lines >= max_lines) {
                        filesize_t *d;
                        size_t len2 = max_lines + 1024;
                        if (/*len2 < 1024 ||*/ len2 > SIZE_MAX / sizeof *tmp->line) goto failed; /* overflow */
                        d = (filesize_t *)realloc(tmp->line, len2 * sizeof *tmp->line);
                        if (d == NULL) goto failed;
                        tmp->line = d;
                        max_lines = len2;
                    }
                    tmp->line[lines++] = fp;
                    if (lines == 0) { lines = ~(linenum_t)0; goto failed; } /* overflow */
                    p = fp;
                    for (;;) {
                        unsigned int i, j;
                        uint8_t ch2;
                        if (p + 6*6 + 1 > tmp->len && extendfile(tmp)) goto failed;
                        if (bp / (BUFSIZ / 2) == qr) {
                            if (qr == 1) {
                                qr = 3;
                                if (feof(f) == 0) bl = BUFSIZ + fread(buffer + BUFSIZ, 1, BUFSIZ, f);
                            } else {
                                qr = 1;
                                if (feof(f) == 0) bl = fread(buffer, 1, BUFSIZ, f);
                            }
                        }
                        if (bp == bl) break;
                        lastchar = c;
                        c = buffer[bp]; bp = (bp + 1) % (BUFSIZ * 2);
                        if (!arguments.to_ascii) {
                            if (c == 10) {
                                if (lastchar == 13) continue;
                                break;
                            }
                            if (c == 13) {
                                break;
                            }
                            if (c != 0 && c < 0x80) tmp->data[p++] = (uint8_t)c; else p += utf8out(c, tmp->data + p);
                            continue;
                        }
                        switch (encoding) {
                        case E_UNKNOWN:
                        case E_UTF8:
                            if (c < 0x80) goto done;
                            if (c < 0xc0) {
                            invalid:
                                if (encoding == E_UNKNOWN) {
                                    c = fromiso(c);
                                    encoding = E_ISO; break;
                                }
                                c = REPLACEMENT_CHARACTER; break;
                            }
                            ch2 = (bp == bl) ? 0 : buffer[bp];
                            if (c < 0xe0) {
                                if (c < 0xc2) goto invalid;
                                c ^= 0xc0; i = 1;
                            } else if (c < 0xf0) {
                                if ((c ^ 0xe0) == 0 && (ch2 ^ 0xa0) >= 0x20) goto invalid;
                                c ^= 0xe0; i = 2;
                            } else if (c < 0xf8) {
                                if ((c ^ 0xf0) == 0 && (uint8_t)(ch2 - 0x90) >= 0x30) goto invalid;
                                c ^= 0xf0; i = 3;
                            } else if (c < 0xfc) {
                                if ((c ^ 0xf8) == 0 && (uint8_t)(ch2 - 0x88) >= 0x38) goto invalid;
                                c ^= 0xf8; i = 4;
                            } else if (c < 0xfe) {
                                if ((c ^ 0xfc) == 0 && (uint8_t)(ch2 - 0x84) >= 0x3c) goto invalid;
                                c ^= 0xfc; i = 5;
                            } else {
                                if (encoding != E_UNKNOWN) goto invalid;
                                if (c == 0xff && ch2 == 0xfe) encoding = E_UTF16LE;
                                else if (c == 0xfe && ch2 == 0xff) encoding = E_UTF16BE;
                                else goto invalid;
                                bp = (bp + 1) % (BUFSIZ * 2);
                                continue;
                            }

                            for (j = i; i != 0; i--) {
                                if (bp != bl) {
                                    ch2 = buffer[bp];
                                    if ((ch2 ^ 0x80) < 0x40) {
                                        c = (c << 6) ^ ch2 ^ 0x80;
                                        bp = (bp + 1) % (BUFSIZ * 2);
                                        continue;
                                    }
                                }
                                if (encoding != E_UNKNOWN) {
                                    c = REPLACEMENT_CHARACTER;break;
                                }
                                encoding = E_ISO;
                                i = (j - i) * 6;
                                qc = false;
                                if (ubuff.p >= ubuff.len && extend_ubuff(&ubuff)) goto failed;
                                ubuff.data[ubuff.p++] = fromiso(((~0x7f >> j) & 0xff) | (c >> i));
                                for (;i != 0; i-= 6) {
                                    if (ubuff.p >= ubuff.len && extend_ubuff(&ubuff)) goto failed;
                                    ubuff.data[ubuff.p++] = fromiso(((c >> (i-6)) & 0x3f) | 0x80);
                                }
                                if (bp == bl) goto eof;
                                c = (ch2 >= 0x80) ? fromiso(ch2) : ch2;
                                j = 0;
                                bp = (bp + 1) % (BUFSIZ * 2);
                                break;
                            }
                            if (j != 0) encoding = E_UTF8;
                            break;
                        case E_UTF16LE:
                            if (bp == bl) goto invalid;
                            c |= (uchar_t)buffer[bp] << 8; bp = (bp + 1) % (BUFSIZ * 2);
                            if (c == 0xfffe) {
                                encoding = E_UTF16BE;
                                continue;
                            }
                            break;
                        case E_UTF16BE:
                            if (bp == bl) goto invalid;
                            c = (c << 8) | buffer[bp]; bp = (bp + 1) % (BUFSIZ * 2);
                            if (c == 0xfffe) {
                                encoding = E_UTF16LE;
                                continue;
                            }
                            break;
                        case E_ISO:
                            if (c >= 0x80) c = fromiso(c);
                            goto done;
                        }
                        if (c == 0xfeff) continue;
                        if (encoding != E_UTF8) {
                            if (c >= 0xd800 && c < 0xdc00) {
                                if (lastchar < 0xd800 || lastchar >= 0xdc00) continue;
                                c = REPLACEMENT_CHARACTER;
                            } else if (c >= 0xdc00 && c < 0xe000) {
                                if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                    c ^= 0x360dc00 ^ (lastchar << 10);
                                    c += 0x10000;
                                } else c = REPLACEMENT_CHARACTER;
                            } else if (lastchar >= 0xd800 && lastchar < 0xdc00) {
                                c = REPLACEMENT_CHARACTER;
                            }
                        }
                    done:
                        if (c < 0xc0) {
                            if (c == 10) {
                                if (lastchar == 13) continue;
                                break;
                            }
                            if (c == 13) {
                                break;
                            }
                            cclass = 0;
                            if (!qc) {
                                if (unfc(&ubuff)) goto failed;
                                qc = true;
                            }
                            if (ubuff.p == 1) {
                                if (ubuff.data[0] != 0 && ubuff.data[0] < 0x80) tmp->data[p++] = (uint8_t)ubuff.data[0]; else p += utf8out(ubuff.data[0], tmp->data + p);
                            } else {
                                if (ubuff.p != 0 && flush_ubuff(&ubuff, &p, tmp)) goto failed;
                                ubuff.p = 1;
                            }
                            ubuff.data[0] = c;
                        } else {
                            const struct properties_s *prop = uget_property(c);
                            uint8_t ncclass = prop->combclass;
                            if ((ncclass != 0 && cclass > ncclass) || (prop->property & (qc_N | qc_M)) != 0) {
                                qc = false;
                                if (ubuff.p >= ubuff.len && extend_ubuff(&ubuff)) goto failed;
                                ubuff.data[ubuff.p++] = c;
                            } else {
                                if (!qc) {
                                    if (unfc(&ubuff)) goto failed;
                                    qc = true;
                                }
                                if (ubuff.p == 1) {
                                    if (ubuff.data[0] != 0 && ubuff.data[0] < 0x80) tmp->data[p++] = (uint8_t)ubuff.data[0]; else p += utf8out(ubuff.data[0], tmp->data + p);
                                } else {
                                    if (ubuff.p != 0 && flush_ubuff(&ubuff, &p, tmp)) goto failed;
                                    ubuff.p = 1;
                                }
                                ubuff.data[0] = c;
                            }
                            cclass = ncclass;
                        }
                    }
                eof:
                    if (!qc && unfc(&ubuff)) goto failed;
                    if (ubuff.p == 1) {
                        if (ubuff.data[0] != 0 && ubuff.data[0] < 0x80) tmp->data[p++] = (uint8_t)ubuff.data[0]; else p += utf8out(ubuff.data[0], tmp->data + p);
                    } else {
                        if (ubuff.p != 0 && flush_ubuff(&ubuff, &p, tmp)) goto failed;
                    }
                    ubuff.p = 0;
                    while (p > fp && (tmp->data[p - 1] == ' ' || tmp->data[p - 1] == '\t')) p--;
                    if (fp == 0 && p > 1 && tmp->data[0] == '#' && tmp->data[1] == '!') p = 0;
                    tmp->data[p++] = 0;
                    fp = p;
                } while (bp != bl);
                err = 0;
            failed:
#ifdef _WIN32
                setlocale(LC_CTYPE, "C");
#endif
                last_ubuff = ubuff;
                tmp->lines = lines;
                if (lines != max_lines) {
                    filesize_t *d = (filesize_t *)realloc(tmp->line, lines * sizeof *tmp->line);
                    if (lines == 0 || d != NULL) tmp->line = d;
                }
            }
            if (fp == 0) {
                free(tmp->data);
                tmp->data = NULL;
            } else if (tmp->len != fp) {
                uint8_t *d = (uint8_t *)realloc(tmp->data, fp);
                if (d != NULL) tmp->data = d;
            }
            tmp->len = fp;
            if (err != 0) errno = ENOMEM;
            err |= ferror(f);
            if (f != stdin) err |= fclose(f);
        openerr:
            if (err != 0 && errno != 0) {
                tmp->err_no = errno;
                free(tmp->data);
                tmp->data = NULL;
                tmp->len = 0;
                free(tmp->line);
                tmp->line = NULL;
                tmp->lines = 0;
            }
        } else {
            const char *cmd_name = "<command line>";
            size_t cmdlen = strlen(cmd_name) + 1;
            s = (char *)mallocx(1);
            s[0] = 0; tmp->name = s;
            s = (char *)mallocx(cmdlen);
            memcpy(s, cmd_name, cmdlen); tmp->realname = s;
        }

        tmp->uid = (ftype != 1) ? curfnum++ : 0;
        tmp->encoding = encoding;
    }
    if (tmp->err_no != 0) {
        if (tmp->pass != pass) {
            char *path = (val != NULL) ? get_path(val, "") : NULL;
            errno = tmp->err_no;
            err_msg_file(tmp->read_error ? ERROR__READING_FILE : ERROR_CANT_FINDFILE, (val != NULL) ? path : name, epoint);
            free(path);
            tmp->pass = pass;
        }
        return NULL;
    }
    if (!tmp->portable && val != NULL && diagnostics.portable) {
#ifdef _WIN32
        tmp->portable = portability2(val, tmp->realname, epoint);
#else
        tmp->portable = portability(val, epoint);
#endif
    }
    tmp->open++;
    return tmp;
}

void closefile(struct file_s *f) {
    if (f->open != 0) f->open--;
}

struct starnode_s {
    struct star_s star;
    struct avltree tree;
    struct avltree_node node;
};

static FAST_CALL int star_compare(const struct avltree_node *aa, const struct avltree_node *bb)
{
    linenum_t a = cavltree_container_of(aa, struct starnode_s, node)->star.line;
    linenum_t b = cavltree_container_of(bb, struct starnode_s, node)->star.line;
    return (a > b) - (a < b);
}

static struct stars_s {
    struct starnode_s stars[255];
    struct stars_s *next;
} *stars = NULL;

static struct starnode_s *lastst;
static int starsp;
struct star_s *new_star(linenum_t line, bool *exists) {
    struct avltree_node *b;
    struct starnode_s *tmp;
    lastst->star.line = line;
    b = avltree_insert(&lastst->node, &((struct starnode_s *)star_tree)->tree, star_compare);
    if (b == NULL) { /* new label */
        *exists = false;
        avltree_init(&lastst->tree);
        if (starsp == 254) {
            struct stars_s *old = stars;
            stars = (struct stars_s *)mallocx(sizeof *stars);
            stars->next = old;
            starsp = 0;
        } else starsp++;
        tmp = lastst;
        lastst = &stars->stars[starsp];
        tmp->star.pass = 0;
        tmp->star.vline = 0;
        return &tmp->star;
    }
    *exists = true;
    tmp = avltree_container_of(b, struct starnode_s, node);
    if (tmp->star.pass != pass) {
        tmp->star.pass = pass;
        tmp->star.vline = 0;
    }
    return &tmp->star;            /* already exists */
}

static struct starnode_s star_root;

struct star_s *init_star(linenum_t i) {
    bool starexists;
    struct star_s *s;
    star_tree = &star_root.star;
    s = new_star(i, &starexists);
    s->addr = 0;
    return s;
}

void destroy_file(void) {
    struct stars_s *old;

    if (file_table.data != NULL) {
        size_t i;
        for (i = 0; i <= file_table.mask; i++) {
            struct file_s *p = file_table.data[i];
            if (p != NULL) file_free(p);
        }
        free(file_table.data);
    }
    free(lastfi);
    free(last_ubuff.data);
    if (command_line != NULL) file_free(command_line);

    include_list_last = include_list.next;
    while (include_list_last != NULL) {
        struct include_list_s *tmp = include_list_last;
        include_list_last = tmp->next;
        free(tmp);
    }

    while (stars != NULL) {
        old = stars;
        stars = stars->next;
        free(old);
    }
}

void init_file(void) {
    curfnum = 1;
    file_table.len = 0;
    file_table.mask = 0;
    file_table.data = NULL;
    stars = (struct stars_s *)mallocx(sizeof *stars);
    stars->next = NULL;
    starsp = 0;
    lastst = &stars->stars[starsp];
    last_ubuff.data = (uchar_t *)mallocx(16 * sizeof *last_ubuff.data);
    last_ubuff.len = 16;
    avltree_init(&star_root.tree);
}

static size_t wrap_print(const char *txt, FILE *f, size_t len) {
    if (len != 0) {
        if (len > 64) {
            fputs(" \\\n", f);
            len = 0;
        }
        len++;
        putc(' ', f);
    }
    return len + makefile_print(txt, f);
}

void makefile(int argc, char *argv[], bool make_phony) {
    FILE *f;
    struct linepos_s nopoint = {0, 0};
    size_t len = 0, j;
    int i, err;

    f = dash_name(arguments.make) ? stdout : file_open(arguments.make, "wt");
    if (f == NULL) {
        err_msg_file(ERROR_CANT_WRTE_MAK, arguments.make, &nopoint);
        return;
    }
    clearerr(f); errno = 0;
    for (j = 0; j < arguments.output_len; j++) {
        const struct output_s *output = &arguments.output[j];
        if (dash_name(output->name)) continue;
        len = wrap_print(output->name, f, len);
    }
    if (arguments.list.name != NULL) {
        if (!dash_name(arguments.list.name)) {
            len = wrap_print(arguments.list.name, f, len);
        }
    }
    for (j = 0; j < arguments.symbol_output_len; j++) {
        const struct symbol_output_s *output = &arguments.symbol_output[j];
        if (dash_name(output->name)) continue;
        len = wrap_print(output->name, f, len);
    }
    if (len != 0) {
        len++;
        putc(':', f);

        for (i = 0; i < argc; i++) {
            if (dash_name(argv[i])) continue;
            len = wrap_print(argv[i], f, len);
        }

        if (file_table.data != NULL) {
            for (j = 0; j <= file_table.mask; j++) {
                const struct file_s *a = file_table.data[j];
                if (a == NULL) continue;
                if (a->type == 0) continue;
                len = wrap_print(a->realname, f, len);
            }
        }
        putc('\n', f);

        if (file_table.data != NULL && make_phony) {
            len = 0;
            for (j = 0; j <= file_table.mask; j++) {
                const struct file_s *a = file_table.data[j];
                if (a == NULL) continue;
                if (a->type == 0) continue;
                len = wrap_print(a->realname, f, len);
            }
            if (len != 0) fputs(":\n", f);
        }
    }

    err = ferror(f);
    err |= (f != stdout) ? fclose(f) : fflush(f);
    if (err != 0 && errno != 0) err_msg_file(ERROR_CANT_WRTE_MAK, arguments.make, &nopoint);
}
